;;; Semgit --- Semantic git commit messages
;;; Copyright © 2016 Alex Sassmannshausen <alex.sassmannshausen@ptfs-europe.com>
;;;
;;; This file is part of Semgit.
;;;
;;; Semgit is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Semgit is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Semgit.  If not, see <http://www.gnu.org/licenses/>.

;;;; Commentary
;;
;; We will provide the following commandline:
;; - git-sem tag -t     :: Defaults to tagging current branch commits
;;                         with topic: branchname
;; - git-sem tag --topic=name $RANGE :: Tag range with topic: name
;; - git-sem tag --tags=$k:$v,$k1:$v1,... $RANGE
;; - git-sem rm -t      :: Remove topic tags from current branch
;; - git-sem rm --tags=$k,$k1,$k2 $RANGE  :: Remove tags $k, $k1
;; - git-sem log --topic=name [--tag=name]
;;
;; Ranges or exact specifications are not yet implemented.  For now
;; all operations are branch operations.
;;
;; `log', i.e. reporting, is not yet implemented.
;;
;;;; Code

(define-module (semgit)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (semgit conf)
  #:use-module (semgit conf monads)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 binary-ports)
  #:export (run-system

            <semtag>
            semtag semtag?
            semtag-name semtag-value

            current-branch
            in-git-repo?

            semgit-tidy
            branch-add-tags
            branch-rm-tags
            ))


;;;; System Monad

;; A simple monad that perform exit status checking after running
;; system commands: on any other result than 0, throw an error to
;; 'semgit-system.
;; 
;; Running the monad will return 0 otherwise.

;; FIXME: Works as expected.  However the very first operation in an
;; mbegin sequence should not be side-effectful: it will be performed
;; immediately rather than delayed.  This is due to (monads) not being
;; implemented lazily.

;;;;; Bind Op
;;;
;;; Resolve the monadic value

(define-inlinable (system-bind mvalue mproc)
  (lambda ()
    (delay 
      (match (force (mvalue))
        (0 (force ((mproc 0))))
        (else (throw 'semgit-system "System call returned with: " else))))))

;;;;; The return op
;;;
;;; Lift value into monadic context

(define-inlinable (system-return value)
  (lambda ()
    (delay value)))

;;; The monad.
(define-monad %system-monad
  (bind system-bind)
  (return system-return))

;;; The runner
(define (run-system mvalue)
  "Run MVALUE a monadic value in the system monad.  Return 0 if all
went well; throw an error to semgit-system otherwise."
  (match (force (mvalue))
    (0 0)
    (else (throw 'semgit-system "System call returned with: " else))))


;;;; Variables

;; semgit magic marker
(define %semgit "===SEMGIT===")
;; semgit temporary branch
(define %semgit-tmpbranch "semgit-tmp")

(define (semgit-bkpbranch branchname)
  "Return the name of a backup branch for BRANCHNAME."
  (string-append "semgit-" branchname "-bkp"))

;;;; Types

;; semcommit := subject body [<semtag> ...]
(define-immutable-record-type <semcommit>
  (semcommit subject body semtags)
  semcommit?
  (subject semcommit-subject)
  (body semcommit-body)
  (semtags semcommit-semtags set-semcommit-semtags))

;; semtag := name value
(define-record-type <semtag>
  (semtag name value)
  semtag?
  (name semtag-name)
  (value semtag-value))


;;;; Git Readers

(define* (fetch-commit-message #:optional (hash ""))
  "Return a <semcommit> containing the commit message of the git
commit identified by HASH or at HEAD."
  (let ((cmd (string-join `("git show --pretty=%B -q" ,hash)
                          " ")))
  (let* ((pipe (open-pipe* OPEN_READ "git" "show" "--pretty=%B" "-q"))
         (output (begin
                   (let ((bv (get-bytevector-all pipe)))
                     (if (eof-object? bv)
                         ""
                         (utf8->string bv)))))
         (ret (close-pipe pipe)))
    (case (status:exit-val ret)
      ((0) (string->semcommit output))
      (else (throw 'git-import "Unable to fetch commit message."))))))

(define* (find-branch-root #:optional (branchname "master"))
  "Return the hash of the commit at the base of the divergence
BRANCHNAME (defaulting to master) and our current branch .  The base
is the first commit to exist in our current branch and not in
BRANCHNAME."
  (let* ((pipe (open-pipe* OPEN_READ "git" "log" "--format=%H"
                           (string-append branchname "..")))
         (output (begin
                   (let ((bv (get-bytevector-all pipe)))
                     (if (eof-object? bv)
                         ""
                         (utf8->string bv)))))
         (ret (close-pipe pipe)))
    (case (status:exit-val ret)
      ((0) (match (filter (negate string-null?)
                          (string-split output #\newline))
             (() #f)
             (divergence (last divergence))))
      (else (throw 'git-import "Unable to fetch commit message.")))))

(define* (find-branch-differences branchname)
  "Return a list of hashes of all commits present in our current
branch and not in BRANCHNAME, ordered oldest to newest."
  (let* ((pipe (open-pipe* OPEN_READ "git" "log" "--format=%H"
                           (string-append ".." branchname)))
         (output (begin
                   (let ((bv (get-bytevector-all pipe)))
                     (if (eof-object? bv)
                         ""
                         (utf8->string bv)))))
         (ret (close-pipe pipe)))
    (case (status:exit-val ret)
      ((0) (reverse (filter (negate string-null?)
                            (string-split output #\newline))))
      (else (throw 'git-import "Unable to fetch commit message.")))))

(define (current-branch)
  "Return the name of the branch we are currently on."
  (last (string-split (with-input-from-file ".git/HEAD"
                        (lambda () (read-line)))
                      #\/)))

(define* (fetch-semgit-branches)
  "Return a list of branch names currently in existence for this
project."
  (let ((branch-dir (string-join '(".git" "logs" "refs" "heads")
                                 file-name-separator-string)))
    (file-system-fold (lambda (name stat result)
                        (string=? name branch-dir))
                      (lambda (name stat result)
                        (let ((name (basename name)))
                          (if (or (string-match "semgit-.*-bkp" name)
                                  (string=? %semgit-tmpbranch name)) 
                              (cons name result)
                              result)))
                      (lambda (name stat result) result)
                      (lambda (name stat result) result)
                      (lambda (name stat result) result)
                      (lambda (error path stat errno result) result)
                      '()
                      branch-dir)))

(define (in-git-repo?)
  (file-exists? ".git"))


;;;; Semtag Manipulators

(define (add-semtags semcommit . semtags)
  "Add the list of <semtag> records SEMTAGS to the <semcommit>
SEMCOMMIT.  Duplicate tags can be added."
  (if (null? semtags)
      semcommit
      (set-semcommit-semtags semcommit
                             (match (semcommit-semtags semcommit)
                               (() semtags)
                               ((tags ...) (append tags semtags))))))

(define (rm-semtags semcommit . keys)
  "Remove the <semtag>s identified by KEYS from the <semcommit>
SEMCOMMIT.  Keys that match no <semtag> are ignored."
  (let ((semtags (semcommit-semtags semcommit)))
    (set-semcommit-semtags semcommit
                           (if (any null? (list semtags keys))
                               '()
                               (let lp ((keys keys)
                                        (remaining semtags))
                                 (match keys
                                   (() remaining)
                                   ((first . rest)
                                    (lp rest
                                        (filter
                                         (compose
                                          (cut (negate string=?) first <>)
                                          semtag-name) 
                                         remaining)))))))))


;;;; Serialization

(define (string->semcommit msg)
  "Return a <semcommit> represented by the string MSG."
  (semcommit (subject msg) (body msg) (semtags msg)))

(define (semcommit->string semcommit)
  "Return a string representation of <semcommit> SEMCOMMIT."
  (match semcommit
    (($ <semcommit> subject body semtags)
     (string-join (cons* subject body
                         (if (null? semtags)
                             '()
                             (list (string-join
                                    (cons %semgit
                                          (map semtag->string semtags))
                                    "\n" 'suffix))))
                  "\n\n"))))

(define (semtag->string semtag)
  "Return a string representation of <semtag> SEMTAG."
  (match semtag
    (($ <semtag> name value)
     (string-append name ": " value))))

(define (subject msg)
  "Return just the subject part of the string MSG treated as a git
commit message."
  (match:substring (regexp-exec (make-regexp "^.*$" regexp/newline) msg)))

(define (body msg)
  "Return just the body part of the string MSG treated as a git commit
message."
  (let* ((esab (regexp-exec (make-regexp "\n\n.*$") msg))
         (base (regexp-exec (make-regexp (string-append %semgit ".*$"))
                            (match:substring esab))))
    (string-trim-both
     (if base
         (substring (match:string base) 0
                    (match:start base))
         (match:substring esab)))))

(define (semtags msg)
  "Return a list of <semtags> represented by the final section of
string MSG treated as a git commit message."
  (let  ((tag-base (string-match (string-append %semgit "\n(.*)") msg)))
    (if (and tag-base (> (match:count tag-base) 1))
        (fold (lambda (candidate semtags)
                (match (string-split candidate #\:)
                  ((name value) (cons (semtag name (string-trim-both value))
                                      semtags))
                  (_ semtags)))
              '()
              (string-split (string-trim-right
                             (match:substring tag-base 1))
                            #\newline))
        '())))


;;;; Git tools

(define (git-commit . args)
  (apply system* "git" "commit" args))

(define sgit-commit (lift git-commit %system-monad))

(define* (git-commit-amend semcommit)
  (git-commit "--amend" "-m" (semcommit->string semcommit)))

(define sgit-commit-amend (lift git-commit-amend %system-monad))

(define (git-checkout . args)
  (apply system* "git" "checkout" args))

(define sgit-checkout (lift git-checkout %system-monad))

(define* (git-checkout-tmp #:optional (hash #f))
  "HASH should be the commit we want to checkout in the tmp branch.
If omitted, it will default to HEAD, like on the commandline."
  (if hash
      (git-checkout "-b" %semgit-tmpbranch hash)
      (git-checkout "-b" %semgit-tmpbranch)))

(define sgit-checkout-tmp (lift git-checkout-tmp %system-monad))

(define (git-cherrypick . args)
  (apply system* "git" "cherry-pick" args))

(define sgit-cherrypick (lift git-cherrypick %system-monad))

(define (git-branch . args)
  (apply system* "git" "branch" args))

(define sgit-branch (lift git-branch %system-monad))

(define (git-log . args)
  (apply system* "git" "log" args))

(define sgit-show (lift git-log %system-monad))

(define (git-show . args)
  (apply system* "git" "show" args))

(define sgit-show (lift git-show %system-monad))


;;;; Monadic Porcelain.

;;;;; Commit operations

(define (commit-rm-tags . keys)
  "Remove all semtags identified by KEYS from the last commit in our
repository.  If KEYS is null, remove all semtags."
  (mlet* %system-monad
      ((new-msg -> (apply rm-semtags (fetch-commit-message) keys)))
    (mbegin %system-monad
      (return 0)
      (sgit-commit-amend new-msg))))

(define (commit-add-tags . semtags)
  "Add SEMTAGS, a list of <semtag> records, to the last commit in our
repository."
  (mlet* %system-monad
      ((new-msg -> (apply add-semtags (fetch-commit-message) semtags)))
    (mbegin %system-monad
      (return 0)
      (sgit-commit-amend new-msg))))

;;;;; Branch operations

(define (branch-rm-tags . keys)
  "Remove all semtags identified by KEYS from the all commits in our
branch, and not in master.  If KEYS is null, remove all semtags."
  (branch-operation rm-semtags keys))

(define (branch-add-tags . semtags)
  "Add SEMTAGS, a list of <semtag> records, to all commits in our
branch and not in master."
  (branch-operation add-semtags semtags))

(define (branch-operation operation values)
  "Higher order helper procedure to perform operations over the
current branch in the repository.  OPERATION should be a two argument
procedure taking a semcommit and a list of datums."
  (mlet* %system-monad
      ((branch -> (current-branch)))
    (match (find-branch-root)
      (#f (format  #t "There are no differences between '~a' and 'master'.
We cannot perform operations in this state.~%" branch)
       (return 0))
      (branch-root
       (mbegin %system-monad
         (return 0)
         (sgit-checkout-tmp branch-root)
         (let lp ((commits (find-branch-differences branch)))
           (match commits
             (()
              (mbegin %system-monad
                (return 0)
                (sgit-commit-amend
                 (apply operation (fetch-commit-message) values))
                (sgit-branch "-m" branch (semgit-bkpbranch branch))
                (sgit-branch "-m" %semgit-tmpbranch branch)))
             ((first . rest)
              (mbegin %system-monad
                (return 0)
                (sgit-commit-amend
                 (apply operation (fetch-commit-message) values))
                (sgit-cherrypick first)
                (lp rest))))))))))

;;;;; Utilities

(define (semgit-tidy checkout)
  "Remove all temporary semgit branches from the repository
(semgit-tmp and any bkp branches found)."
  (mbegin %system-monad
    (return 0)
    (if checkout
        (sgit-checkout checkout)
        (return 0))
    (match (string-match "^semgit-" (current-branch))
      (#f
       (match (fetch-semgit-branches)
         (() (return 0))
         (branches (apply sgit-branch "-D" branches))))
      (_
       (format  #t "You appear to be on a semgit branch.
Please change branch so that we can tidy all semgit branches.~%")
       (return 0)))))
