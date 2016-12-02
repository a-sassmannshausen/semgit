

;;;;; Commentary
;;
;; We will provide the following commandline:
;; - git-sem tag -f     :: Defaults to tagging current branch commits
;;                         with feature: branchname
;; - git-sem tag --feature=name $RANGE :: Tag range with feature: name
;; - git-sem tag --tags=$k:$v,$k1:$v1,... $RANGE
;; - git-sem rm -f      :: Remove feature tags from current branch
;; - git-sem rm --tags=$k,$k1,$k2 $RANGE  :: Remove tags $k, $k1
;; - git-sem log --feature=name [--tag=name]
;;
;;;;; Code

(define-module (semgit)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 popen)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 binary-ports)
  #:export ())

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

;; semgit magic marker
(define %semgit "===SEMGIT===")

;;;; Poor man's git commit message importer

(define* (fetch-commit-message #:optional (hash ""))
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

(define (string->semcommit msg)
  (semcommit (subject msg) (body msg) (semtags msg)))

(define (subject msg)
  (match:substring (regexp-exec (make-regexp "^.*$" regexp/newline) msg)))

(define (body msg)
  (let* ((esab (regexp-exec (make-regexp "\n\n.*$") msg))
         (base (regexp-exec (make-regexp (string-append %semgit ".*$"))
                            (match:substring esab))))
    (string-trim-both
     (if base
         (substring (match:string base) 0
                    (match:start base))
         (match:substring esab)))))

(define (semtags body)
  (let  ((tag-base (string-match (string-append %semgit "\n(.*)") body)))
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

;;;; Git find branch root

(define* (find-branch-root #:optional (branchname "master"))
  (let* ((pipe (open-pipe* OPEN_READ "git" "log" "--format=%H"
                           (string-append branchname "..")))
         (output (begin
                   (let ((bv (get-bytevector-all pipe)))
                     (if (eof-object? bv)
                         ""
                         (utf8->string bv)))))
         (ret (close-pipe pipe)))
    (case (status:exit-val ret)
      ((0) (read-last-line output))
      (else (throw 'git-import "Unable to fetch commit message.")))))

(define* (find-branch-differences branchname)
  (let* ((pipe (open-pipe* OPEN_READ "git" "log" "--format=%H"
                           (string-append branchname "..")))
         (output (begin
                   (let ((bv (get-bytevector-all pipe)))
                     (if (eof-object? bv)
                         ""
                         (utf8->string bv)))))
         (ret (close-pipe pipe)))
    (case (status:exit-val ret)
      ((0) (string-split (string-trim-both output) #\newline))
      (else (throw 'git-import "Unable to fetch commit message.")))))

(define (read-last-line string)
  (last (string-split (string-trim-both string) #\newline)))

(define (current-branch)
  (last (string-split (with-input-from-file ".git/HEAD"
                        (lambda () (read-line)))
                      #\/)))

;;;; Semtag Manipulators

(define (add-semtags semcommit . semtags)
  (if (null? semtags)
      semcommit
      (set-semcommit-semtags semcommit
                             (match (semcommit-semtags semcommit)
                               (() semtags)
                               ((tags ...) (append tags semtags))))))

(define (rm-semtags semcommit . keys)
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

;;;; Git tools

;;;;; Commit Level
(define* (git-amend-commit semgit #:optional (hash ""))
  (system* "git" "commit" "--amend" "-m" (semcommit->string semgit) hash))

(define (git-checkout-tmp-branch hash)
  (system* "git" "checkout" "-b" "tmp" hash))

;;;;; Branch Level

;;;; semcommit tools

(define (semcommit->string semcommit)
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
  (match semtag
    (($ <semtag> name value)
     (string-append name ": " value))))
