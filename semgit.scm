

(define-module (semgit)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:export ())

;; semcommit := subject body [<semtag> ...]
(define-immutable-record-type <semcommit>
  (semcommit subject body)
  semcommit?
  (subject semcommit-subject)
  (body semcommit-body)
  (semtags semcommit-semtags set-semcommit-semtags))

;; semtag := 'name value
(define-record-type <semtag>
  (semtag name value)
  semtag?
  (name semtag-name)
  (value semtag-value))

;;;; Poor man's git commit message importer

(define %file ".tmp-commit-msg")

(define* (dump-commit-message #:optional (hash "") (file %file))
  (let ((cmd (string-join `("git show --pretty=%B -q" ,hash ">" ,file)
                          " ")))
    (system cmd)))

(define* (fetch-commit-message #:optional (file %file))
  (with-input-from-file file
    (lambda ()
      (semcommit (read-line) (read-delimited ";")))))

;;;; Git tools

(define* (git-amend-commit semgit #:optional (hash ""))
  (system* "git" "commit" "--amend" "-m" (semcommit->string semgit) hash))

(define* (git-augment-commit semtags #:optional (hash "") (file %file))
  (dump-commit-message hash file)
  (git-amend-commit 
   (set-semcommit-semtags (fetch-commit-message file)
                          (match semtags
                            ((semtags ...) semtags)
                            (semtag (list semtag))))
   hash))

;;;; semcommit tools

(define (enhance-commit-message semcommit semtags)
  (set-semcommit-semtags semcommit semtags))

(define (semcommit->string semcommit)
  (match semcommit
    (($ <semcommit> subject body (semtags ...))
     (string-join `(,subject ,body "---semgit---"
                             ,@(map semtag->string semtags))
                  "\n"))))

(define (semtag->string semtag)
  (match semtag
    (($ <semtag> name value)
     (string-append (symbol->string name) ": " value))))

(git-augment-commit (semtag 'feature "semtags"))

