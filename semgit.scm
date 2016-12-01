

(define-module (semgit)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export ())

(define-record-type <commit-message>
  (commit-message subject body)
  commit-message?
  (subject commit-message-subject)
  (body commit-message-body))

(define-record-type <semtag>
  (semtag name value)
  semtag?
  (name semtag-name)
  (value semtag-value))

;; semgit := <commit-message> [<semtag> ...]
(define-record-type <semgit>
  (semgit message tags)
  semgit?
  (message semgit-message)
  (tags semgit-tags))

(define %file ".tmp-commit-msg")

(define* (dump-commit-message #:optional (hash "") (file %file))
  (let ((cmd (string-join `("git show --pretty=%B -q" ,hash ">" ,file)
                          " ")))
    (system cmd)))

(define* (fetch-commit-message #:optional (file %file))
  (with-input-from-file file
    (lambda ()
      (commit-message (read-line) (read-delimited ";")))))

(define (enhance-commit-message commitmsg . semtags)
  (semgit commitmsg semtags))

(define (semgit->string semgit)
  (match semgit
    (($ <semgit> ($ <commit-message> subject body) (semtags ...))
     (string-join `(,subject ,body "---semgit---"
                             ,@(map semtag->string semtags))
                  "\n"))))

(define (semtag->string semtag)
  (match semtag
    (($ <semtag> name value)
     (string-append (symbol->string name) ": " value))))

(semgit->string (enhance-commit-message (fetch-commit-message)
                                        (semtag 'feature "semtags")))

(define* (augment-commit semgit #:optional hash)
  (system* "git" "commit" "--amend" "-m" (semgit->string semgit)
           (or hash "")))
