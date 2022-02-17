#! /usr/bin/env racket
#lang racket

;;; Commentary:
;;
;; Usage: dostow FOLDER
;;
;; This tool is a Racket implementation of part of the functionality of
;; the GNU Stow utility, dependint strongly on certain assumptions (see list),
;; while fixing stow issues with "dot-" syntax for folders, and adding support
;; for basic templating to address some limitations I found when deploying
;; certain of my dot files on different systems with different usernames, etc.
;;
;; Assumptions:
;;   - FOLDER is located in ${HOME}/<whatever-name-you-use>/FOLDER
;;   - files to be ignored are suffixed with '-nostow' or '-NOSTOW'
;;   - dot files or folder names are preffixed by 'dot-'
;;   - template file names are preffixed by 'template-'
;;      - templates are evaluable Racket scripts returning the contents to be
;;        written to the intended file (OBVIOUSLY UNSAFE)

;;; Code:

(module+ main

  (require racket/cmdline)
  (require racket/file)
  (require racket/match)
  (require racket/list)
  (require racket/path)
  (require racket/string)
  (require racket/set)

  (define (target-name filename)
    "Returns intended resulting filename"
    (let ([m (regexp-match* (regexp "^(dot|template)-([ -~]*)") filename
                            #:match-select rest)])
      (if (empty? m)
          filename
          (let ((fname (last (first m)))
                (ftype (first (first m))))
            (if (equal? ftype "template")
                fname
                (format ".~a" fname))))))

  (define (rebase-path original-path path-to-remove base-path)
    "Rebase (original-path - path-to-remove) onto base-path"
    (let* ([op (explode-path original-path)]
           [ptr (explode-path path-to-remove)]
           [relative (string-join (map path->string (reverse (set-subtract op ptr))) "/")])
      (string-join (list base-path relative) "/")))

  (define (process-template src dst)
    "Evaluate template located at <arg:src> and write output to <arg:dst>"
    (display (format "[template] ~a -> ~a ~%" src dst))
    (let* ((raw-content (file->list src))
           (ns (make-base-namespace))
           (content (last (map (lambda (x) (eval x ns)) raw-content))))
      (call-with-output-file dst
        (lambda (port) (display content port)))))

  (define (process-folder name)
    "Process folder"
    (let* ((target (map target-name (string-split name "/")))
           (dname (string-join (cons base-path target) "/"))
           (rdname (rebase-path dname (unbox root-folder) base-path)))
      (display (format "[folder] ~a -> ~a~%" name rdname))
      (unless (directory-exists? rdname)
          (begin
            (display (format "Creating folder: ~a ~%" rdname))
            (make-directory rdname)))))

  (define (process-file name)
    "Process standard files"
    (let* ((bname (file-name-from-path name))
           (target (map target-name (string-split name "/")))
           (dname (string-join (cons base-path target) "/"))
           (rdname (rebase-path dname (unbox root-folder) base-path)))
      (let ([m (regexp-match "^template-" bname)])
        (when (file-exists? rdname) (delete-file rdname))
        (if m
            ;; Evaluate template
            (process-template name rdname)
            (begin
              (display (format "[file] ~a -> ~a~%" name rdname))
              (make-file-or-directory-link name rdname))))))

    ;; https://www.gnu.org/software/guile/manual/html_node/File-Tree-Walk.html
  (define (dotstow directory-name)
    "stow files under FOLDER (similar to GNU Stow.)"

    (for ([f (in-directory directory-name (lambda (name) (not (regexp-match "-nostow$" name))))])
      (cond
        [(regexp-match "-nostow$" f) #f] ; Ignore
        [(file-exists? f) (process-file (path->string f))]
        [(directory-exists? f) (process-folder (path->string f))]
        [else #f])))

  (define base-path (getenv "HOME"))
  (define root-folder (box "."))

  (command-line
   #:program "dotstow"
   #:once-each
   [("-t" "--target-folder") target-folder "Target folder" (set-box! root-folder target-folder)]
   #:args ()
   (dotstow (unbox root-folder))))

