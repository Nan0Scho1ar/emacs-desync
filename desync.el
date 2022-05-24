;;; desync.el --- Desynchronize function calls to prevent lockups -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Nan0Scho1ar
;;
;; Author: Nan0Scho1ar <scorch267@gmail.com>
;; Maintainer: Nan0Scho1ar <scorch267@gmail.com>
;; Created: May 25, 2022
;; Modified: May 25, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/nan0scho1ar/desync
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Concurrency where parallelism is not an option.
;;
;;; Code:

;; Parent Vars
(defvar-local desync-parent nil)
(defvar-local desync-parent-cmd nil)
(defvar-local desync-parent-cmd-type nil)
(defvar-local desync-parent-post-process nil)
(defvar-local desync-parent-buffer-lock nil)
(defvar-local desync-children nil)

;; Child Vars
(defvar-local desync-child-cmd nil)
(defvar-local desync-child-cmd-type nil)
(defvar-local desync-child-post-process nil)


(defun desync/parent-create-buffer (name)
  "NAME CHILDREN."
  (with-current-buffer (get-buffer-create name)
    (erase-buffer)
    (current-buffer)))

(defun desync/parent-run (buffer)
  "Run list CMD in child buffer BUFFER."
  (with-current-buffer buffer
    (make-process :name (buffer-name buffer)
                  :buffer buffer
                  :command (list "sleep" "1")
                  :noquery t
                  :sentinel #'desync/parent-sentinel)))

;; TODO
(defun desync/parent-sentinel (process event)
  "Execute when PROCESS triggers EVENT for further processing."
  (let ((parent-buffer (process-buffer process))
        (sent nil))
    (when (not (null parent-buffer))
      (with-current-buffer parent-buffer
        ))))

(defun desync/parent-run-children (buffer)
  (with-current-buffer buffer
    (dolist (child desync-children)
      (with-current-buffer child (message "%s" desync-child-cmd))
      (desync/child-run child))))

(defun desync/child-create-buffer (name parent cmd cmd-type post-process dir)
  "OUTBUFFER-NAME PATH-LIST."
  (with-current-buffer (generate-new-buffer name t)
    (setq desync-child-cmd           cmd
          desync-child-cmd-type      cmd-type
          desync-parent              parent
          desync-child-post-process  post-process
          default-directory          dir)
    (current-buffer)))

(defun desync/child-run (buffer)
  "Run list CMD in child buffer BUFFER."
  (with-current-buffer buffer
    (cond ((equal desync-child-cmd-type 'shell-command)
           (make-process :name (buffer-name buffer)
                         :buffer buffer
                         :command desync-child-cmd
                         :noquery t
                         :sentinel #'desync/child-sentinel))
          ((equal desync-child-cmd-type 'elisp) (message "TODO Child elisp")))))

(defun desync/child-sentinel (process event)
  "Execute when PROCESS triggers EVENT for further processing."
  (let ((child-buffer (process-buffer process))
        (sent nil))
    (when (not (null child-buffer))
      (with-current-buffer child-buffer
        (let ((output (list desync-child-cmd event (buffer-string))))
          (when desync-child-post-process
            (setq output (funcall desync-child-post-process output)))
          (with-current-buffer desync-parent
            (while (not sent)
              (unless desync-parent-buffer-lock
                (setq desync-parent-buffer-lock child-buffer)
                (when (equal desync-parent-buffer-lock child-buffer)
                  (insert (format "%S\n" output))
                  (delete child-buffer desync-children)
                  (when (equal event "finished\n")
                    (kill-buffer child-buffer))
                  (setq desync-parent-buffer-lock nil
                        sent t))))))))))

(defun desync/map-shell-command (buffname cmd paths &optional post-proc)
  "Run CMD for each PATHS and fill BUFFNAME using POST-PROC.
Asyncronosly run many commands and aggregate all the
results into a single buffer."
  (when (stringp cmd) (setq cmd (list cmd)))
  (let ((parent-buffer (desync/parent-create-buffer buffname))
        (child-buffname (format "%s child" buffname))
        (children '()))
    (dolist (path paths children)
      (push (desync/child-create-buffer child-buffname parent-buffer cmd 'shell-command post-proc path) children))
    (with-current-buffer parent-buffer
      (setq desync-children children))

    (desync/parent-run-children parent-buffer)))


(setq test-dirs '("/home/nan0scho1ar/repos/me/dotfiles"
                  "/home/nan0scho1ar/repos/me/scripts"
                  "/home/nan0scho1ar/repos/me/gitmanager"
                  "/home/nan0scho1ar/repos/me/bish"))

(desync/map-shell-command "LS" "ls" test-dirs)

;; Desync Parent
;; Desync Child

;; Desync prog

(provide 'desync)
;;; desync.el ends here
