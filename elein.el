;;; elein.el -- running leiningen commands from emacs

;; Copyright (C) 2010 R.W van 't Veer

;; Author: R.W. van 't Veer
;; Created: 2 Aug 2010
;; Keywords: tools processes

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; Provides support for running leiningen commands like swank and test.

;;; Code:

(require 'cl)

(defgroup elein nil
  "running leiningen commands from emacs"
  :prefix "elein-"
  :group 'applications)

(defcustom elein-lein "lein"
  "Leiningen 'lein' command."
  :type 'string
  :group 'elein)

(defcustom elein-swank-buffer-name "*elein-swank*"
  "Buffer name for swank process."
  :type 'string
  :group 'elein)

(defcustom elein-swank-port 4005
  "Swank port to listen."
  :type 'integer
  :group 'elein)

(defcustom elein-swank-host "127.0.0.1"
  "Swank address to listen."
  :type 'string
  :group 'elein)

(defcustom elein-swank-options ":encoding '\"utf-8\"'"
  "Swank options."
  :type 'string
  :group 'elein)

(defun elein-project-root ()
  "Look for project.clj file to find project root."
  (let ((cwd default-directory)
        (found nil)
        (max 10))
    (while (and (not found) (> max 0))
      (if (file-exists-p (concat cwd "project.clj"))
        (setq found cwd)
        (setq cwd (concat cwd "../") max (- max 1))))
    (and found (expand-file-name found))))

(defmacro elein-in-project-root (body)
  "Wrap BODY to make `default-directory' the project root."
  (let ((dir (gensym)))
    `(let ((,dir (elein-project-root)))
       (if ,dir
         (let ((default-directory ,dir)) ,body)
         (error "No leiningen project root found")))))

(defvar elein-task-alist nil
  "Holds cached task list by directory name.  The car of the
  value is the mtime of the project.clj file and the cdr is the
  task list itself.")

(defun elein-project-clj-mtime ()
  "Get mtime from the project.clj in the current project."
  (nth 5 (elein-in-project-root
          (file-attributes "project.clj"))))

(defun elein-list-tasks ()
  "Collect tasks for current project."
  (let* ((root (elein-project-root))
         (cached (assoc root elein-task-alist)))
    (if (and cached (equal (elein-project-clj-mtime) (cadr cached)))
      (cddr cached)
      (let ((tasks (elein-in-project-root
                    (let ((output (shell-command-to-string (concat elein-lein " help")))
                          (result nil)
                          (offset 0))
                      (while (string-match "^  \\(.*\\)" output offset)
                        (setq result (cons (match-string 1 output) result))
                        (setq offset (match-end 0)))
                      (sort result (lambda (a b) (string< a b)))))))
        (setq elein-task-alist (cons (cons root (cons (elein-project-clj-mtime)
                                                      tasks))
                                     elein-task-alist))
        tasks))))

(defun elein-swank-command ()
  "Build lein swank command from customization variables."
  (format "%s swank %d %s %s"
          elein-lein
          elein-swank-port
          elein-swank-host
          elein-swank-options))

;;;###autoload
(defun elein-swank ()
  "Launch lein swank and connect slime to it."
  (interactive)
  (let ((buffer (get-buffer-create elein-swank-buffer-name)))
    (flet ((display-buffer (buffer-or-name &optional not-this-window frame) nil))
      (bury-buffer buffer)
      (elein-in-project-root (shell-command (concat (elein-swank-command) "&") buffer)))

    (set-process-filter (get-buffer-process buffer)
                        (lambda (process output)
                          (with-current-buffer elein-swank-buffer-name (insert output))

                          (when (string-match "Connection opened on local port +\\([0-9]+\\)" output)
                            (slime-set-inferior-process
                             (slime-connect "localhost" (match-string 1 output))
                             process)
                            (set-process-filter process nil))))

    (message "Starting swank..")))

;;;###autoload
(defun elein-kill-swank ()
  "Kill swank process started by lein swank."
  (interactive)
  (let ((process (get-buffer-process "*elein-swank*")))
    (when process
      (ignore-errors (slime-quit-lisp))
      (let ((timeout 10))
        (while (and (> timeout 0)
                    (eql 'run (process-status process)))
          (sit-for 1)
          (decf timeout)))
      (ignore-errors (kill-buffer "*elein-swank*")))))

;;;###autoload
(defun elein-reswank ()
  "Kill current lisp, restart lein swank and connect slime to it."
  (interactive)
  (elein-kill-swank)
  (elein-swank))

;;;###autoload
(defun elein-run-cmd (args)
  "Run 'lein ARGS' using `compile' in the project root directory."
  (interactive "sArguments: ")
  (elein-in-project-root (compile (concat elein-lein " " args))))

;;;###autoload
(defun elein-run-task (task)
  "Run 'lein TASK' using `compile' in the project root directory."
  (interactive (list (completing-read "Task: " (elein-list-tasks))))
  (elein-run-cmd task))

(defmacro elein-defun-run-task (task)
  "Define shortcut function for `elein-run-task' with argument TASK."
  `(defun ,(intern (concat "elein-" task)) ()
     ,(concat "Run 'lein " task "' in the project root directory.")
     (interactive)
     (elein-run-task ,task)))

;; define interactive elein-TASK commands for common tasks
(dolist (task '(classpath
                clean
                compile
                deps
                help
                install
                jar
                new
                pom
                repl
                test
                uberjar
                upgrade
                version))
  (eval `(elein-defun-run-task ,(symbol-name task))))

(provide 'elein)

;;; elein.el ends here
