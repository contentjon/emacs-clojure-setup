;;;;;;;;;;;;;;;;;;
;; Global Stuff ;;
;;;;;;;;;;;;;;;;;;

(setq conf-dir (concat
                ;; expanded for stuff like java class path
                (expand-file-name user-emacs-directory)
                user-login-name
                "/"))

(setq inhibit-splash-screen t)
(push '("." . "~/.emacs-backups") backup-directory-alist)
(desktop-save-mode 1)
(setq kill-whole-line t)
(show-paren-mode t)
;(setq-default transient-mark-mode t)
(setq show-trailing-whitespace t)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(remove-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq-default fill-column 72)
(setq-default indent-tabs-mode nil)

(setq tab-stop-list
  (let ((res nil))
    (dotimes (i 50 res)
      (setq res (cons (* 2 (- 50 i)) res)))))

(require 'bar-cursor)
(bar-cursor-mode 1)
(blink-cursor-mode nil)
(fset 'yes-or-no-p 'y-or-n-p)
(setq search-highlight t
      query-replace-highlight t)
(setq x-select-enable-clipboard t)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; starter-kit-cleanup ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(remove-hook 'coding-hook 'turn-on-idle-highlight)
(remove-hook 'coding-hook 'turn-on-hl-line-mode)
(setq ring-bell-function 'ignore)

;;;;;;;;;;;;;;;;;;;;;;
;; Desktop Projects ;;
;;;;;;;;;;;;;;;;;;;;;;

;; defines development-dir
(load "local-settings")

(defun project (name)
  (let ((dir (concat development-dir "/" name)) )
    (desktop-change-dir dir)
    (cd dir)))

(defun libs ()
  (interactive)
  (project "libs"))

(defun panda ()
  (interactive)
  (project "gopanda"))

(defun go ()
  (interactive)
  (project "gostrategy"))

(defun sandbox ()
  (interactive)
  (project "~/dev/sandbox"))

(defun bet ()
  (interactive)
  (project "~/dev/bet"))

;;;;;;;;;;;;;;
;; cua-mode ;;
;;;;;;;;;;;;;;

(cua-mode)

;;;;;;;;;;;;;;
;; uniquify ;;
;;;;;;;;;;;;;;

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;;;;;;;;;
;; ido ;;
;;;;;;;;;

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-create-new-buffer 'always)

;;;;;;;;;;
;; Look ;;
;;;;;;;;;;

;(tool-bar-mode -1)
;(menu-bar-mode -1)
(set-scroll-bar-mode 'right)
(set-default-font "Monospace-11")
;; (set-default-font "Monospace-12")

(set-frame-position (selected-frame) 0 0)
(let ((max-size '(170 . 52)))
  (set-frame-size (selected-frame) (car max-size) (cdr max-size)))

;; (require 'color-theme)
(load "color-theme-blue")(color-theme-blue)
;; (load "color-theme-dark-bliss")(color-theme-dark-bliss)
;; (load "color-theme-twilight")(color-theme-twilight)
;; (load "color-theme-sunburst")(color-theme-tm)

(global-font-lock-mode 1)
;; maximum colors
(setq font-lock-maximum-decoration t)

;;;;;;;;;;;;;;;;;;
;; Transparency ;;
;;;;;;;;;;;;;;;;;;

(defun djcb-opacity-modify (&optional dec)
  "modify the transparency of the emacs frame; if DEC is t,
    decrease the transparency, otherwise increase it in 5%-steps"
  (let* ((alpha-or-nil (frame-parameter nil 'alpha)) ; nil before setting
          (oldalpha (if alpha-or-nil alpha-or-nil 100))
          (newalpha (if dec (- oldalpha 5) (+ oldalpha 5))))
    (when (and (>= newalpha frame-alpha-lower-limit) (<= newalpha 100))
      (modify-frame-parameters nil (list (cons 'alpha newalpha))))))

(djcb-opacity-modify t)
;(djcb-opacity-modify)

;;;;;;;;;;;;;;;;
;; Key Chords ;;
;;;;;;;;;;;;;;;;

(require 'key-chord)
(key-chord-mode 1)

;;;;;;;;;;;;;;;;;;;;;
;; Key Definitions ;;
;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-S-<right>") 'other-window)
(global-set-key (kbd "C-S-<left>") (lambda () (interactive) (other-window -1)))

(global-set-key (kbd "C-+") 'text-scale-adjust)
(global-set-key (kbd "C--") 'text-scale-adjust)
(global-set-key (kbd "C-0") 'text-scale-adjust)

(global-set-key [f2] 'fullscreen)

(global-set-key [f5]   (lambda()(interactive)(slime-connect "127.0.0.1" 4005)))
(global-set-key [f6]   'clojurescript-repl)

(global-set-key [f8]   (lambda()(interactive)(find-file (concat conf-dir "init.el"))))
(global-set-key [f9]   'start-kbd-macro)
(global-set-key [f10]  'end-kbd-macro)
(global-set-key [f11]  'call-last-kbd-macro)
(global-set-key [f12]  'apply-macro-to-region-lines)

;; C-8 will increase opacity (== decrease transparency)
(global-set-key (kbd "C-8") '(lambda()(interactive)(djcb-opacity-modify)))
;; C-9 will decrease opacity (== increase transparency
(global-set-key (kbd "C-9") '(lambda()(interactive)(djcb-opacity-modify t)))
;; C-0 will returns the state to normal
(global-set-key (kbd "C-0") '(lambda()(interactive)
                               (modify-frame-parameters nil `((alpha . 100)))))

;; (global-set-key "\M-C" 'compile)
;; (global-set-key "\C-^" 'next-error)
;; (global-set-key "\C-\M-g" 'goto-line)

(key-chord-define-global ",." 'comment-or-uncomment-region)
(key-chord-define-global "<y" 'previous-buffer)
(key-chord-define-global "<x" 'next-buffer)

(global-set-key [(C f8)] 'dotemacs-header)

(add-to-list 'load-path (concat conf-dir "midje"))
(require 'clojure-mode)
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))
(require 'midje-mode)
(add-hook 'clojure-mode-hook 'midje-mode)

(key-chord-define clojure-mode-map "89"      'align-cljlet)
(key-chord-define clojure-mode-map "öä"      'clojure-set-ns-goto-repl)
(define-key clojure-mode-map (kbd "C-c t")   'clojure-goto-test-or-back)
(define-key clojure-mode-map (kbd "C-c C-a") 'clojure-add-ns)

(eval-after-load 'clojure-mode
  '(define-clojure-indent
     (def 'defun)
     (redef 'defun)
     (redefn 'defun)
     (let-args 'defun)
     (fact 'defun)
     (against-background 'defun)
     (defhandler 'defun)))

(require 'align-cljlet)

;;;;;;;;;;;;;
;; Paredit ;;
;;;;;;;;;;;;;

(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       (lambda () (paredit-mode +1)))
(add-hook 'lisp-mode-hook             (lambda () (paredit-mode +1)))
(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'clojure-mode-hook          (lambda () (paredit-mode +1)))

(require 'durendal)

(durendal-enable)

;; (require 'elein)

(custom-set-variables '(slime-net-coding-system (quote utf-8-unix)))

;;;;;;;;;;;;;;;;
;; Processing ;;
;;;;;;;;;;;;;;;;

;; (autoload 'processing-mode "processing-mode" "Processing mode" t)
;; (add-to-list 'auto-mode-alist '("\\.pde$" . processing-mode))
;; (setq processing-location "/Applications/Processing.app/")

;;;;;;;;;;;;
;; AucTex ;;
;;;;;;;;;;;;

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (ispell-change-dictionary "deutsch")
            (setq TeX-open-quote "\"`")
            (setq TeX-close-quote "\"'")))

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq font-latex-quotes 'german)
;(add-hook 'LaTeX-mode-hook 'flyspell-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Working with .emacs ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dotemacs-header ()
  (interactive)
  (let* ((header (read-from-minibuffer "Header name: "))
         (text-line (concat ";; " header " ;;"))
         (count (length text-line))
         (comment-line (make-string count (string-to-char ";"))))
    (insert (concat
             comment-line "\n"
             text-line "\n"
             comment-line "\n\n"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clojure Convenience Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun clojure-string->keyword ()
  (interactive)
  (goto-char (search-backward "\""))
  (delete-char 1)
  (insert ":")
  (goto-char (- (search-forward "\"")
                1))
  (delete-char 1))

(defun clojure-ns->file (ns)
  (concat
   (replace-in-string
    (replace-in-string ns "[.]" "/")
    "-" "_")
   ".clj"))

(defun clojure-file->ns (file)
  (replace-in-string
   (replace-in-string
    (replace-in-string file "/" ".")
    "_" "-")
   ".clj$" ""))

(defun clojure-add-ns ()
  (interactive)
  (let* ((current-project (locate-dominating-file (buffer-file-name)
                                                  "project.clj"))
         (promt           (if current-project
                              (concat "Add ns to Project (default " current-project "): ")
                            "Add ns to Project: "))
         (target-project  (file-name-as-directory
                           (read-directory-name
                            promt
                            (file-name-as-directory development-dir)
                            current-project)))
         (ns              (read-from-minibuffer "Namespace: "))
         (file            (concat target-project
                                  "src/"
                                  (clojure-ns->file ns)))
         (dir             (file-name-directory file)))
    (if (file-exists-p file)
        (find-file file)
      (progn (make-directory dir t)
             (find-file file)
             (insert "(ns " ns ")\n\n")))))

(defun clojure-goto-test-or-back ()
  (interactive)
  (let* ((current-file    (buffer-file-name))
         (current-project (locate-dominating-file current-file
                                                  "project.clj"))
         (test-p          (string-match "/test/test/" current-file))
         (rel-filename    (file-relative-name
                           current-file
                           (concat current-project
                                   (if test-p "test/test/" "src/"))))
         (outer-path      (concat current-project
                                  (if test-p "src/" "test/test/")))
         (target-file     (concat outer-path rel-filename)))
    (other-window 1)
    (if (or test-p
            (file-exists-p target-file)
            (find-buffer-visiting target-file))
        (find-file target-file)
      (let* ((ns  (clojure-file->ns rel-filename))
             (dir (file-name-directory target-file)))
        (progn (make-directory dir t)
               (find-file target-file)
               (insert "(ns test." ns
                       "\n  (:use (midje sweet)"
                       "\n        [" ns " :reload true]))\n\n"))))))

(defun clojure-set-ns-goto-repl ()
  (interactive)
  (slime-repl-set-package (slime-find-buffer-package))
  (slime-switch-to-output-buffer))

;;;;;;;;;;;;;;;;;;;
;; ClojureScript ;;
;;;;;;;;;;;;;;;;;;;

(defun clojurescript-repl ()
  (interactive)
  (let ((cljs-home (getenv "CLOJURESCRIPT_HOME")))
    (run-lisp (concat cljs-home "/script/repl"))
    (paredit-mode +1)
    (lisp-eval-string "(require '[cljs.repl :as repl])
                       (require '[cljs.repl.browser :as browser])
                       (def env (browser/repl-env))
                       (repl/repl env)")))
