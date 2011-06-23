;;;;;;;;;;;;;;;;;;
;; Global Stuff ;;
;;;;;;;;;;;;;;;;;;

(setq inhibit-splash-screen t)
(push '("." . "~/.emacs-backups") backup-directory-alist)
(desktop-save-mode 1)
(setq kill-whole-line t)
(show-paren-mode t)
;(setq-default transient-mark-mode t)
(setq show-trailing-whitespace t)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
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

;;;;;;;;;;;;;;;;;;;;;;
;; Desktop Projects ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun project (dir)
  (desktop-change-dir dir)
  (cd dir))

(defun libs ()
  (interactive)
  (project "~/dev/libs"))

(defun panda ()
  (interactive)
  (project "~/dev/gopanda"))

(defun go ()
  (interactive)
  (project "~/dev/gostrategy"))

(defun gen ()
  (interactive)
  (project "~/dev/clj-gen"))

(defun contenjon ()
  (interactive)
  (project "~/dev/contenjon"))

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

(global-set-key (kbd "C-+") 'text-scale-adjust)
(global-set-key (kbd "C--") 'text-scale-adjust)
(global-set-key (kbd "C-0") 'text-scale-adjust)

(global-set-key [f2] 'fullscreen)

(global-set-key [f5]   (lambda()(interactive)(slime-connect "127.0.0.1" 4005)))

(global-set-key [f8]   (lambda()(interactive)(find-file "~/.emacs.d/ben/init.el")))
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
(key-chord-define-global "89" 'clojure-mode)
(key-chord-define-global "öä" 'clojure-string->keyword)
(global-set-key [(C f8)] 'dotemacs-header)

;; (setq swank-clojure-init-files '("/Users/ben/.emacs.d/clojure-init.clj"))

(add-to-list 'load-path "~/.emacs.d/ben/midje")
(require 'clojure-mode)
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
(require 'midje-mode)
(add-hook 'clojure-mode-hook 'midje-mode)

(eval-after-load 'clojure-mode
  '(define-clojure-indent
     (describe 'defun)
     (testing 'defun)
     (given 'defun)
     (using 'defun)
     (with 'defun)
     (it 'defun)
     (do-it 'defun)
     (def 'defun)
     (add-parser 'defun)
     (p/let 'defun)
     (register-hook 'defun)
     (bind 'defun)
     (where 'defun)
     (with 'defin)))

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
(add-hook 'slime-repl-mode-hook       (lambda () (paredit-mode +1)))


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
            (setq TeX-close-quote "\"'")
            (local-set-key (kbd "C-c i") (lambda ()
                                           (interactive)
                                           (insert "\\icode{")))
            ))

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

(defun clojure-string->keyword ()
  (interactive)
  (goto-char (search-backward "\""))
  (delete-char 1)
  (insert ":")
  (goto-char (- (search-forward "\"")
                1))
  (delete-char 1))

;;;;;;;;;;;;
;; Server ;;
;;;;;;;;;;;;

(server-start)
