;;;;;;;;;;;;;;;;;;
;; Global Stuff ;;
;;;;;;;;;;;;;;;;;;

(setq inhibit-splash-screen t)
(add-to-list 'load-path "~/.emacs.d/")
(push '("." . "~/.emacs-backups") backup-directory-alist)
(desktop-save-mode 1)
(setq kill-whole-line t)
(show-paren-mode t)
;(setq-default transient-mark-mode t)
(setq show-trailing-whitespace t)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
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

(defun magicl ()
  (interactive)
  (desktop-change-dir "~/magicl"))

(defun pandanet ()
  (interactive)
  (desktop-change-dir "~/pandanet"))

(defun goni ()
  (interactive)
  (desktop-change-dir "~/dev/goni"))

(defun clj-go ()
  (interactive)
  (desktop-change-dir "~/dev/clj-go"))

(defun clj-gen ()
  (interactive)
  (desktop-change-dir "~/dev/clj-gen"))

(defun contenjon ()
  (interactive)
  (desktop-change-dir "~/dev/contenjon"))

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
;(set-default-font "Monospace-9")
(set-default-font "Monospace-11")

(set-frame-position (selected-frame) 0 0)
(let ((max-size (cond
                 ((eql window-system 'ns) '(200 . 77))
                 ((eql window-system 'x)  '(200 . 77)))))
  (set-frame-size (selected-frame) (car max-size) (cdr max-size)))

(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0")
(require 'color-theme)
(load "color-theme-blue")(color-theme-blue)
(load "color-theme-dark-bliss") ;(color-theme-dark-bliss)
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

(global-set-key (kbd "C-c i") (lambda()(interactive)(insert "\\icode{")))
(global-set-key (kbd "C-c s") (lambda()(interactive)(insert "\\sexp{}")))
(global-set-key (kbd "C-c S") (lambda()(interactive)(insert "\\sexps{}")))

(global-set-key [f2] 'fullscreen)

(global-set-key [f5]   (lambda()(interactive)(compile "cd ~/magicl;mage")))
(global-set-key [f6]   (lambda()(interactive)(shell-command "mage doc")))

(global-set-key [f8]   (lambda()(interactive)(find-file "~/.emacs")))
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
(global-set-key [(C f8)] 'dotemacs-header)

;; Mac Keys
(global-set-key (kbd "M-5") "[")
(global-set-key (kbd "M-6") "]")
(global-set-key (kbd "M-7") "|")
(global-set-key (kbd "M-/") "\\")
(global-set-key (kbd "C-M-7") "\\")

(global-set-key (kbd "M-8") "{")
(global-set-key (kbd "M-9") "}")

(global-set-key (kbd "M-#") "~")
(global-set-key (kbd "M-+") "@")

;; (setq swank-clojure-classpath
;;       (list "~/dev/clojure/clojure-slim-1.2.0-master-SNAPSHOT.jar"
;;             "~/dev/clojure-contrib/target/clojure-contrib-1.2.0-SNAPSHOT.jar"
;;             "~/dev/swank-clojure/swank-clojure.jar"))

(add-to-list 'load-path "~/.emacs.d/elpa/clojure-mode-1.6")
(require 'clojure-mode)

;;;;;;;;;;;;;
;; Paredit ;;
;;;;;;;;;;;;;

(eval-after-load 'paredit
  '(progn (define-key paredit-mode-map (kbd "{")
            'paredit-open-brace)
          (define-key paredit-mode-map (kbd "}")
            'paredit-close-brace)))

(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       (lambda () (paredit-mode +1)))
(add-hook 'lisp-mode-hook             (lambda () (paredit-mode +1)))
(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'clojure-mode-hook          (lambda () (paredit-mode +1)))

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

;;;;;;;;;;
;; ELPA ;;
;;;;;;;;;;

(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))


;;;;;;;;;;;
;; CEDET ;;
;;;;;;;;;;;

(defun load-rudel ()
  (interactive)
  ;; Do these in a shell in ~/src:
  ;; cvs -d:pserver:anonymous@cedet.cvs.sourceforge.net:/cvsroot/cedet login
  ;; cvs -z3 -d:pserver:anonymous@cedet.cvs.sourceforge.net:/cvsroot/cedet co -P cedet
  ;; cd cedet && make && cd ..
  ;; svn co https://rudel.svn.sourceforge.net/svnroot/rudel/rudel/trunk rudel
  (add-to-list 'load-path "~/.emacs.d/cedet-1.0pre7/eieio")
  (add-to-list 'load-path "~/.emacs.d/cedet-1.0pre7/common")
  (add-to-list 'load-path "~/.emacs.d/rudel-0.2-4/")
  (add-to-list 'load-path "~/.emacs.d/rudel-0.2-4/jupiter")
  (add-to-list 'load-path "~/.emacs.d/rudel-0.2-4/obby")
  (require 'rudel-mode)
  (require 'rudel-obby)
  (global-rudel-minor-mode))


;;;;;;;;;;;;;;;;
;; Organizing ;;
;;;;;;;;;;;;;;;;

(split-window-horizontally)
(find-file "~/TODO")
(find-file-other-window "~/CALENDER")

;;;;;;;;;;;;
;; Server ;;
;;;;;;;;;;;;

(server-start)