; emacs "shift-mark" functionality
;
; Allows you to mark a region by holding down the Shift modifier key
; and moving the cursor.
; Source: http://www.cs.ucsb.edu/~matz/study/EmacsShiftMark.html
;
; written by matz"a"cs.ucsb.edu, March 10th, 1998

(defun shift-mark (cmd)
  "Expands marked region to the point (position of cursor) after executing
command 'cmd'. If no region is marked, we mark one first."
  (interactive "_a")
  (if (not (region-active-p))
      (progn (set-mark-command nil)
	     (command-execute cmd))
  (command-execute cmd)
))

(defun shift-mark-forward-char ()
  (interactive)
  (shift-mark 'forward-char)
)

(defun shift-mark-backward-char ()
  (interactive)
  (shift-mark 'backward-char)
)

(defun shift-mark-forward-word ()
  (interactive)
  (shift-mark 'forward-word)
)

(defun shift-mark-backward-word ()
  (interactive)
  (shift-mark 'backward-word)
)

(defun shift-mark-forward-paragraph ()
  (interactive)
  (shift-mark 'forward-paragraph)
)

(defun shift-mark-backward-paragraph ()
  (interactive)
  (shift-mark 'backward-paragraph)
)

(defun shift-mark-previous-line ()
  (interactive)
  (shift-mark 'previous-line)
)

(defun shift-mark-next-line ()
  (interactive)
  (shift-mark 'next-line)
)

(defun backspace-delete-marked-region ()
  (interactive)
					;  (zmacs-region-stays t)
  (if (region-active-p)
      (kill-region (mark) (point))
    (delete-backward-char 1)
    )
)

(global-set-key [(S right)]      'shift-mark-forward-char)
(global-set-key [(S left)]    'shift-mark-backward-char)
(global-set-key [(S up)]	  'shift-mark-previous-line)
(global-set-key [(S down)]    'shift-mark-next-line)
(global-set-key [(S C right)]    'shift-mark-forward-word)
(global-set-key [(S C left)]    'shift-mark-backward-word)
(global-set-key [(S C up)]	  'shift-mark-backward-paragraph)
(global-set-key [(S C down)]    'shift-mark-forward-paragraph)
(global-set-key [(S backspace)]	 'backspace-delete-marked-region)
(global-set-key [(C backspace)]    'backspace-delete-marked-region)
(global-set-key [(S C backspace)]	 'backspace-delete-marked-region)
(global-set-key [(del)]	 'backspace-delete-marked-region)

(global-set-key [(C left)] 'backward-word)
(global-set-key [(C right)] 'forward-word)
(global-set-key [(C up)] 'backward-paragraph)
(global-set-key [(C down)] 'forward-paragraph)
(global-set-key [(f27)] 'beginning-of-line);HOME
(global-set-key [(f33)] 'end-of-line)	   ;END
