;;
;; Handy dandy stuff from http://www.dotfiles.com/files/7/81_my.el
;;
(defun delete-trailing-whitespace (begin end)
  "Delete trailing whitespace from all lines in region BEGIN and END."
  (save-excursion
    (narrow-to-region begin end)
    (goto-char (point-min))
    (while (re-search-forward "[ \t\r]+$" nil t)
      (replace-match ""))
    (widen)))

(defun just-one-line (begin end)
  "Leave just one blank in region BEGIN and END."
  (interactive)
  (save-excursion
    (narrow-to-region begin end)
    (goto-char (point-min))
    ;; take care if next def start with comments
    (while (re-search-forward "^\n+" nil t)
      (replace-match "\n"))))

(defun make-buffer-neat ()
  "Make code neat."
  (interactive)
  (delete-trailing-whitespace (point-min) (point-max))
  (just-one-line (point-min) (point-max)))

(defun kill-all-buffers ()
  "Kill literally all buffers.
This keeps Emacs from bloating."
  (interactive)
  (setq list (buffer-list))
  (while list
    (let* ((buffer (car list)))
      (kill-buffer buffer))
    (setq list (cdr list))))

(defun kill-all-file-buffers ()
  "Kill literally all buffers associated with a file.
This keeps Emacs from bloating."
  (interactive)
  (setq list (buffer-list))
  (while list
    (let* ((buffer (car list)))
      (if (buffer-file-name buffer) (kill-buffer buffer)))
    (setq list (cdr list))))

(defun exchange-point-and-mark-without-zmacs ()
  "Exchange point and mark without activiating zmacs region.
To activate use `activate-region' \\[activate-region] instead."
  (interactive)
  (exchange-point-and-mark 'dont-activate-region))

(defun touch-buffer ()
  "Touch, to be saved, and force recompile."
  (interactive)
  (set-buffer-modified-p t))

(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

(defun compile-buffer-name (major-mode-name)
  "Generate name for comiler buffer."
  (concat "*" (downcase major-mode-name) "*" default-directory)
)

(defun cstyle-c-mode-hooks ()
  "Look at auto-c-mode-alist to decide on the c style mode"
  (save-excursion
    (let ((name  (file-name-sans-versions buffer-file-name))
	  (alist auto-c-mode-alist)
	  (mode nil))
      (while (and (not mode) alist)
	(if (string-match (car (car alist)) name)
	    (if (and (consp (cdr (car alist)))
		     (nth 2 (car alist)))
		(progn
		  (setq mode (car (cdr (car alist)))
			name (substring name 0 (match-beginning 0))
			keep-going t))
	      (setq mode (cdr (car alist))
		    keep-going nil)))
	(setq alist (cdr alist)))
      (c-set-style mode)
      (c-set-offset 'arglist-intro '++))))


(defconst mambo-c-style '("cc-mode" (indent-tabs-mode . nil)))
(c-add-style "mambo" mambo-c-style)

(defconst ev3-c-style '("gnu" (indent-tabs-mode . nil)))
(c-add-style "ev3" ev3-c-style)

(defconst mare-c-style '("gnu"
               (indent-tabs-mode                   . nil)
               (c-basic-offset                     . 2)
               (comment-column                     . 40)
               (c-backslash-column                 . 76)

               (c-indent-comments-syntactically-p  . t)
               (c-hanging-comment-starter-p        . nil)
               (c-hanging-comment-ender-p          . nil)
               (c-comment-only-line-offset         . 0)

               (c-hanging-braces-alist . ((class-open after)
                                          (class-close before)
                                          (brace-list-open after)
                                          (brace-list-close before)
                                          (substatement-open after)
                                          (inline-open after)
                                          (extern-lang-open after)
                                          (namespace-open after)
                                          (block-close . c-snug-do-while)))

               (c-offsets-alist . ((innamespace . 0)
                                   (inextern-lang . 0)
                                   (substatement-open . 0)))

               (c-hanging-semi&comma-criteria . (c-semi&comma-no-newlines-before-nonblanks
                                                 c-semi&comma-inside-parenlist
                                                 c-semi&comma-no-newlines-for-oneline-inliners))

               (c-cleanup-list . (brace-else-brace
                                  brace-elseif-brace
                                  empty-defun-braces
                                  defun-close-semi
                                  compact-empty-funcall))
               (arglist-intro                      . ++)
	       ))
(c-add-style "mare" mare-c-style)


(defun force-write ()
  "force file and buffer to be writable"
  (interactive)
  (toggle-read-only -1)
  (shell-command (concat "chmod u+w " (buffer-file-name))))

;
; Stuff xemacs has but I want
; from simple.el
;

(defun mark-beginning-of-buffer (&optional arg)
  "Push a mark at the beginning of the buffer; leave point where it is.
With arg N, push mark N/10 of the way from the true beginning."
  (interactive "P")
  (push-mark (if arg
                 (if (> (buffer-size) 10000)
                     ;; Avoid overflow for large buffer sizes!
                     (* (prefix-numeric-value arg)
                        (/ (buffer-size) 10))
                   (/ (+ 10 (* (buffer-size) (prefix-numeric-value arg))) 10))
               (point-min))
             nil
             t))

(defun mark-end-of-buffer (&optional arg)
  "Push a mark at the end of the buffer; leave point where it is.
With arg N, push mark N/10 of the way from the true end."
  (interactive "P")
  (push-mark (if arg
                 (- (1+ (buffer-size))
                    (if (> (buffer-size) 10000)
                        ;; Avoid overflow for large buffer sizes!
                        (* (prefix-numeric-value arg)
                           (/ (buffer-size) 10))
                      (/ (* (buffer-size) (prefix-numeric-value arg)) 10)))
                 (point-max))
             nil
             t))

(defun recomment (beg end &optional arg)
  "Re comment a region it undoes one comment type and redoes it in the prefered manner"
  (interactive "*r\nP")
  (comment-normalize-vars)
  (comment-or-uncomment-region beg end arg)
  (comment-or-uncomment-region beg end arg))

(defun recomment-line ()
  "Re comment a line it undoes one comment type and redoes it in the prefered manner"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position))
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(define-derived-mode arm-mode asm-mode "ARM"
  "Major mode for editing ARM assembler code."
  ;; Unset ; key.
  (local-unset-key (vector asm-comment-char))
  (set (make-local-variable 'asm-comment-char) ?@)
  (local-set-key (vector asm-comment-char) 'asm-comment)
  ;; Update syntax for new comment char.
  (set-syntax-table (make-syntax-table asm-mode-syntax-table))
  (modify-syntax-entry asm-comment-char "< b")
  ;; Fix one level comments.
  (set (make-local-variable 'comment-start) (string asm-comment-char))
  (set (make-local-variable 'indent-tabs-mode) nil) ; use spaces to indent
  (electric-indent-mode -1) ; indentation in asm-mode is annoying
  (set (make-local-variable 'tab-stop-list) (number-sequence 4 60 4))
)


;;; handle line numbers
;;; https://www.emacswiki.org/emacs/FindFileAtPoint
(defvar ffap-file-at-point-line-number nil
  "Variable to hold line number from the last `ffap-file-at-point' call.")

(defadvice ffap-file-at-point (after ffap-store-line-number activate)
  "Search `ffap-string-at-point' for a line number pattern and
save it in `ffap-file-at-point-line-number' variable."
  (let* ((string (ffap-string-at-point)) ;; string/name definition copied from `ffap-string-at-point'
         (name
          (or (condition-case nil
                  (and (not (string-match "//" string)) ; foo.com://bar
                       (substitute-in-file-name string))
                (error nil))
              string))
         (line-number-string 
          (and (string-match ":[0-9]+" name)
               (substring name (1+ (match-beginning 0)) (match-end 0))))
         (line-number
          (and line-number-string
               (string-to-number line-number-string))))
    (if (and line-number (> line-number 0)) 
        (setq ffap-file-at-point-line-number line-number)
      (setq ffap-file-at-point-line-number nil))))

(defadvice find-file-at-point (after ffap-goto-line-number activate)
  "If `ffap-file-at-point-line-number' is non-nil goto this line."
  (when ffap-file-at-point-line-number
    (goto-line ffap-file-at-point-line-number)
    (setq ffap-file-at-point-line-number nil)))

;; https://www.emacswiki.org/emacs/CopyingWholeLines
(defun copy-line (arg)
    "Copy lines (as many as prefix argument) in the kill ring.
      Ease of use features:
      - (DISABLED) Move to start of next line.
      - Appends the copy on sequential calls.
      - (DISABLED) Use newline as last char even on the last line of the buffer.
      - If region is active, copy its lines."
    (interactive "p")
    (let ((beg (line-beginning-position))
          (end (line-end-position arg)))
      (when mark-active
        (if (> (point) (mark))
            (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
          (setq end (save-excursion (goto-char (mark)) (line-end-position)))))
      (if (eq last-command 'copy-line)
          (kill-append (buffer-substring beg end) (< end beg))
        (kill-ring-save beg end)))
;;    (kill-append "\n" nil)
;;    (beginning-of-line (or (and arg (1+ arg)) 2))
    (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))

(defun my-comint-copy-line ()
  "Copy line to comint input"
  (interactive)
  (copy-line 1)
  (comint-goto-process-mark)
  (yank))

