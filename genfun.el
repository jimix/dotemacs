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
      (c-set-style mode))))

(defun force-write ()
  "force file and buffer to be writable"
  (interactive)
  (toggle-read-only -1)
  (shell-command (concat "chmod u+w " (buffer-file-name))))

;
; Stuff xemacs has but I want
; frome simple.el
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
