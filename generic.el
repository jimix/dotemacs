;;
;; This should work on ALL emacs versions
;;

(server-start)
(progn (load "~/.emacs.d/genfun"))

;; C-x
(define-key global-map (kbd "C-x w") 'what-line)
(define-key global-map (kbd "C-x f") 'find-file-at-point)
(define-key global-map (kbd "C-x x") 'match-paren)
(define-key global-map (kbd "C-x t") 'touch-buffer)
(define-key global-map (kbd "C-x Q") 'force-write)
(define-key global-map [(control x) down] 'bury-buffer)
(define-key global-map [(control x) return] nil)
(define-key global-map [(control x) (control x)]
  'exchange-point-and-mark-without-zmacs)
(define-key global-map [(control x) (control c)]
  (lambda ()
    (interactive) 
    (if (yes-or-no-p "Do you want to exit ")
	(save-buffers-kill-emacs))))

(define-key global-map [(control x) (control h)]
  (lambda ()
    (interactive) 
    (delete-region (point-min) (point-max))))

(global-set-key [(control <)] 'mark-beginning-of-buffer)
(global-set-key [(control >)] 'mark-end-of-buffer)

;; Fn
(define-key global-map [f1] 'compile)
(define-key global-map [f2] 'remote-compile)
;;(define-key global-map [f8] "\C-xk\C-m\C-x0")
;;(define-key global-map [(shift f8)] "\C-xk\C-m\C-x0")
(define-key global-map [f9] 'previous-error)
(define-key global-map [f10] 'next-error)
(define-key global-map [f11] 'gdb-many-windows)
(define-key global-map [f12] 'make-buffer-neat)
(define-key global-map [(control f12)] 'rename-uniquely)
(define-key global-map [(shift f12)] 'rename-buffer)

;; Other keys
(define-key global-map [(meta g)] 'goto-line)
(define-key global-map [(control z)] nil)
(define-key global-map [(shift tab)] 'tab-to-tab-stop)
(define-key global-map [(control t)] 'toggle-truncate-lines)

(require 'shell)
(define-key shell-mode-map [(meta return)] 'shell-resync-dirs)
(define-key shell-mode-map [(control return)] 'comint-copy-old-input)
(define-key shell-mode-map [(meta p)]
  'comint-previous-matching-input-from-input)
(define-key shell-mode-map [(meta n)] 'comint-next-matching-input-from-input)


(require 'gud)
(define-key gud-mode-map [(meta p)]
  'comint-previous-matching-input-from-input)
(define-key gud-mode-map [(meta n)] 'comint-next-matching-input-from-input)


(require 'vc)
(define-key vc-prefix-map "+" 'vc-version-diff)

;; ssh stuff
(progn (load "~/.emacs.d/ssh"))
(define-key ssh-mode-map [(meta return)] 'ssh-directory-tracking-mode)

(require 'font-lock)
(require 'time)
(require 'uniquify)

;;; X11/Xdefaults used to do this for me, this does it always
(setq scroll-bar-width 5)

;;; filladapt.. I MISS you
(progn (load "~/.emacs.d/filladapt"))
(add-hook 'text-mode-hook 'filladapt-mode)
(add-hook 'log-edit-mode-hook 'filladapt-mode)
(setq default-major-mode 'filladapt-mode)


;; keep this separate cause I like to play with it
(setq frame-title-format
      '("Emacs"
	"[%p%*L%l]:"
	(buffer-file-name
	 "%f"
	 ("%b [" mode-name ":" default-directory "]"))))

;; do cscope stuff
;(progn (load "~/.emacs.d/cscope-projects"))
;(defvar cscope-init-hook nil)
;(add-hook 'cscope-init-hook (function cscope-my-projects))
(progn (load "~/.emacs.d/cscope/cscope"))
(progn (load "~/.emacs.d/cscope/cscope-project"))

;;;; These are new since I like to cscope these files as well
(add-hook 'makefile-mode-hook  (function cscope-project:c-mode-hook))
(add-hook 'sh-mode-hook (function cscope-project:c-mode-hook))
(add-hook 'asm-mode-hook (function cscope-project:c-mode-hook))
(add-hook 'compile-mode-hook (function cscope-project:c-mode-hook))

(setq auto-mode-alist
      (append auto-mode-alist
	      '(("Make"	. makefile-mode)
		("\\.make$"	. makefile-mode)
		("\\.ent$"	. sgml-mode))))

(setq c-style-variables-are-local-p t)
(setq auto-c-mode-alist
  '(("/work/xml/"				. "cc-mode")
    ("/work/manticore/"				. "mare")
    ("/work/mare/"				. "mare")
    ("/work/mambo/"				. "mambo")
    ("/work/ev3/"				. "ev3")
    ;; default
    (""						. "linux")))

(add-hook 'c-mode-common-hook 'cstyle-c-mode-hooks)
;;; color oopsie comments
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (font-lock-add-keywords nil
				    '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))

;;; mambo tcl crap
(defun mambo-tcl-hook ()
  (if (and (buffer-file-name)
	   (string-match "/work/mambo/" (buffer-file-name)))
	   (setq indent-tabs-mode nil)))
(add-hook 'tcl-mode-hook 'mambo-tcl-hook)

;;; imenu, not as good as fume but WTF
(defun try-to-add-imenu ()
  (condition-case nil (imenu-add-menubar-index) (error nil)))
(add-hook 'font-lock-mode-hook 'try-to-add-imenu)


;;; pending delete.. I LOVE you
(delete-selection-mode t)

;;; Turn on which-function mode
(which-function-mode)

;;; Turn on ido mode (obsoletes iswitchb)... nah.. to much shit
;(ido-mode t)

;;; windmove SHIFT and arrow keys
(windmove-default-keybindings)

;;; Teach the default minibuffer map how to do shell completions
(define-key minibuffer-local-map "\t" 'comint-dynamic-complete)
(define-key minibuffer-local-map "\M-\t" 'comint-dynamic-complete)
(define-key minibuffer-local-map "\M-?" 'comint-dynamic-list-completions)

;;; fix gcc include regex for newer compilers on version 23.1
;;; I don't know if this works before 23.x
(require 'compile)
;; Latest pattern from repo
(defconst my-gcc-include '(my-gcc-include
     "^\\(?:In file included \\|                 \\|\t\\)from \
\\([0-9]*[^0-9\n]\\(?:[^\n :]\\| [^-/\n]\\| :[^ :\n]\\)*?\\):\
\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?\\(?:\\(:\\)\\|\\(,\\|$\\)\\)?"
     1 2 3 (4 . 5)))
(if (string-match "^23\.1\." emacs-version)
    (progn
     ;; remove original
     (setq compilation-error-regexp-alist-alist
	   (assq-delete-all 'gcc-include compilation-error-regexp-alist-alist))
     ;; add mine
     (add-to-list 'compilation-error-regexp-alist-alist my-gcc-include)
     ;; rebuild alist
     (setq compilation-error-regexp-alist
	   (mapcar 'car compilation-error-regexp-alist-alist))))

;;; move this to a hook
;;(define-key c-mode-map [(control c) (control /)] 'recomment-line)

;;; cedet
;; (if (string-match "^24\.[3-9]\." emacs-version)
;;     (progn
;;       (global-ede-mode 1)
;;       (require 'semantic/sb)
;;       (require 'srecode)
;;       (semantic-mode 1)))


;;; add markdown mode for github files
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; Force gdb-mi to not dedicate any windows
(defadvice gdb-display-buffer (after undedicate-gdb-display-buffer)
  (set-window-dedicated-p ad-return-value nil))
(ad-activate 'gdb-display-buffer)

(defadvice gdb-set-window-buffer (after undedicate-gdb-set-window-buffer (name &optional ignore-dedi window))
  (set-window-dedicated-p window nil))
(ad-activate 'gdb-set-window-buffer)

