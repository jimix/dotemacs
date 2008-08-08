;;
;; Project stuff for cscope mode (cscope.el)
;;
;; Copyright (c) 1998-2000 by Georg Nikodym
;;
;; In trans-gate:
;;
;;	ident "@(#)cscope-project.el 1.3	01/01/10"
;;
;; Usage:
;;
;; After installing this file (and the .elc) into somewhere useful in
;; your load-path, simply add:
;;
;;	(require 'cscope-project)
;;
;; to your .emacs file
;; 

(require 'custom)
(require 'cus-load)

(defgroup cscope nil
  "Support for cscope addons"
  :group 'local)

(custom-add-load 'cscope '("cscope-project"))

;; Cscope additions
(autoload (function cscope-project:cscope-bind-keys) "cscope-project")
(autoload (function cscope-project:c-mode-hook) "cscope-project")
(autoload (function cscope-project:c++-mode-hook) "cscope-project")
(autoload (function cscope-new-project) "cscope-project")
(autoload (function cscope-new-project-disjoint) "cscope-project")
(add-hook 'cscope-bind-keys-hook (function cscope-project:cscope-bind-keys))

;; The load hook
(defcustom cscope-project-hook nil
  "*Hook called when `cscope-project' is loaded."
  :group 'cscope
  :type 'hook)

(defcustom cscope-projects-file "~/.cscope-projects"
  "*The name of the file in which cscope projects are stored."
  :group 'cscope
  :type 'file)

(defvar cscope-projects-buffer nil)

;; Set the autoload
(autoload 'cscope-bind-keys "cscope" nil t)
(autoload 'cscope-find-c-symbol "cscope" nil t)

;; The key to the Temple (see cscope.el)
(defvar cscope-id nil)
(make-variable-buffer-local 'cscope-id)

;; Set up the functions for source browsing
(defun cs-browse (idstring symbol)
  (interactive
   (list
    (completing-read "What tree: "
		     cscope-master-info-table nil t nil)
    (read-string "What symbol: ")))
  ;; If we start a new cscope and cscope-id is set, we need to make
  ;; sure we restore the original cscope-id after creating the new one
  (let ((cur-id cscope-id))
    (save-current-buffer
      (setq cscope-id idstring)
      (cscope-find-c-symbol symbol))
    (if cur-id (setq cscope-id cur-id))))

;; Default cscope command line args
(defvar cscope-project-opts (list "cscope" "-l" "-d"))

;; The table
(defvar cscope-master-info-table nil)

;; Varible used to store buffer name
(defvar cscope-projects-buffer nil)

;; function to remove trailing slashes
(defun cscope-project-nuke-trailing-slash (string)
  "Function to remove trailing slashes from directory names"
  (let ((len (length string))
	(slash 47))
    (if (char-equal (aref string (- len 1)) slash)
	(substring string 0 (- len 1))
      string)))

;; Function to add stuff to cscope-master-info-table
(defun cscope-new-project-disjoint
  (cscope-project-tag
   cscope-project-cscope-out
   cscope-project-source-directory)
  "Function to add new data to cscope-master-info-table.

cscope-project-cscope-out is that full path to a cscope
data file.

cscope-project-source-directory is the path to the source
code.  It is assumed that this is different from the
directory containing the cscope data."
  (interactive "sCSCOPE project tag: 
fPath to cscope data (must already exist): 
DPath to source: ")

  ;; Remove trailing slash from cscope-project-source-directory, if any
  (setq cscope-project-source-directory
	(cscope-project-nuke-trailing-slash cscope-project-source-directory))

  (if (assoc cscope-project-tag cscope-master-info-table)
      (cscope-delete-project cscope-project-tag))

  (setq cscope-master-info-table
	(cons (list cscope-project-tag
		    (append cscope-project-opts
			    (list "-f" (expand-file-name cscope-project-cscope-out)))
		    cscope-project-source-directory
		    cscope-project-source-directory)
	      cscope-master-info-table)))

;; Function to add stuff to cscope-master-info-table
;; It's assumed that the cscope data file is called cscope.out
;; and it lives with the source.
(defun cscope-new-project
  (cscope-project-tag cscope-project-directory)
  "Function to add new data to cscope-master-info-table
The cscope data is in a file cscope.out and it lives with the source."
  (interactive "sCSCOPE project tag: 
DPath to cscope data (must already be built): ")

  ;; Remove trailing slash from cscope-project-directory, if any
  (setq cscope-project-directory
	(cscope-project-nuke-trailing-slash cscope-project-directory))

  (if (assoc cscope-project-tag cscope-master-info-table)
      (cscope-delete-project cscope-project-tag))

  (setq cscope-master-info-table
	(cons (list cscope-project-tag cscope-project-opts
		    cscope-project-directory
		    cscope-project-directory) cscope-master-info-table)))

;; Function to nuke stuff from cscope-master-info-table...
(defun cscope-delete-project (cscope-project-tag)
  "Function to remove a tag from the cscope-master-info-table."
  (interactive "i")

  (if (not cscope-project-tag)
      (setq cscope-project-tag
	    (completing-read "CSCOPE project tag: "
			     cscope-master-info-table nil t nil)))

  ; Fix the list
  (if (assoc cscope-project-tag cscope-master-info-table)
      (setq cscope-master-info-table
	    (delete
	     (assoc cscope-project-tag cscope-master-info-table)
	     cscope-master-info-table))))

(defun cscope-project:cscope-bind-keys ()
  "Function to init local key bindings for cscope"
  (define-key (current-local-map) "\M-?" 'cscope-find-c-symbol)
  nil)

;; Function to be added to C mode
(defun cscope-project:c-mode-hook ()
  "Function to init cscope key bindings"
  (or (where-is-internal 'cscope-find-c-symbol (current-local-map))
      ;; Hack to prevent cscope and Barry Warsaw's new cc-mode from
      ;; clashing on \C-c \C-s
      (let ((ctrl-c-s-func (lookup-key c-mode-map "\C-c\C-s")))
	(if (or
	     (eq ctrl-c-s-func 'c-show-semantic-information)
	     (eq ctrl-c-s-func 'c-show-syntactic-information))
	    (progn
	      (define-key c-mode-map "\C-c\C-s" nil)
	      (define-key c-mode-map "\C-c\M-\C-s" ctrl-c-s-func)
	      nil)))
      (cscope-bind-keys)))

;; Function to be added to C++ mode
(defun cscope-project:c++-mode-hook ()
  "Function to init cscope key bindings"
  (or (where-is-internal 'cscope-find-c-symbol (current-local-map))
      ;; Hack to prevent cscope and Barry Warsaw's new cc-mode from
      ;; clashing on \C-c \C-s
      (if (eq (lookup-key c-mode-map "\C-c\C-s")
	      'c-show-semantic-information)
	  (progn
	    (define-key c-mode-map "\C-c\C-s" nil)
	    (define-key c-mode-map "\C-c\M-\C-s" 'c-show-semantic-information)
	    nil))
      (cscope-bind-keys)))

(defun cscope-project-read-file ()
  "Function to read the cscope-projects file"
  (interactive)
  (if (and (file-exists-p cscope-projects-file) (file-readable-p cscope-projects-file))
      (save-excursion
	(find-file cscope-projects-file)
	(setq cscope-projects-buffer (current-buffer))
	(goto-char 1)
	(setq cscope-master-info-table (read cscope-projects-buffer))
	(bury-buffer))
    (save-excursion
      (find-file cscope-projects-file)
      (setq cscope-projects-buffer (current-buffer))
      (goto-char (point-min))
      (bury-buffer))))

(defun cscope-project-write-file ()
  "Function to write out the cscope-projects file"
  (interactive)
  (save-excursion
	(set-buffer cscope-projects-buffer)
	(delete-region (point-min) (point-max))
	(insert (format "%S\n" cscope-master-info-table))
	(save-buffer)))

;; Add our hooks
(add-hook 'cscope-bind-keys-hook (function cscope-project:cscope-bind-keys))
(add-hook 'c-mode-hook (function cscope-project:c-mode-hook))
(add-hook 'c++-mode-hook (function cscope-project:c++-mode-hook))

;; Run any hooks
(run-hooks 'cscope-project-hook)

;; Load up the projects file
(cscope-project-read-file)

(provide 'cscope-project)
