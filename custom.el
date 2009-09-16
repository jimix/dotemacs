
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(asm-comment-char 35)
 '(blink-cursor-mode nil)
 '(compilation-ask-about-save nil)
 '(compilation-scroll-output t)
 '(compilation-skip-threshold 0)
 '(compilation-skip-visited t)
 '(compile-command "make ")
 '(cscope-projects-file "~/.emacs.d/cscope-projects.el")
 '(cvs-auto-remove-handled t)
 '(cvs-find-file-and-jump t)
 '(focus-follows-mouse t)
 '(fringe-mode (quote (1 . 1)) nil (fringe))
 '(grep-find-command "find . \\( -name CVS -o -name SCCS -o -name .hg -o -name .git \\) -prune -o \\! \\( -name \\*~ -o -name .\\#\\* -o -name cscope.\\* \\) -type f -print0 | xargs -0 -e grep -n ")
 '(indicate-buffer-boundaries nil)
 '(log-edit-confirm t)
 '(menu-bar-mode t)
 '(mouse-autoselect-window t)
 '(mouse-wheel-progressive-speed nil)
 '(safe-local-variable-values (quote ((backup-inhibited . t))))
 '(show-paren-mode t)
 '(ssh-directory-tracking-mode t)
 '(tool-bar-mode nil)
 '(tramp-debug-buffer t)
 '(tramp-terminal-type "tramp")
 '(truncate-partial-width-windows nil)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(vc-diff-switches "-u")
 '(visible-bell t)
 '(warning-suppress-types (quote ((undo discard-info)))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "black" :foreground "yellow" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 18 :width condensed :family "schumacher-clean"))))
 '(cursor ((t (:background "red"))))
 '(region ((((class color) (min-colors 88) (background dark)) (:background "grey40")))))
