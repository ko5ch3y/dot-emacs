
(evil-mode 1)
(add-hook 'find-file-hook '(lambda ()
                             (setq evil-shift-width standard-indent)))

(evil-set-initial-state 'completion-list-mode 'emacs)
(evil-set-initial-state 'term-mode 'emacs)
(evil-set-initial-state 'occur-mode 'normal)
(evil-set-initial-state 'compilation-mode 'normal)
(evil-set-initial-state 'gdb-locals-mode 'normal)
(evil-set-initial-state 'gdb-breakpoints-mode 'normal)
(evil-set-initial-state 'gdb-frames-mode 'normal)
