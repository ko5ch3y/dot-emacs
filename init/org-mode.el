
(require 'org)
(setq-default initial-major-mode 'org-mode)
(add-hook 'find-file-hook '(lambda () (setq standard-indent 2)))
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))
