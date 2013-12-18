
(setq-default autopair-skip-whitespace t)

(add-hook 'find-file-hook             (lambda () (autopair-mode t)))
(add-hook 'fundamental-mode-hook      (lambda () (autopair-mode t)))
(add-hook 'lisp-interaction-mode-hook (lambda () (autopair-mode t)))
(add-hook 'slime-repl-mode-hook       (lambda () (autopair-mode t)))
(add-hook 'minibuffer-setup-hook      (lambda () (autopair-mode t)))
