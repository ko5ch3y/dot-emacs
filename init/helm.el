
(require 'helm-gtags)
(require 'helm-ring)
(require 'helm-match-plugin)
(require 'helm-buffers)
(require 'helm-grep)
(require 'helm-command)

(helm-mode t)

(add-hook 'helm-after-initialize-hook
          #'(lambda ()
              (with-current-buffer helm-buffer
                (visual-line-mode))))

(setq-default helm-gtags-enable-initial-pattern t)
(setq-default gtags-path-style 'relative)
(setq-default helm-su-or-sudo "sudo")
(setq-default helm-buffer-max-length nil)
