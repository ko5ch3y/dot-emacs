
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")

(setq-default ac-auto-show-menu 0)
(setq-default ac-clang-flags (list "-I/usr/include"
                                   "-I/usr/local/include"
                                   "-I/usr/include/QtCore"
                                   "-I/usr/include/QtGui"))
(setq-default ac-sources '(ac-source-abbrev
                           ac-source-dictionary
                           ac-source-functions
                           ac-source-variables
                           ac-source-symbols
                           ac-source-features
                           ac-source-yasnippet
                           ac-source-words-in-same-mode-buffers))

(setq-default ac-ignore-case t)

(add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
(add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
(add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
(add-hook 'css-mode-hook 'ac-css-mode-setup)
(add-hook 'auto-complete-mode-hook 'ac-common-setup)

(global-auto-complete-mode t)

(require 'ac-helm2)

(require 'auto-complete-clang)
(setq-default ac-clang-auto-save nil)
