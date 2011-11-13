
(defvar my-load-path
  (list "~/.emacs.d/auto-install"
        "~/.emacs.d/site-lisp"
        "~/.emacs.d/site-lisp/anything-config"
        "~/.emacs.d/site-lisp/anything-config/extensions"
        "~/.emacs.d/site-lisp/auto-complete"
        "~/.emacs.d/site-lisp/auto-complete-clang"
        "~/.emacs.d/site-lisp/color-theme-github"
        "~/.emacs.d/site-lisp/egg"
        "~/.emacs.d/site-lisp/elscreen"
        "~/.emacs.d/site-lisp/geiser/elisp"
        "~/.emacs.d/site-lisp/org-mode/EXPERIMENTAL"
        "~/.emacs.d/site-lisp/org-mode/contrib/lisp"
        "~/.emacs.d/site-lisp/org-mode/lisp"
        "~/.emacs.d/site-lisp/vim-mode"
        "~/.emacs.d/site-lisp/yasnippet-0.6.1c"))
(setq load-path (append load-path my-load-path))


(defun my-package-setup ()
  (load "~/.emacs.d/elpa/package.el")
  (require 'package)
  (dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
                    ("elpa" . "http://tromey.com/elpa/")))
    (add-to-list 'package-archives source t))
  (package-initialize))

(defun my-auto-install-setup ()
  (require 'auto-install)
  (setq-default url-proxy-services '(("http" . "localhost:3128"))))

(defun my-tramp-setup ()
  (require 'tramp)
  (setq-default tramp-default-method "ssh"))

(defun my-misc-function-setup ()
  (defun fix-server ()
    (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function))

  (defun start-server ()
    (interactive)
    (server-mode t)
    (fix-server))

  (defun open-client-other-window ()
    (let ((server-buf (current-buffer)))
      (bury-buffer)
      (switch-to-buffer-other-window server-buf)))

  (defun compile-autoclose (buffer string)
    (cond ((string-match "finished" string)
           (bury-buffer "*compilation*")
           (replace-buffer-in-windows "*compilation*")
           (message "Build successful."))
          (t
           (message "Compilation exited abnormally: %s" string))))

  (defun comment-or-uncomment-line ()
    (interactive)
    (vim:visual-toggle-linewise)
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (vim:visual-toggle-linewise)
    (vim:motion-down :count 1))

  (defun indent-line ()
    (interactive)
    (let ((tab-always-indent t))
      (indent-for-tab-command nil)))

  (defun generate-tab-stop-list ()
    (let ((result (list)))
      (dotimes (n 10 result)
        (setq result (cons (* (+ 1 n) standard-indent) result)))
      (reverse result)))

  (defun gud-kill-yes ()
    (interactive)
    (gud-kill nil)
    (sleep-for 0.1)
    (gud-yes nil))

  (defun gud-run-yes ()
    (interactive)
    (gud-run nil)
    (sleep-for 0.1)
    (gud-yes nil))

  (defun gud-interrupt ()
    (interactive)
    (set-buffer (get-buffer "*gud*"))
    (comint-interrupt-subjob))

  (defun gud-restart ()
    (interactive)
    (let ((previous-buffer (current-buffer)))
      (switch-to-buffer-other-window "*gud*")
      (gud-interrupt)
      (sleep-for 0.1)
      (gud-run-yes)
      (switch-to-buffer-other-window previous-buffer)))

  (defun paredit-change ()
    (interactive)
    (paredit-kill)
    (vim:insert-mode))

  (defun my-lisp-mode-hook ()
    (setq standard-indent 2)
    (setq tab-stop-list (generate-tab-stop-list)))

  (defun my-scheme-mode-hook ()
    (mapc (lambda (sym)
            (put sym 'scheme-indent-function 'defun))
          (list 'call/cc 'c-lambda 'module-map))

    (define-key scheme-mode-map "\t" 'scheme-complete-or-indent)

    (make-local-variable 'eldoc-documentation-function)
    (setq eldoc-documentation-function 'scheme-get-current-symbol-info)
    (eldoc-mode))

  (defun no-junk-please-were-unixish ()
    (let ((coding-str (symbol-name buffer-file-coding-system)))
      (when (string-match "-\\(?:dos\\|mac\\)$" coding-str)
        (setq coding-str
              (concat (substring coding-str 0 (match-beginning 0)) "-unix"))
        (message "CODING: %s" coding-str)
        (set-buffer-file-coding-system (intern coding-str)) )))

  (defun now ()
    "Insert string for the current date and time ISO formatted like '2011-08-01 2:34 PM'."
    (interactive)                 ; permit invocation in minibuffer
    (insert (format-time-string "%Y-%m-%d %I:%M %p")))

  (defun time ()
    "Insert string for the current time ISO formatted like '2:34 PM'."
    (interactive)                 ; permit invocation in minibuffer
    (insert (format-time-string "%I:%M %p")))

  (defun today ()
    "Insert string for today's date nicely formatted in ISO style, e.g. 2011-08-01."
    (interactive)                 ; permit invocation in minibuffer
    (insert (format-time-string "%Y-%m-%d")))

  (defun short-date ()
    "Insert string for today's date formatted like 110801."
    (interactive)                 ; permit invocation in minibuffer
    (insert (format-time-string "%y%m%d"))))

(defun my-server-setup ()
  (setq-default server-name "terminal"))

(defun my-window-system-setup ()
  (when window-system
    ;; (add-hook 'server-switch-hook open-client-other-window)
    (global-hl-line-mode t)
    (require 'color-theme)
    (require 'color-theme-github)
    (setq server-name "gui")))

(defun my-elscreen-setup ()
  (setq-default elscreen-prefix-key "`")
  (setq-default elscreen-startup-command-line-processing nil)
  (load "elscreen" "ElScreen" t))

(defun my-org-mode-setup ()
  (require 'org-install)
  (require 'org-mw)
  (setq-default initial-major-mode 'org-mode)
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
  (add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode)))

(defun my-anything-setup ()
  (require 'anything-startup)
  (require 'anything-gtags)
  (setq-default anything-gtags-enable-initial-pattern t)
  (setq-default gtags-path-style 'relative))

(defun my-autopair-setup ()
  (require 'autopair)
  (setq-default autopair-skip-whitespace t)
  (add-hook 'autopair-mode-hook
            (lambda ()
              (define-key (cdr (car autopair-emulation-alist)) [return] nil)
              (define-key (cdr (car autopair-emulation-alist)) (kbd "RET") nil)))
  ;; (autopair-global-mode t)
  (add-hook 'c-mode-common-hook (lambda () (autopair-mode t))))

(defun my-paredit-setup ()
  (require 'paredit)
  (add-hook 'find-file-hook             (lambda () (if (not (eq major-mode 'org-mode))
                                                       (paredit-mode t))))
  (add-hook 'fundamental-mode-hook      (lambda () (paredit-mode t)))
  (add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode t)))
  (add-hook 'slime-repl-mode-hook       (lambda () (paredit-mode t)))
  (add-hook 'minibuffer-setup-hook      (lambda () (paredit-mode t))))

(defun my-auto-complete-setup ()
  (require 'auto-complete-config)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
  (ac-config-default)
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
  (define-key ac-completing-map "\t" 'ac-complete)
  (setq-default ac-ignore-case t)
  (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
  (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
  (add-hook 'css-mode-hook 'ac-css-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))

(defun my-ac-anything2-setup ()
  (require 'ac-anything2)
  (define-key ac-complete-mode-map "\M-s" 'ac-anything2))

(defun my-auto-complete-clang-setup ()
  (require 'auto-complete-clang)
  (setq-default ac-clang-auto-save nil))

(defun my-yasnippet-setup ()
  (require 'yasnippet)
  (yas/initialize)
  (yas/load-directory "~/.emacs.d/site-lisp/yasnippet-0.6.1c/snippets")
  ;; (setq-default yas/prompt-functions '(yas/dropdown-prompt))
  (yas/global-mode t))

(defun my-egg-setup ()
  (require 'egg)
  (setq-default egg-buffer-hide-sub-blocks-on-start nil)
  (setq-default egg-enable-tooltip t)
  (setq-default egg-refresh-index-in-backround t))

(defun my-vim-mode-setup ()
  (require 'vim)
  (require 'vim-elscreen)
  (vim-mode 1)
  (setq-default vim:default-initial-mode 'normal)
  (setq-default vim:initial-modes
                '((debugger-mode . window)
                  (compilation-mode . normal)
                  (grep-mode . normal)
                  (gud-mode . normal)
                  (sldb-mode . window)
                  (slime-repl-mode . window)
                  (reftex-select-bib-mode . window)
                  (completion-list-mode . normal)
                  (help-mode . normal)
                  (Info-mode . motion)))

  (vim:defcmd vim:cmd-delete-bwd-word (count register)
    "Deletes the next count characters."
    (vim:cmd-delete :motion (vim:motion-bwd-word :count 1)
                    :register register))

  (vim:defcmd vim:cmd-make (nonrepeatable argument)
    "Executes compile or recompile."
    (let ((previous-buffer (current-buffer)))
      (if argument
          ((lambda ()
             (compile (concat "make " argument))
             (switch-to-buffer-other-window "*compilation*")
             (vim:motion-go-to-first-non-blank-end)
             (switch-to-buffer-other-window previous-buffer)))
        ((lambda ()
           (recompile)
           (switch-to-buffer-other-window "*compilation*")
           (vim:motion-go-to-first-non-blank-end)
           (switch-to-buffer-other-window previous-buffer))))))

  (vim:defcmd vim:cmd-next-error (nonrepeatable count)
    "Moves to the `count'th next error."
    (next-error count))

  (vim:defcmd vim:cmd-prev-error (nonrepeatable count)
    "Moves to the `count'th previous error."
    (next-error (- (or count 1)))))

(defun my-scheme-complete-setup ()
  (autoload 'scheme-smart-complete "scheme-complete" nil t)
  (autoload 'scheme-get-current-symbol-info "scheme-complete" nil t)
  (setq-default scheme-indent-function 'scheme-smart-indent-function))

(defun my-tab-and-indent-setup ()
  (setq-default indent-line-function 'indent-according-to-mode)
  (setq-default indent-tabs-mode nil)
  (setq-default standard-indent 4)
  (setq-default tab-width 4)
  ;; (add-hook 'completion-at-point-functions 'hippie-expand nil)
  (setq-default tab-always-indent t)
  (setq-default tab-stop-list (generate-tab-stop-list)))

(defun my-gambit-setup ()
  (autoload 'gambit-inferior-mode "gambit" "Hook Gambit mode into cmuscheme.")
  (autoload 'gambit-mode "gambit" "Hook Gambit mode into scheme.")
  (add-hook 'inferior-scheme-mode-hook (function gambit-inferior-mode))
  (add-hook 'scheme-mode-hook (function gambit-mode))
  (setq-default scheme-program-name "gsi -:d-"))

(defun my-geiser-setup ()
  (require 'geiser)
  (setq-default geiser-active-implementations '(guile)))

(defun my-gud-setup ()
  (require 'gud)
  (gud-def gud-kill "k" nil)
  (gud-def gud-yes "y" nil))

(defun my-haskell-mode-setup ()
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (add-hook 'haskell-mode-hook 'my-haskell-mode-hook))

(defun my-cc-mode-setup ()
  (require 'cc-mode)
  (setq-default c-default-style
                '((java-mode . "java")
                  (other . "linux"))
                c-basic-offset 4))

(defun my-qi-mode-setup ()
  (require 'qi-mode)
  (add-to-list 'auto-mode-alist '("\\.qml$" . js-mode)))

(defun my-whitespace-setup ()
  (require 'whitespace)
  (global-whitespace-mode t)
  (setq-default whitespace-style '(face tabs tab-mark)))

(defun my-misc-setup ()
  (require 'undo-tree)
  (winner-mode t)
  (setq-default compilation-finish-functions 'compile-autoclose)
  (setq-default read-file-name-completion-ignore-case t)
  (setq-default backup-inhibited t)
  (setq-default auto-save-default nil)
  (setq-default inhibit-read-only t)
  (global-auto-revert-mode 1)
  (setq-default scroll-margin 10)
  (add-hook 'term-mode-hook '(lambda () (setq scroll-margin 0)))
  (setq-default scroll-step 1)
  (setq-default require-final-newline t)
  (setq-default next-line-add-newlines nil)
  (setq-default visible-bell t)
  (fset 'yes-or-no-p 'y-or-n-p)
  (setq-default show-paren-delay 0)
  (setq-default show-paren-style 'mixed)
  (setq-default grep-command "grep --exclude-from=$HOME/.grepignore -niHIR -e ")
  (setq initial-scratch-message "")
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (global-linum-mode t)
  (setq-default fill-column 80)
  (add-hook 'find-file-hooks 'no-junk-please-were-unixish)
  ;; (mapcar (lambda (mode-hook)
            ;; (add-hook mode-hook 'flyspell-prog-mode))
          ;; '(c-mode-common-hook
            ;; emacs-lisp-mode-hook
            ;; js-mode-hook
            ;; asm-mode-hook
            ;; egg-mode-hook
            ;; java-mode-hook))
  (add-hook 'find-file-hook       'flyspell-mode-off)
  (add-hook 'lisp-mode-hook       'my-lisp-mode-hook)
  (add-hook 'emacs-lisp-mode-hook 'my-lisp-mode-hook)
  (add-hook 'scheme-mode-hook 'my-lisp-mode-hook)
  (add-hook 'scheme-mode-hook 'my-scheme-mode-hook)
  (set-frame-font "Monospace 10")
  ;; (set-face-attribute 'default nil :weight 'bold)
  ;; (require 'rainbow-delimiters)
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(egg-diff-add ((((class color) (background light)) (:foreground "#009926"))))
   '(egg-diff-del ((((class color) (background light)) (:foreground "#DD1144"))))

   '(hl-line ((t (:background "#F3F3FF"))))
   '(vline ((t (:background "#F5F5FF"))))

   '(rainbow-delimiters-depth-1-face ((t (:foreground "red"))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "violet"))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "orange"))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "purple"))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "brown"))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "darkblue"))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "green"))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "blue"))))
   '(rainbow-delimiters-depth-9-face ((t (:foreground "cyan")))))

  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(ansi-color-names-vector ["#000000" "#DD1144" "#009926" "#990000" "#445588" "#990073" "#0086B3" "#999988"])
   '(comint-buffer-maximum-size 100000)
   '(comint-completion-addsuffix t)
   '(comint-get-old-input (lambda nil "") t)
   '(comint-input-ignoredups t)
   '(comint-input-ring-size 5000)
   '(comint-move-point-for-output nil)
   '(comint-prompt-read-only nil)
   '(comint-scroll-show-maximum-output t)
   '(comint-scroll-to-bottom-on-input t)
   '(egg-buffer-hide-help-on-start nil)
   '(egg-buffer-hide-section-type-on-start (quote ((egg-status-buffer-mode . :diff) (egg-diff-buffer-mode . :diff))))
   '(egg-buffer-hide-sub-blocks-on-start nil)
   '(egg-quit-window-actions (quote ((egg-status-buffer-mode restore-windows) (egg-log-buffer-mode restore-windows) (egg-commit-buffer-mode restore-windows) (egg-reflog-buffer-mode restore-windows) (egg-diff-buffer-mode restore-windows) (egg-file-log-buffer-mode restore-windows))))
   '(protect-buffer-bury-p nil)))

(defun my-eldoc-setup ()
  (require 'eldoc)
  (require 'c-eldoc)
  (setq-default c-eldoc-includes "`pkg-config QtCore QtGui --cflags` -I./ -I../ -I/usr/include")
  (add-hook 'c-mode-common-hook 'c-turn-on-eldoc-mode))


(defun my-anything-map-setup ()
  (define-key anything-map "\M-e" 'anything-execute-persistent-action)
  (define-key anything-map "\M-h" 'paredit-backward-delete)
  (define-key anything-map "\M-H" 'paredit-backward-delete)
  (define-key anything-map "\M-n" 'anything-next-line)
  (define-key anything-map "\C-n" 'next-history-element)
  (define-key anything-map "\M-o" 'anything-ff-run-switch-other-window)
  (define-key anything-map "\M-p" 'anything-previous-line)
  (define-key anything-map "\C-p" 'previous-history-element)
  (define-key anything-map "\M-r" 'anything-buffer-revert-persistent)
  (define-key anything-map "\M-s" 'anything-buffer-save-persistent)
  (define-key anything-map "\M-w" 'paredit-backward-kill-word)

  (define-key anything-find-files-map "\M-e" 'anything-execute-persistent-action)
  (define-key anything-find-files-map "\M-h" 'paredit-backward-delete)
  (define-key anything-find-files-map "\M-H" 'paredit-backward-delete)
  (define-key anything-find-files-map "\M-l" 'anything-ff-run-switch-to-history)
  (define-key anything-find-files-map "\M-n" 'anything-next-line)
  (define-key anything-find-files-map "\C-n" 'next-history-element)
  (define-key anything-find-files-map "\M-o" 'anything-ff-run-switch-other-window)
  (define-key anything-find-files-map "\M-p" 'anything-previous-line)
  (define-key anything-find-files-map "\C-p" 'previous-history-element)
  (define-key anything-find-files-map "\M-r" 'anything-select-3rd-action)
  (define-key anything-find-files-map "\M-u" 'anything-find-files-down-one-level)
  (define-key anything-find-files-map "\M-w" 'paredit-backward-kill-word))

(defun my-minibuffer-map-setup ()
  (define-key minibuffer-local-map "\C-p" 'previous-history-element)
  (define-key minibuffer-local-map "\C-n" 'next-history-element)
  (define-key minibuffer-local-map "\M-p" 'anything-previous-line)
  (define-key minibuffer-local-map "\M-n" 'anything-next-line)
  (define-key minibuffer-local-map "\M-l" 'anything-minibuffer-history)
  (define-key minibuffer-local-map "\M-h" 'paredit-backward-delete)
  (define-key minibuffer-local-map "\M-H" 'paredit-backward-delete)
  (define-key minibuffer-local-map "\M-w" 'paredit-backward-kill-word))

(defvar my-tab-map (make-sparse-keymap))
(defun my-tab-map-setup ()
  (define-key my-tab-map "\M-c" 'vim:cmd-tab-new)
  (define-key my-tab-map "\M-d" 'vim:cmd-tab-close)
  (define-key my-tab-map "\M-o" 'vim:cmd-tab-close-other)
  (define-key my-tab-map "\M-n" 'vim:cmd-tab-next)
  (define-key my-tab-map "\M-p" 'vim:cmd-tab-previous)
  (define-key my-tab-map "\M-t" 'elscreen-toggle))

(defvar my-haskell-map (make-sparse-keymap))
(defun my-haskell-map-setup ()
  (define-key my-haskell-map "d" 'inferior-haskell-find-definition)
  (define-key my-haskell-map "i" 'inferior-haskell-info)
  (define-key my-haskell-map "l" 'inferior-haskell-load-file)
  (define-key my-haskell-map "t" 'inferior-haskell-type))

(defvar my-gud-map (make-sparse-keymap))
(defun my-gud-map-setup ()
  (define-key my-gud-map "\M-b" 'gud-break)
  (define-key my-gud-map "\M-c" 'gud-cont)
  (add-hook 'scheme-mode-hook (lambda () (define-key my-gud-map "\M-c" 'gambit-continue)))
  (define-key my-gud-map "\M-f" 'gud-finish)
  (define-key my-gud-map "\M-g" (lambda () (interactive) (switch-to-buffer "*gud*")))
  (define-key my-gud-map "\M-i" 'gud-interrupt)
  (define-key my-gud-map "\M-k" 'gud-kill-yes)
  (add-hook 'scheme-mode-hook (lambda () (define-key my-gud-map "\M-l" 'gambit-leap-continuation)))
  (define-key my-gud-map "\M-m" 'gud-restart)
  (define-key my-gud-map "\M-n" 'gud-next)
  (add-hook 'scheme-mode-hook (lambda () (define-key my-gud-map "\M-n" 'gambit-crawl-backtrace-newer)))
  (add-hook 'scheme-mode-hook (lambda () (define-key my-gud-map "\M-o" 'gambit-crawl-backtrace-older)))
  (define-key my-gud-map "\M-p" 'gud-print)
  (define-key my-gud-map "\M-r" 'gud-remove)
  (define-key my-gud-map "\M-s" 'gud-step)
  (add-hook 'scheme-mode-hook (lambda () (define-key my-gud-map "\M-s" 'gambit-step-continuation)))
  (define-key my-gud-map "\M-t" 'gud-tbreak)
  (define-key my-gud-map "\M-u" 'gud-until))
;; scheme-send-definition
;; scheme-send-region
;; scheme-send-last-sexp
;; scheme-load-file
;; switch-to-scheme
;; scheme-expand-current-form
;; scheme-send-definition-and-go
;; scheme-send-region-and-go
;; scheme-compile-file
;; scheme-compile-definition

(defvar my-egg-map (make-sparse-keymap))
(defun my-egg-map-setup ()
  (define-key my-egg-map "c" 'egg-commit-log-edit)
  (define-key my-egg-map "g" 'egg-next-action)
  (define-key my-egg-map "l" 'egg-log)
  (define-key my-egg-map "s" 'egg-status))

(defvar my-tag-map (make-sparse-keymap))
(defun my-tag-map-setup ()
  (define-key my-tag-map "s" 'anything-gtags-select)
  (define-key my-tag-map "t" 'anything-gtags-resume))

(defvar my-geiser-map (make-sparse-keymap))
(defun my-geiser-map-setup ()
  (define-key my-geiser-map "b" 'geiser-load-current-buffer)
  (define-key my-geiser-map "d" 'geiser-eval-definition)
  (define-key my-geiser-map "D" 'geiser-eval-definition-and-go)
  (define-key my-geiser-map "l" 'geiser-eval-last-sexp)
  (define-key my-geiser-map "r" 'geiser-eval-region)
  (define-key my-geiser-map "R" 'geiser-eval-region-and-go)
  (define-key my-geiser-map "s" 'geiser-mode-switch-to-repl)
  (define-key my-geiser-map "S" 'geiser-mode-switch-to-repl-and-enter))

(defun my-vim-map-setup ()
  (vim:nmap "C"    'paredit-change)
  (vim:nmap "D"    'paredit-kill)
  (vim:nmap "\M-d" 'paredit-forward-delete)
  (add-hook 'shell-mode-hook (lambda ()
                               (vim:local-imap "\C-d" 'comint-send-eof)
                               (vim:local-nmap "\C-d" 'comint-send-eof)))
  (vim:nmap "\M-g"  my-gud-map)
  (vim:nmap "H"    'windmove-left)
  (vim:imap "\M-h" 'paredit-backward-delete)
  (vim:nmap "\M-h" 'paredit-backward)
  (vim:imap "\M-H" 'paredit-backward-delete)
  (vim:nmap "J"    'windmove-down)
  (vim:nmap "\M-j" 'paredit-forward-down)
  (vim:nmap "\M-J" 'paredit-backward-down)
  (vim:nmap "K"    'windmove-up)
  (vim:nmap "\M-k" 'paredit-forward-up)
  (vim:nmap "\M-K" 'paredit-backward-up)
  (vim:nmap "L"    'windmove-right)
  (vim:nmap "\M-l" 'paredit-forward)
  (vim:nmap "\M-n" 'vim:cmd-next-error)
  (vim:nmap "\M-N" 'vim:cmd-next-jump)
  (vim:nmap "\M-p" 'vim:cmd-prev-error)
  (vim:nmap "\M-P" 'vim:cmd-prev-jump)
  (vim:imap "\M-s"  nil)
  (vim:nmap "Tc"   'transpose-chars)
  (vim:nmap "Tl"   'transpose-lines)
  (vim:nmap "Tp"   'transpose-paragraphs)
  (vim:nmap "Ts"   'transpose-sentences)
  (vim:nmap "Tw"   'transpose-words)
  (vim:nmap "\M-t"  my-tab-map)
  (vim:imap "\M-w" 'paredit-backward-kill-word)
  (add-hook 'term-mode-hook (lambda ()
                              (vim:local-imap "\M-x" 'anything-execute-extended-command)
                              (vim:local-nmap "\M-x" 'anything-execute-extended-command)))
  (vim:nmap "Y"    "y$")
  (vim:imap "\C-z" 'vim:activate-emacs-mode)
  (vim:nmap "za" 'align-current)
  (vim:vmap "za" 'align)
  (vim:nmap "zA" 'align-regexp)
  (vim:vmap "zA" 'align-regexp)
  (vim:nmap "zc" 'vim:scroll-line-to-center)
  (vim:nmap "zd" 'kill-this-buffer)
  (vim:nmap "ze" 'anything-find-files)
  (vim:nmap "zg" 'grep)
  (vim:nmap "zG"  my-egg-map)
  (vim:nmap "zh"   'split-window-horizontally)
  (vim:nmap "zi" 'anything-imenu)
  (vim:nmap "zj" 'paredit-join-sexps)
  (vim:nmap "zk"   'kill-compilation)
  (vim:nmap "zl"   'vim:cmd-nohighlight)
  (vim:nmap "zm" 'vim:cmd-make)
  (vim:nmap "zo" 'occur)
  (vim:nmap "zO" 'ff-find-other-file)
  (vim:nmap "zq"   'save-buffers-kill-terminal)
  (vim:nmap "zr" 'anything)
  (vim:nmap "zs" 'paredit-splice-sexp)
  (vim:nmap "zS" 'paredit-split-sexp)
  (vim:nmap "zt"  my-tag-map)
  (vim:nmap "zv"   'split-window-vertically)
  (vim:nmap "zw" 'save-buffer)
  (vim:nmap "zx"   'delete-window)
  (vim:nmap "zz"    my-geiser-map)
  (vim:vmap "zz"    my-geiser-map)
  (vim:imap "\M-z" 'vim:insert-mode-exit)
  (vim:nmap "\M-z" 'vim:insert-mode-exit)
  (vim:vmap "\M-z" 'vim:visual-mode-exit)
  (vim:imap "\M-1" 'paredit-backward-slurp-sexp)
  (vim:nmap "\M-1" 'paredit-backward-slurp-sexp)
  (vim:imap "\M-2" 'paredit-forward-slurp-sexp)
  (vim:nmap "\M-2" 'paredit-forward-slurp-sexp)
  ;; (vim:imap "{"    'paredit-open-curly)
  (vim:imap "\M-{" 'paredit-backward-barf-sexp)
  (vim:nmap "\M-{" 'paredit-backward-barf-sexp)
  ;; (vim:imap "}"    'paredit-close-curly)
  (vim:imap "\M-}" 'paredit-forward-barf-sexp)
  (vim:nmap "\M-}" 'paredit-forward-barf-sexp)
  ;; (vim:imap "["    'paredit-open-square)
  ;; (vim:imap "]"    'paredit-close-square)
  (vim:imap "\M-[" 'paredit-wrap-square)
  (vim:nmap "\M-[" 'paredit-wrap-square)
  ;; (vim:imap "<"    'paredit-open-angled)
  (vim:imap "\M-<" 'paredit-wrap-angled)
  (vim:nmap "\M-<" 'paredit-wrap-angled)
  ;; (vim:imap ">"    'paredit-close-angled)
  (vim:imap "\M- " 'yas/expand)
  (vim:nmap ";"    'vim:ex-read-command)
  (vim:vmap ";"    'vim:ex-read-command)
  (vim:imap "`"    'self-insert-command)
  (vim:nmap "-"    'comment-or-uncomment-line)
  (vim:vmap "-"    'comment-or-uncomment-region)
  (vim:nmap "_"    'paredit-comment-dwim)

  (vim:omap "\t" nil)
  (vim:vmap [tab] 'vim:cmd-indent)
  (vim:imap [C-tab] 'tab-to-tab-stop)
  (vim:imap (kbd "RET") 'newline-and-indent)
  (vim:nmap (kbd "RET") 'newline-and-indent)

  (vim:emap "make" 'vim:cmd-make)
  (vim:emap "m"    "make"))

(defun my-org-mode-vim-map-hook ()
  (vim:local-nmap "\M-b" 'org-backward-same-level)
  (vim:local-nmap "C"    "c$")
  (vim:local-nmap "D"    "d$")
  (vim:local-nmap "\M-d" 'vim:cmd-delete-char)
  (vim:local-nmap "\M-f" 'org-forward-same-level)
  (vim:local-imap "\M-h" 'delete-backward-char)
  (vim:local-nmap "\M-h" 'org-metaleft)
  (vim:local-imap "\M-H" 'delete-backward-char)
  (vim:local-nmap "\M-j" 'org-metadown)
  (vim:local-nmap "\M-k" 'org-metaup)
  (vim:local-nmap "\M-l" 'org-metaright)
  (vim:local-nmap "\M-n" 'outline-next-visible-heading)
  (vim:local-nmap "\M-p" 'outline-previous-visible-heading)
  (vim:local-nmap "\M-q" 'fill-paragraph)
  (vim:local-nmap "\M-u" 'outline-up-heading)
  (vim:local-imap "\M-w" 'vim:cmd-delete-bwd-word)
  ;; (vim:local-imap "{" 'self-insert-command)
  ;; (vim:local-imap "}" 'self-insert-command)
  ;; (vim:local-imap "[" 'self-insert-command)
  ;; (vim:local-imap "]" 'self-insert-command)
  ;; (vim:local-imap "<" 'self-insert-command)
  ;; (vim:local-imap ">" 'self-insert-command)
  (vim:local-imap [backspace] 'backward-delete-char-untabify)
  (vim:local-imap "\"" 'self-insert-command)
  (vim:local-nmap [(tab)]  'org-cycle))

(defun my-org-mode-vim-map-setup ()
  (add-hook 'org-mode-hook 'my-org-mode-vim-map-hook))

(defun my-misc-map-setup ()
  (defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")
  (define-minor-mode my-keys-minor-mode
    "A minor mode so that my key settings override annoying major modes."
    t "" 'my-keys-minor-mode-map)
  (my-keys-minor-mode 1)

  (define-key my-keys-minor-mode-map (kbd "C-w") 'ido-delete-backward-word-updir)
  (define-key my-keys-minor-mode-map (kbd "M-z") 'universal-argument)
  (define-key paredit-mode-map "\\" nil)
  (define-key key-translation-map [?\C-h] [?\C-?])
  (define-key key-translation-map [?\C-\S-h] [?\C-?])
  (define-key read-expression-map [(tab)] 'hippie-expand))


(my-package-setup)
(my-misc-function-setup)

(my-ac-anything2-setup)
(my-anything-setup)
(my-auto-complete-clang-setup)
(my-auto-complete-setup)
(my-auto-install-setup)
(my-autopair-setup)
(my-cc-mode-setup)
(my-egg-setup)
(my-eldoc-setup)
(my-elscreen-setup)
(my-gambit-setup)
(my-geiser-setup)
(my-gud-setup)
(my-haskell-mode-setup)
(my-misc-setup)
(my-org-mode-setup)
(my-paredit-setup)
(my-qi-mode-setup)
(my-scheme-complete-setup)
(my-server-setup)
(my-tab-and-indent-setup)
(my-tramp-setup)
(my-vim-mode-setup)
(my-whitespace-setup)
(my-window-system-setup)
(my-yasnippet-setup)

(my-anything-map-setup)
(my-egg-map-setup)
(my-geiser-map-setup)
(my-gud-map-setup)
(my-haskell-map-setup)
(my-minibuffer-map-setup)
(my-misc-map-setup)
(my-tab-map-setup)
(my-tag-map-setup)
(my-vim-map-setup)
(my-org-mode-vim-map-setup)
