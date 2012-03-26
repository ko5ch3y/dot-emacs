
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
        "~/.emacs.d/site-lisp/evil"
        "~/.emacs.d/site-lisp/geiser/elisp"
        "~/.emacs.d/site-lisp/monky"
        "~/.emacs.d/site-lisp/org-mode/EXPERIMENTAL"
        "~/.emacs.d/site-lisp/org-mode/contrib/lisp"
        "~/.emacs.d/site-lisp/org-mode/lisp"
        "~/.emacs.d/site-lisp/yasnippet-0.6.1c"))
(setq load-path (append load-path my-load-path))


(defun my-monky-setup ()
  (require 'evil)
  (evil-mode t))

(defun my-monky-setup ()
  (require 'monky)
  (setq-default monky-process-type 'cmdserver)
  (setq-default monky-log-cutoff-length 10)

  (set-face-attribute 'monky-diff-add nil :foreground "#009926")
  (set-face-attribute 'monky-diff-del nil :foreground "#DD1144")
  (set-face-attribute 'monky-log-sha1 nil :foreground "#0086B3"))

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

  (setq-default vc-ignore-dir-regexp
                (format "\\(%s\\)\\|\\(%s\\)"
                        vc-ignore-dir-regexp
                        tramp-file-name-regexp))

  (setq-default tramp-default-method "ssh"))

(defun my-misc-function-setup ()
  (defun evil-map (state key def &optional keymap)
    (if (eq keymap nil)
        (case state
          ('normal (define-key evil-normal-state-map key def))
          ('insert (define-key evil-insert-state-map key def))
          ('operator (define-key evil-operator-state-map key def))
          ('visual (define-key evil-visual-state-map key def))
          ('replace (define-key evil-replace-state-map key def))
          ('motion (define-key evil-motion-state-map key def)))
      (evil-define-key state keymap key def)))

  (defun evil-nmap (key def &optional keymap) (evil-map 'normal key def keymap))
  (defun evil-imap (key def &optional keymap) (evil-map 'insert key def keymap))
  (defun evil-omap (key def &optional keymap) (evil-map 'operator key def keymap))
  (defun evil-vmap (key def &optional keymap) (evil-map 'visual key def keymap))
  (defun evil-rmap (key def &optional keymap) (evil-map 'replace key def keymap))
  (defun evil-mmap (key def &optional keymap) (evil-map 'motion key def keymap))

  (defun make (argument)
    (interactive "Mmake: ")
    (let ((previous-buffer (current-buffer)))
      ((lambda ()
         (compile (concat "make " argument))
         (switch-to-buffer-other-window "*compilation*")
         (evil-goto-line)
         (switch-to-buffer-other-window previous-buffer)))))

  (defun remake ()
    (interactive)
    (let ((previous-buffer (current-buffer)))
      ((lambda ()
         (recompile)
         (switch-to-buffer-other-window "*compilation*")
         (evil-goto-line)
         (switch-to-buffer-other-window previous-buffer)))))

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
    (evil-visual-line)
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (evil-exit-visual-state)
    (evil-next-line))

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
    (evil-insert-state))

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
    (set-face-attribute 'hl-line nil :background "#F3F3FF")
    ;; (set-face-attribute 'vline nil :background "#F5F5FF")
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
  (add-hook 'find-file-hook '(lambda () (setq standard-indent 2)))
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
  (add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode)))

(defun my-anything-setup ()
  (require 'anything-config)
  (require 'anything-gtags)

  (ac-mode t)

  (add-hook 'anything-after-initialize-hook
            #'(lambda ()
                (with-current-buffer anything-buffer
                  (visual-line-mode))))

  (setq-default anything-gtags-enable-initial-pattern t)
  (setq-default gtags-path-style 'relative)
  (setq-default anything-su-or-sudo "sudo"))

(defun my-autopair-setup ()
  (require 'autopair)
  (require 'auto-pair+)
  (setq-default autopair-skip-whitespace t)
  (add-hook 'autopair-mode-hook
            (lambda ()
              (define-key (cdr (car autopair-emulation-alist)) [return] nil)
              (define-key (cdr (car autopair-emulation-alist)) (kbd "RET") nil)))
  (add-hook 'find-file-hook             (lambda () (autopair-mode t)))
  (add-hook 'fundamental-mode-hook      (lambda () (autopair-mode t)))
  (add-hook 'lisp-interaction-mode-hook (lambda () (autopair-mode t)))
  (add-hook 'slime-repl-mode-hook       (lambda () (autopair-mode t)))
  (add-hook 'minibuffer-setup-hook      (lambda () (autopair-mode t))))

(defun my-paredit-setup ()
  (require 'paredit))

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
  (setq-default egg-refresh-index-in-backround t)
  (setq-default egg-buffer-hide-help-on-start nil)
  (setq-default egg-buffer-hide-section-type-on-start '((egg-status-buffer-mode . :diff)
                                                        (egg-diff-buffer-mode . :diff)))
  (setq-default egg-buffer-hide-sub-blocks-on-start nil)
  (setq-default egg-quit-window-actions '((egg-status-buffer-mode restore-windows)
                                          (egg-log-buffer-mode restore-windows)
                                          (egg-commit-buffer-mode restore-windows)
                                          (egg-reflog-buffer-mode restore-windows)
                                          (egg-diff-buffer-mode restore-windows)
                                          (egg-file-log-buffer-mode restore-windows)))

  (set-face-attribute 'egg-diff-add nil :foreground "#009926")
  (set-face-attribute 'egg-diff-del nil :foreground "#DD1144"))

(defun my-evil-mode-setup ()
  (require 'evil)
  (evil-mode 1)
  (add-hook 'find-file-hook '(lambda ()
                               (setq evil-shift-width standard-indent)))

  (evil-set-initial-state 'completion-list-mode 'emacs)
  (evil-set-initial-state 'occur-mode 'normal)
  (evil-set-initial-state 'compilation-mode 'normal)
  (evil-set-initial-state 'gdb-locals-mode 'normal)
  (evil-set-initial-state 'gdb-breakpoints-mode 'normal)
  (evil-set-initial-state 'gdb-frames-mode 'normal))

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
  (require 'gdb-ui)
  (gud-def gud-kill "k" nil)
  (gud-def gud-yes "y" nil)
  (setq-default gdb-many-windows t)
  (setq-default gdb-use-separate-io-buffer nil)
  (setq-default gdb-max-frames 100)

  (add-hook 'change-major-mode-hook
            (lambda ()
              (if (not (eq "gud-mode" major-mode))
                  (if (get-buffer "*gud*")
                      (bury-buffer "*gud*")))))

  (defun gdb-restore-windows ()
    "Restore the basic arrangement of windows used by gdba.
This arrangement depends on the value of `gdb-many-windows'."
    (interactive)
    (pop-to-buffer gud-comint-buffer)	;Select the right window and frame.
    (delete-other-windows)
    (if gdb-many-windows
        (gdb-setup-windows)
      (when (or gud-last-last-frame gdb-show-main)
        (split-window-horizontally)
        (switch-to-buffer
         (if gud-last-last-frame
             (gud-find-file (car gud-last-last-frame))
           (gud-find-file gdb-main-file)))
        (setq gdb-source-window (selected-window))
        (other-window 1)))))


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

(defun my-rainbow-delimiters-setup ()
  (require 'rainbow-delimiters)
  (set-face-attribute 'rainbow-delimiters-depth-1-face nil :foreground "red")
  (set-face-attribute 'rainbow-delimiters-depth-2-face nil :foreground "violet")
  (set-face-attribute 'rainbow-delimiters-depth-3-face nil :foreground "orange")
  (set-face-attribute 'rainbow-delimiters-depth-4-face nil :foreground "purple")
  (set-face-attribute 'rainbow-delimiters-depth-5-face nil :foreground "brown")
  (set-face-attribute 'rainbow-delimiters-depth-6-face nil :foreground "darkblue")
  (set-face-attribute 'rainbow-delimiters-depth-7-face nil :foreground "green")
  (set-face-attribute 'rainbow-delimiters-depth-8-face nil :foreground "blue")
  (set-face-attribute 'rainbow-delimiters-depth-9-face nil :foreground "cyan"))

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
  (setq-default show-paren-style 'expression)
  (setq-default grep-command "grep --exclude-from=$HOME/.grepignore -niHIR -e ")
  (setq initial-scratch-message "")
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (add-hook 'diff-mode-hook '(lambda () (remove-hook 'before-save-hook 'delete-trailing-whitespace)))
  (global-linum-mode t)
  (setq-default fill-column 80)
  (setq-default truncate-lines t)
  (add-hook 'find-file-hook       'no-junk-please-were-unixish)
  (add-hook 'find-file-hook       'flyspell-mode-off)
  (add-hook 'lisp-mode-hook       'my-lisp-mode-hook)
  (add-hook 'emacs-lisp-mode-hook 'my-lisp-mode-hook)
  (add-hook 'scheme-mode-hook     'my-lisp-mode-hook)
  (add-hook 'scheme-mode-hook     'my-scheme-mode-hook)
  (set-frame-font "Monospace 10")
  (add-to-list 'auto-mode-alist '("SConscript" . python-mode))
  (add-to-list 'auto-mode-alist '("SConstruct" . python-mode))

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
   '(protect-buffer-bury-p nil)))

(defun my-eldoc-setup ()
  (require 'eldoc)
  (require 'c-eldoc)
  (setq-default c-eldoc-includes "`pkg-config QtCore QtGui --cflags` -I./ -I../ -I/usr/include")
  (add-hook 'c-mode-common-hook 'c-turn-on-eldoc-mode))

(defun my-eshell-setup ()
  (require 'eshell)
  (require 'em-smart)

  (defun my-eshell-prompt-function ()
    (let ((host (car (split-string (system-name) "[.]")))
          (prompt (if (= (user-uid) 0) "# " "> "))
          (path (abbreviate-file-name (eshell/pwd)))
          (fg-black   "\e[30m")
          (fg-red     "\e[31m")
          (fg-green   "\e[32m")
          (fg-yellow  "\e[33m")
          (fg-blue    "\e[34m")
          (fg-magenta "\e[35m")
          (fg-cyan    "\e[36m")
          (fg-white   "\e[37m")
          (bg-black   "\e[40m")
          (bg-red     "\e[41m")
          (bg-green   "\e[42m")
          (bg-yellow  "\e[43m")
          (bg-blue    "\e[44m")
          (bg-magenta "\e[45m")
          (bg-cyan    "\e[46m")
          (bg-white   "\e[47m")
          (normal     "\e[0m")
          (bold       "\e[1m")
          (underlined "\e[4m")
          (blinking   "\e[5m")
          (reverse    "\e[7m"))
      (concat normal fg-black "at " fg-red host fg-black " in " fg-cyan path
              "\n"
              fg-green prompt fg-black)))

  (add-hook 'eshell-mode-hook
            #'(lambda ()
                (define-key eshell-mode-map
                  [remap pcomplete] 'anything-esh-pcomplete)))

  (add-hook 'change-major-mode-hook
            (lambda ()
              (if (not (eq "eshell-mode" major-mode))
                  (if (get-buffer "*eshell*")
                      (bury-buffer "*eshell*")))))

  (setq-default eshell-prompt-function 'my-eshell-prompt-function)
  (setq-default eshell-where-to-jump 'begin)
  (setq-default eshell-review-quick-commands nil)
  (setq-default eshell-smart-space-goes-to-end t)

  (add-hook 'shell-mode-hook '(lambda () (setq scroll-margin 0))))

;; Adapted from http://snarfed.org/why_i_run_shells_inside_emacs
(defun my-shell-setup ()
  (require 'tramp)
  (require 'protbuf)

  (defvar my-local-shells '("*shell0*" ))
  (defvar my-remote-shells '("*pltonia*" ))
  (defvar my-shells (append my-local-shells my-remote-shells))

  (setq-default comint-scroll-to-bottom-on-input t    ; always insert at the bottom
                comint-scroll-to-bottom-on-output nil ; always add output at the bottom
                comint-scroll-show-maximum-output t   ; scroll to show max possible output
                comint-completion-autolist t          ; show completion list when ambiguous
                comint-input-ignoredups t             ; no duplicates in command history
                comint-completion-addsuffix t         ; insert space/slash after file completion
                comint-buffer-maximum-size 20000      ; max length of the buffer in lines
                comint-prompt-read-only nil           ; if this is t, it breaks shell-command
                comint-get-old-input (lambda () "")   ; what to run when i press enter on a
                                                      ; line above the current prompt
                comint-input-ring-size 5000           ; max shell history size
                protect-buffer-bury-p nil)

  (setenv "PAGER" "cat")

  ;; truncate buffers continuously
  (add-hook 'comint-output-filter-functions 'comint-truncate-buffer)

  (defun make-my-shell-output-read-only (text)
    "Add to comint-output-filter-functions to make stdout read only in my shells."
    (if (member (buffer-name) my-shells)
        (let ((inhibit-read-only t)
              (output-end (process-mark (get-buffer-process (current-buffer)))))
          (put-text-property comint-last-output-start output-end 'read-only t))))
  (add-hook 'comint-output-filter-functions 'make-my-shell-output-read-only)

  (defun my-dirtrack-mode ()
    "Add to shell-mode-hook to use dirtrack mode in my shell buffers."
    (when (member (buffer-name) my-shells)
      (shell-dirtrack-mode 0)
      (set-variable 'dirtrack-list '("^.*[^ ]+:\\(.*\\)>" 1 nil))
      (dirtrack-mode 1)))
  (add-hook 'shell-mode-hook 'my-dirtrack-mode)

  ;; interpret and use ansi color codes in shell output windows
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

  (defun set-scroll-conservatively ()
    "Add to shell-mode-hook to prevent jump-scrolling on newlines in shell buffers."
    (set (make-local-variable 'scroll-conservatively) 10))
  (add-hook 'shell-mode-hook 'set-scroll-conservatively)
  (add-hook 'shell-mode-hook '(lambda () (setq scroll-margin 0)))

  ;; i think this is wrong, and it buries the shell when you run emacsclient from
  ;; it. temporarily removing.
  ;; (defun unset-display-buffer-reuse-frames ()
  ;;   "Add to shell-mode-hook to prevent switching away from the shell buffer
  ;; when emacsclient opens a new buffer."
  ;;   (set (make-local-variable 'display-buffer-reuse-frames) t))
  ;; (add-hook 'shell-mode-hook 'unset-display-buffer-reuse-frames)

  ;; make it harder to kill my shell buffers
  (add-hook 'shell-mode-hook 'protect-process-buffer-from-kill-mode)

  (defun make-comint-directory-tracking-work-remotely ()
    "Add this to comint-mode-hook to make directory tracking work
while sshed into a remote host, e.g. for remote shell buffers
started in tramp. (This is a bug fix backported from Emacs 24:
http://comments.gmane.org/gmane.emacs.bugs/39082"
    (set (make-local-variable 'comint-file-name-prefix)
         (or (file-remote-p default-directory) "")))
  (add-hook 'comint-mode-hook 'make-comint-directory-tracking-work-remotely)

  (defun enter-again-if-enter ()
    "Make the return key select the current item in minibuf and shell history isearch.
An alternate approach would be after-advice on isearch-other-meta-char."
    (when (and (not isearch-mode-end-hook-quit)
               (equal (this-command-keys-vector) [13])) ; == return
      (cond ((active-minibuffer-window) (minibuffer-complete-and-exit))
            ((member (buffer-name) my-shells) (comint-send-input)))))
  (add-hook 'isearch-mode-end-hook 'enter-again-if-enter)

  (defadvice comint-previous-matching-input
    (around suppress-history-item-messages activate)
    "Suppress the annoying 'History item : NNN' messages from shell history isearch.
If this isn't enough, try the same thing with
comint-replace-by-expanded-history-before-point."
    (let ((old-message (symbol-function 'message)))
      (unwind-protect
          (progn (fset 'message 'ignore) ad-do-it)
        (fset 'message old-message))))

  (defadvice comint-send-input (around go-to-end-of-multiline activate)
    "When I press enter, jump to the end of the *buffer*, instead of the end of
the line, to capture multiline input. (This only has effect if
`comint-eol-on-send' is non-nil."
    (flet ((end-of-line () (end-of-buffer)))
      ad-do-it))

  ;; not sure why, but comint needs to be reloaded from the source (*not*
  ;; compiled) elisp to make the above advise stick.
  (load "comint.el.gz"))


(defun my-anything-map-setup ()
;;;###autoload
  (defun anything-buffer-or-ff-run-switch-other-window ()
    "Run switch to other window action from `anything-c-source-buffer+' or `anything-c-source-find-files'."
    (interactive)
    (if (eq nil (get-buffer (anything-get-selection)))
        (anything-ff-run-switch-other-window)
      (anything-buffer-switch-other-window)))

  ;; ported from v1.3.9 - latest change breaks recentf candidates
  (defun anything-ff-run-switch-other-window ()
    "Run switch to other window action from `anything-c-source-find-files'."
    (interactive)
    (anything-c-quit-and-execute-action 'find-file-other-window))

  (dolist (map (list anything-c-buffer-map
                     anything-kill-ring-map
                     anything-generic-files-map
                     anything-c-grep-map
                     anything-c-read-file-map
                     anything-find-files-map
                     anything-occur-map
                     anything-map))
    (define-key map "\C-n" 'next-history-element)
    (define-key map "\C-p" 'previous-history-element)
    (define-key map "\M-n" 'anything-next-line)
    (define-key map "\M-p" 'anything-previous-line)
    (define-key map "\M-h" 'paredit-backward-delete)
    (define-key map "\M-H" 'paredit-backward-delete)
    (define-key map "\M-w" 'paredit-backward-kill-word)
    (define-key map "\M-W" 'paredit-backward-kill-word)
    (define-key map "\M-o" 'anything-buffer-or-ff-run-switch-other-window)
    (define-key map "\M-e" 'anything-execute-persistent-action)
    (define-key map "\M-u" 'anything-find-files-down-one-level)
    (define-key map "\M-z" 'universal-argument))

  (define-key anything-c-buffer-map "\M-r" 'anything-buffer-revert-persistent)
  (define-key anything-c-buffer-map "\M-s" 'anything-buffer-save-persistent)
  (define-key anything-c-buffer-map "\M-k" 'anything-buffer-run-kill-buffers)

  (define-key anything-c-grep-map "\M-s" 'anything-c-grep-run-save-buffer)

  (define-key anything-find-files-map "\M-g" 'anything-ff-run-grep)
  (define-key anything-find-files-map "\M-l" 'anything-ff-run-switch-to-history))

(defun my-minibuffer-map-setup ()
  (dolist (map (list minibuffer-local-filename-completion-map
                     minibuffer-local-completion-map
                     minibuffer-local-must-match-filename-map
                     minibuffer-local-filename-must-match-map
                     minibuffer-local-map
                     minibuffer-local-isearch-map
                     minibuffer-local-must-match-map
                     minibuffer-local-ns-map))
    (define-key map "\C-n" 'next-history-element)
    (define-key map "\C-p" 'previous-history-element)
    (define-key map "\M-n" 'anything-next-line)
    (define-key map "\M-p" 'anything-previous-line)
    (define-key map "\M-l" 'anything-minibuffer-history)
    (define-key map "\M-h" 'paredit-backward-delete)
    (define-key map "\M-H" 'paredit-backward-delete)
    (define-key map "\M-w" 'paredit-backward-kill-word)
    (define-key map "\M-W" 'paredit-backward-kill-word)))


(defvar my-tab-map (make-sparse-keymap))
(defun my-tab-map-setup ()
  (define-key my-tab-map "\M-c" 'elscreen-create)
  (define-key my-tab-map "\M-d" 'elscreen-kill)
  (define-key my-tab-map "\M-o" 'elscreen-kill-others)
  (define-key my-tab-map "\M-n" 'elscreen-next)
  (define-key my-tab-map "\M-p" 'elscreen-previous)
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
  (define-key my-gud-map "\M-d" 'gud-down)
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
  (define-key my-gud-map "\M-t" 'gud-until)
  (define-key my-gud-map "\M-u" 'gud-up)
  (define-key my-gud-map "\M-w" 'gdb-many-windows))
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

(defun my-evil-map-setup ()
  (evil-nmap "C"    'paredit-change)
  (evil-nmap "D"    'paredit-kill)
  (evil-nmap "\M-d" 'evil-scroll-down)
  (evil-vmap "\M-d" 'evil-scroll-down)
  (evil-nmap "\M-u" 'evil-scroll-up)
  (evil-vmap "\M-u" 'evil-scroll-up)
  (evil-imap "\C-d" 'comint-send-eof shell-mode-map)
  (evil-nmap "\C-d" 'comint-send-eof shell-mode-map)
  (evil-nmap "\M-g"  my-gud-map)
  (evil-nmap "H"    'windmove-left)
  (evil-imap "\M-h" 'paredit-backward-delete)
  (evil-nmap "\M-h" 'paredit-backward)
  (evil-rmap "\M-h" 'paredit-backward-delete)
  (evil-imap "\M-H" 'paredit-backward-delete)
  (evil-nmap "\M-H" 'paredit-backward)
  (evil-rmap "\M-H" 'paredit-backward-delete)
  (evil-nmap "j"    'evil-next-visual-line)
  (evil-vmap "J"    'evil-join)
  (evil-nmap "J"    'windmove-down)
  (evil-nmap "\M-j" 'paredit-forward-down)
  (evil-nmap "\M-J" 'paredit-backward-down)
  (evil-nmap "k"    'evil-previous-visual-line)
  (evil-nmap "K"    'windmove-up)
  (evil-nmap "\M-k" 'paredit-forward-up)
  (evil-nmap "\M-K" 'paredit-backward-up)
  (evil-nmap "L"    'windmove-right)
  (evil-nmap "\M-l" 'paredit-forward)
  (evil-nmap "\M-n" '(lambda () (interactive) (next-error 1)))
  (evil-nmap "\M-N" 'evil-jump-forward)
  (evil-nmap "\M-p" '(lambda () (interactive) (next-error -1)))
  (evil-nmap "\M-P" 'evil-jump-backward)
  (evil-imap "\M-s"  nil)
  (evil-nmap "\M-s" 'eshell)
  (evil-nmap "Tc"   'transpose-chars)
  (evil-nmap "Tl"   'transpose-lines)
  (evil-nmap "Tp"   'transpose-paragraphs)
  (evil-nmap "Ts"   'transpose-sentences)
  (evil-nmap "Tw"   'transpose-words)
  (evil-nmap "\M-t"  my-tab-map)
  (evil-imap "\M-w" 'paredit-backward-kill-word)
  (evil-imap "\M-x" 'anything-M-x)
  (evil-nmap "\M-x" 'anything-M-x)
  (evil-vmap "\M-x" 'anything-M-x)
  (evil-rmap "\M-x" 'anything-M-x)
  (evil-nmap "Y"    "y$")
  (evil-imap "\C-z" 'evil-emacs-state)
  (evil-nmap "za" 'align-current)
  (evil-vmap "za" 'align-current)
  (evil-vmap "za" 'align)
  (evil-nmap "zA" 'align-regexp)
  (evil-vmap "zA" 'align-regexp)
  (evil-nmap "zc" 'evil-scroll-line-to-center)
  (evil-nmap "zd" 'kill-this-buffer)
  (evil-nmap "ze" 'anything-find-files)
  (evil-nmap "zg" 'anything-do-grep)
  (evil-nmap "zG"  my-egg-map)
  (evil-nmap "zh"   'split-window-horizontally)
  (evil-nmap "zi" 'anything-imenu)
  (evil-nmap "zj" 'paredit-join-sexps)
  (evil-nmap "zk"   'kill-compilation)
  (evil-nmap "zl"   'evil-ex-nohighlight)
  (evil-nmap "zm" 'remake)
  (evil-nmap "zM" 'make)
  (evil-nmap "zo" 'anything-occur)
  (evil-nmap "zO" 'ff-find-other-file)
  (evil-nmap "zp"   'pwd)
  (evil-nmap "zq"   'save-buffers-kill-terminal)
  (evil-nmap "zr" 'anything)
  (evil-nmap "zs" 'paredit-splice-sexp)
  (evil-nmap "zS" 'paredit-split-sexp)
  (evil-nmap "zt"  my-tag-map)
  (evil-nmap "zv"   'split-window-vertically)
  (evil-nmap "zV"   'visual-line-mode)
  (evil-nmap "zw" 'save-buffer)
  (evil-nmap "zx"   'delete-window)
  (evil-nmap "zy"   'anything-show-kill-ring)
  (evil-nmap "zz"    my-geiser-map)
  (evil-vmap "zz"    my-geiser-map)
  (evil-imap "\M-z" 'evil-normal-state)
  (evil-rmap "\M-z" 'evil-normal-state)
  (evil-nmap "\M-z" 'evil-normal-state)
  (evil-vmap "\M-z" 'evil-normal-state)
  (evil-imap "\M-1" 'paredit-backward-slurp-sexp)
  (evil-nmap "\M-1" 'paredit-backward-slurp-sexp)
  (evil-imap "\M-2" 'paredit-forward-slurp-sexp)
  (evil-nmap "\M-2" 'paredit-forward-slurp-sexp)
  (evil-imap "\M-{" 'paredit-backward-barf-sexp)
  (evil-nmap "\M-{" 'paredit-backward-barf-sexp)
  (evil-imap "\M-}" 'paredit-forward-barf-sexp)
  (evil-nmap "\M-}" 'paredit-forward-barf-sexp)
  (evil-imap "\M-[" 'paredit-wrap-square)
  (evil-nmap "\M-[" 'paredit-wrap-square)
  (evil-imap "\M-<" 'paredit-wrap-angled)
  (evil-nmap "\M-<" 'paredit-wrap-angled)
  (evil-imap "\M- " 'yas/expand)
  (evil-nmap ";"    'evil-ex-read-command)
  (evil-vmap ";"    'evil-ex-read-command)
  (evil-imap "`"    'self-insert-command)
  (evil-nmap "-"    'comment-or-uncomment-line)
  (evil-vmap "-"    'comment-or-uncomment-region)
  (evil-nmap "_"    'paredit-comment-dwim)

  (evil-mmap "$"    'evil-end-of-visual-line visual-line-mode-map)
  (evil-mmap "^"    'evil-beginning-of-visual-line visual-line-mode-map)

  (evil-omap "\t" nil)
  (evil-nmap [tab] 'indent-line)
  (evil-vmap [tab] 'indent-line)
  (evil-imap [C-tab] 'tab-to-tab-stop)
  (evil-imap (kbd "RET") 'newline-and-indent)
  (evil-nmap (kbd "RET") 'newline-and-indent)
  (evil-nmap (kbd "RET") 'occur-mode-goto-occurrence occur-mode-map))


(defun my-org-mode-evil-map-setup ()
  (evil-nmap "\M-b" 'org-backward-same-level org-mode-map)
  (evil-nmap "C"    "c$" org-mode-map)
  (evil-nmap "\M-c" 'org-shiftright org-mode-map)
  (evil-nmap "\M-C" 'org-shiftleft org-mode-map)
  (evil-nmap "D"    "d$" org-mode-map)
  (evil-nmap "\M-f" 'org-forward-same-level org-mode-map)
  (evil-nmap "\M-h" 'org-metaleft org-mode-map)
  (evil-nmap "\M-j" 'org-metadown org-mode-map)
  (evil-nmap "\M-k" 'org-metaup org-mode-map)
  (evil-nmap "\M-l" 'org-metaright org-mode-map)
  (evil-nmap "\M-n" 'outline-next-visible-heading org-mode-map)
  (evil-nmap "\M-o" 'outline-up-heading org-mode-map)
  (evil-nmap "\M-p" 'outline-previous-visible-heading org-mode-map)
  (evil-nmap "\M-q" 'fill-paragraph org-mode-map)
  (evil-imap "\M-w" 'backward-kill-word org-mode-map)
  (evil-imap [backspace] 'backward-delete-char-untabify org-mode-map)
  (evil-imap "\"" 'self-insert-command org-mode-map)
  (evil-nmap [(tab)]  'org-cycle org-mode-map))


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

(defun my-gud-mode-evil-map-setup ()
  (evil-imap (kbd "RET") 'comint-send-input gud-mode-map)
  (evil-nmap (kbd "RET") 'gdb-frames-select gdb-frames-mode-map)
  (evil-nmap (kbd "RET") 'gdb-goto-breakpoint gdb-breakpoints-mode-map)
  (evil-nmap (kbd "RET") 'gud-watch gdb-locals-mode-map))


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
(my-evil-mode-setup)
(my-gambit-setup)
(my-geiser-setup)
(my-gud-setup)
(my-haskell-mode-setup)
(my-misc-setup)
(my-monky-setup)
(my-org-mode-setup)
(my-paredit-setup)
(my-qi-mode-setup)
;; (my-rainbow-delimiters-setup)
(my-scheme-complete-setup)
(my-server-setup)
(my-tab-and-indent-setup)
(my-tramp-setup)
(my-whitespace-setup)
(my-window-system-setup)
(my-yasnippet-setup)
;; (my-shell-setup)
(my-eshell-setup)

(my-anything-map-setup)
(my-egg-map-setup)
(my-evil-map-setup)
(my-geiser-map-setup)
(my-gud-map-setup)
(my-gud-mode-evil-map-setup)
(my-haskell-map-setup)
(my-minibuffer-map-setup)
(my-misc-map-setup)
(my-tab-map-setup)
(my-tag-map-setup)
(my-org-mode-evil-map-setup)
