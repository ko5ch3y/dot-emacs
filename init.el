
(setq url-proxy-services '(("http" . "127.0.0.1:3128")
                           ("https" . "127.0.0.1:3128")))

(setq ange-ftp-try-passive-mode t)

(add-to-list 'load-path "~/.emacs.d/site-lisp/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")


(defun my-monky-setup ()
  (require 'monky)
  (setq-default monky-process-type 'cmdserver)
  (setq-default monky-log-cutoff-length 10)

  (set-face-attribute 'monky-diff-add nil :foreground "#009926")
  (set-face-attribute 'monky-diff-del nil :foreground "#DD1144")
  (set-face-attribute 'monky-log-sha1 nil :foreground "#0086B3"))

(defun my-package-setup ()
  (require 'package)
  (dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
                    ("elpa" . "http://tromey.com/elpa/")
                    ("SC"  . "http://joseito.republika.pl/sunrise-commander/")))
    (add-to-list 'package-archives source t))
  (package-initialize))

(defun my-tramp-setup ()
  (require 'tramp)

  (setq-default vc-ignore-dir-regexp
                (format "\\(%s\\)\\|\\(%s\\)"
                        vc-ignore-dir-regexp
                        tramp-file-name-regexp))

  (setq-default tramp-default-method "ssh"))


(defun my-server-setup ()
  (setq-default server-name "terminal"))


(defun my-tab-and-indent-setup ()
  (setq-default indent-line-function 'indent-according-to-mode)
  (setq-default indent-tabs-mode nil)
  (setq-default standard-indent 2)
  (setq-default tab-width 2)
  ;; (add-hook 'completion-at-point-functions 'hippie-expand nil)
  (setq-default tab-always-indent t)
  (setq-default tab-stop-list (generate-tab-stop-list)))

(defun my-geiser-setup ()
  (require 'geiser)
  (setq-default geiser-active-implementations '(guile)))

(defun my-gud-setup ()
  (require 'gud)
  (if (locate-library "gdb-mi")
      (require 'gdb-mi)
      (require 'gdb-ui))
  (gud-def gud-kill "k" nil)
  (gud-def gud-yes "y" nil)
  (setq-default gdb-show-main t)
  (setq-default gdb-use-separate-io-buffer nil)
  (setq-default gdb-max-frames 100)
  (add-hook 'change-major-mode-hook
            (lambda ()
              (if (not (eq 'gud-mode major-mode))
                  (if (get-buffer "*gud*")
                      (bury-buffer "*gud*")))
              (if (not (eq 'Buffer-menu-mode major-mode))
                  (if (get-buffer "*Buffer List*")
                      (bury-buffer "*Buffer List*")))
              (if (not (eq 'gdb-locals-mode major-mode))
                  (if (get-buffer "*locals of *")
                      (bury-buffer "*locals of *")))
              (if (not (eq 'gdb-breakpoints-mode major-mode))
                  (if (get-buffer "*breakpoints of *")
                      (bury-buffer "*breakpoints of *")))
              (if (not (eq 'gdb-frames-mode major-mode))
                  (if (get-buffer "*stack frames of *")
                      (bury-buffer "*stack frames of *")))))


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


(defun my-cc-mode-setup ()
  (require 'cc-mode)
  (setq-default c-default-style
                '((java-mode . "java")
                  (other . "linux"))
                c-basic-offset 2))

(defun my-whitespace-setup ()
  (require 'whitespace)
  (global-whitespace-mode t)
  (setq-default whitespace-style '(face tabs tab-mark)))

(defun my-misc-setup ()
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
  (setq-default scroll-conservatively 10000)
  (setq-default require-final-newline t)
  (setq-default next-line-add-newlines nil)
  (setq-default visible-bell t)
  (fset 'yes-or-no-p 'y-or-n-p)
  (show-paren-mode t)
  (setq-default show-paren-delay 0)
  (setq-default show-paren-style 'parenthesis)
  (setq-default grep-command "grep --exclude-from=$HOME/.grepignore -niHIR -e ")
  (setq initial-scratch-message "")
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (add-hook 'diff-mode-hook '(lambda () (remove-hook 'before-save-hook 'delete-trailing-whitespace)))
  (global-linum-mode 1)
  (setq-default fill-column 80)
  (add-hook 'text-mode-hook (lambda () (auto-fill-mode t)))
  (add-hook 'org-mode-hook (lambda () (auto-fill-mode t)))
  (add-hook 'c-mode-hook (lambda () ((set (make-local-variable 'comment-auto-fill-only-comments) t))))
  (setq-default truncate-lines t)
  (add-hook 'find-file-hook       'flyspell-mode-off)
  (add-hook 'lisp-mode-hook       'my-lisp-mode-hook)
  (add-hook 'emacs-lisp-mode-hook 'my-lisp-mode-hook)
  (add-hook 'scheme-mode-hook     'my-lisp-mode-hook)
  (add-hook 'scheme-mode-hook     'my-scheme-mode-hook)
  (set-frame-font "Meslo LG S for Powerline 12")
  (add-to-list 'auto-mode-alist '("SConscript" . python-mode))
  (add-to-list 'auto-mode-alist '("SConstruct" . python-mode))
  (column-number-mode t)
  (setq-default inhibit-startup-screen t)

  (add-hook 'change-major-mode-hook
            (lambda ()
              (if (not (eq 'grep-mode major-mode))
                  (if (get-buffer "*grep*")
                      (bury-buffer "*grep*")))))

  (blink-cursor-mode 1)
  (setq-default default-cursor-type 'box)

  (setq url-proxy-services '(("http" . "127.0.0.1:3128")
                             ("https" . "127.0.0.1:3128")))

  (modify-syntax-entry ?_ "w" c-mode-syntax-table)

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



  (add-hook 'change-major-mode-hook
            (lambda ()
              (if (not (eq 'eshell-mode major-mode))
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


(my-package-setup)

(my-cc-mode-setup)
(my-eldoc-setup)
(my-gud-setup)
;; (my-monky-setup)
(my-server-setup)
(my-tramp-setup)
(my-whitespace-setup)
;; (my-shell-setup)
(my-eshell-setup)


(require 'cl)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(defun get-el-get ()
  (unless (require 'el-get nil t)
    (load-file "~/.emacs.d/el-get-user/el-get-install.el")))

(get-el-get)


;; set local recipes
(setq el-get-sources
      '((:name color-theme :type elpa)
        (:name magit :type elpa)
        (:name multi-term :type elpa)
        ))

;; set own packages
(setq my:el-get-packages
      '(el-get
        ace-jump-mode
        smarttabs
        helm
        auto-complete
        auto-complete-clang
        org-mode
        markdown-mode
        yasnippet
        evil
        elscreen
        auto-pair-plus
        scheme-complete
        find-file-in-project
        haskell-mode
        auctex
        lua-mode
        paredit
        yaml-mode
        ruby-mode
        coffee-mode
        sunrise-commander
        sunrise-x-loop
        sunrise-x-tabs
        sunrise-x-modeline
        switch-window
        helm-ls-git
        skewer-mode
        s
        color-theme-solarized))

(setq my:el-get-packages
      (append my:el-get-packages
              (loop for src in el-get-sources collect (el-get-source-name src))))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

;; install new packages and init already installed packages
(el-get 'sync my:el-get-packages)


;; setup
(defun my-window-system-setup ()
  (require 'color-theme)
  (load-theme 'gruvbox t)
  (menu-bar-mode 0)
  (when window-system
    ;; (add-hook 'server-switch-hook open-client-other-window)
    (global-hl-line-mode 1)
    (tool-bar-mode 0)
    (scroll-bar-mode 0)
    (setq server-name "gui")))

(my-window-system-setup)


(setq user-init-dir "~/.emacs.d/init")

(defun load-init-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))

(load-init-file "evil.el")
(load-init-file "helpers.el")
(load-init-file "ace-jump-mode.el")
(load-init-file "smarttabs.el")
(load-init-file "helm.el")
(load-init-file "auto-complete.el")
;(load-init-file "color-theme-solarized.el")
(load-init-file "org-mode.el")
(load-init-file "yasnippet.el")
(load-init-file "elscreen.el")
(load-init-file "auto-pair-plus.el")
(load-init-file "scheme-complete.el")
(load-init-file "haskell-mode.el")
(load-init-file "paredit.el")
(load-init-file "multi-term.el")
(load-init-file "key-map.el")
(load-init-file "slime.el")
(load-init-file "coffeescript.el")

(my-tab-and-indent-setup)
(my-misc-setup)
(start-server)
(add-hook 'find-file-hook       'no-junk-please-were-unixish)
