
(setq-default url-proxy-services '(("http" . "http://localhost:3128")
                                   ("https" . "https://localhost:3128")))

(defvar my-site-lisp "~/.emacs.d/site-lisp/")

(defvar my-load-path
  (append (mapcar (lambda (x) (concat my-site-lisp x))
                  (list ""
                        "elscreen"
                        "evil"
                        "org-mode/EXPERIMENTAL"
                        "org-mode/contrib/lisp"
                        "org-mode/lisp"
                        "magit"
                        "yasnippet-0.6.1c"))
          (list "~/.emacs.d/auto-install")))
(setq load-path (append load-path my-load-path))
(setq custom-theme-load-path (append custom-theme-load-path my-load-path))


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
                    ("elpa" . "http://tromey.com/elpa/")
                    ("SC"  . "http://joseito.republika.pl/sunrise-commander/")))
    (add-to-list 'package-archives source t))
  (package-initialize))

(defun my-auto-install-setup ()
  (require 'auto-install))

(defun my-tramp-setup ()
  (require 'tramp)

  (setq-default vc-ignore-dir-regexp
                (format "\\(%s\\)\\|\\(%s\\)"
                        vc-ignore-dir-regexp
                        tramp-file-name-regexp))

  (setq-default tramp-default-method "ssh"))

(defun my-misc-function-setup ()
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
    (let ((previous-buffer (current-buffer))
          (switch-back nil))
      (if (not (eq 'gud-mode major-mode))
          (progn
            (setq switch-back t)
            (switch-to-buffer-other-window "*gud*")))
      (gud-interrupt)
      (sleep-for 0.1)
      (gud-run-yes)
      (if switch-back
          (switch-to-buffer-other-window previous-buffer))))

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

  (defun org-tag-match-context (&optional todo-only match)
    "Identical search to `org-match-sparse-tree', but shows the content of the matches."
    (interactive "P")
    (org-prepare-agenda-buffers (list (current-buffer)))
    (org-overview)
    (org-remove-occur-highlights)
    (org-scan-tags '(progn (org-show-entry)
                           (org-show-context))
                   (cdr (org-make-tags-matcher match)) todo-only))

  (defun now ()
    "Insert string for the current date and time ISO formatted like '2011-08-01 2:34 PM'."
    (interactive)                 ; permit invocation in minibuffer
    (insert (format-time-string "%Y.%m.%d %H:%M")))

  (defun time ()
    "Insert string for the current time ISO formatted like '2:34 PM'."
    (interactive)                 ; permit invocation in minibuffer
    (insert (format-time-string "%H:%M")))

  (defun date ()
    "Insert string for today's date nicely formatted in ISO style, e.g. 2011-08-01."
    (interactive)                 ; permit invocation in minibuffer
    (insert (format-time-string "%Y.%m.%d"))))

(defun my-server-setup ()
  (setq-default server-name "terminal"))

(defun my-elscreen-setup ()
  (setq-default elscreen-prefix-key "`")
  (setq-default elscreen-startup-command-line-processing nil)
  (load "elscreen" "ElScreen" t))

(defun my-org-mode-setup ()
  (require 'org)
  (setq-default initial-major-mode 'org-mode)
  (add-hook 'find-file-hook '(lambda () (setq standard-indent 2)))
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
  (add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode)))

(defun my-autopair-setup ()
  (require 'autopair)
  (require 'auto-pair+)
  (setq-default autopair-skip-whitespace t)
  (add-hook 'find-file-hook             (lambda () (autopair-mode t)))
  (add-hook 'fundamental-mode-hook      (lambda () (autopair-mode t)))
  (add-hook 'lisp-interaction-mode-hook (lambda () (autopair-mode t)))
  (add-hook 'slime-repl-mode-hook       (lambda () (autopair-mode t)))
  (add-hook 'minibuffer-setup-hook      (lambda () (autopair-mode t))))

(defun my-paredit-setup ()
  (require 'paredit))

(defun my-yasnippet-setup ()
  (require 'yasnippet)
  (yas/initialize)
  (yas/load-directory "~/.emacs.d/site-lisp/yasnippet-0.6.1c/snippets")
  ;; (setq-default yas/prompt-functions '(yas/dropdown-prompt))
  (yas/global-mode t))

(defun my-evil-mode-setup ()
  (require 'evil)
  (evil-mode 1)
  (add-hook 'find-file-hook '(lambda ()
                               (setq evil-shift-width standard-indent)))

  (evil-set-initial-state 'completion-list-mode 'emacs)
  (evil-set-initial-state 'term-mode 'emacs)
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
  (setq-default standard-indent 2)
  (setq-default tab-width 2)
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


(defun my-haskell-mode-setup ()
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (add-hook 'haskell-mode-hook 'my-haskell-mode-hook))

(defun my-cc-mode-setup ()
  (require 'cc-mode)
  (setq-default c-default-style
                '((java-mode . "java")
                  (other . "linux"))
                c-basic-offset 2))

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
  (add-hook 'find-file-hook       'no-junk-please-were-unixish)
  (add-hook 'find-file-hook       'flyspell-mode-off)
  (add-hook 'lisp-mode-hook       'my-lisp-mode-hook)
  (add-hook 'emacs-lisp-mode-hook 'my-lisp-mode-hook)
  (add-hook 'scheme-mode-hook     'my-lisp-mode-hook)
  (add-hook 'scheme-mode-hook     'my-scheme-mode-hook)
  (set-frame-font "Source Code Pro 10")
  (add-to-list 'auto-mode-alist '("SConscript" . python-mode))
  (add-to-list 'auto-mode-alist '("SConstruct" . python-mode))
  (column-number-mode t)
  (setq-default inhibit-startup-screen t)
  (start-server)

  (add-hook 'change-major-mode-hook
            (lambda ()
              (if (not (eq 'grep-mode major-mode))
                  (if (get-buffer "*grep*")
                      (bury-buffer "*grep*")))))

  (blink-cursor-mode 1)
  (setq-default default-cursor-type 'box)
  (setq-default evil-default-cursor '("#8B2323" box))

  (setq url-proxy-services '(("http" . "127.0.0.1:3128")
                             ("https" . "127.0.0.1:3128")))

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

(defun my-magit-setup ()
  (require 'magit))

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

(defun my-multi-term-setup ()
  (require 'multi-term)
  (setq multi-term-program "/usr/bin/zsh"))



(my-package-setup)
(my-misc-function-setup)

(my-auto-install-setup)
(my-autopair-setup)
(my-cc-mode-setup)
(my-eldoc-setup)
(my-elscreen-setup)
(my-evil-mode-setup)
(my-gambit-setup)
(my-gud-setup)
(my-haskell-mode-setup)
;; (my-monky-setup)
(my-org-mode-setup)
(my-paredit-setup)
(my-qi-mode-setup)
;; (my-rainbow-delimiters-setup)
(my-scheme-complete-setup)
(my-server-setup)
(my-tab-and-indent-setup)
(my-tramp-setup)
(my-whitespace-setup)
(my-yasnippet-setup)
;; (my-shell-setup)
(my-eshell-setup)
(my-magit-setup)
(my-misc-setup)
(my-multi-term-setup)


(require 'cl)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(defun get-el-get ()
  (unless (require 'el-get nil t)
    (url-retrieve "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
                  (lambda (s)
                    (end-of-buffer)
                    (eval-print-last-sexp)))))

(get-el-get)


;; set local recipes
(setq el-get-sources
      '((:name color-theme :type elpa)
        ))

;; set own packages
(setq my:el-get-packages
      '(el-get
        ace-jump-mode
        smarttabs
        helm
        auto-complete
        auto-complete-clang
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
  (load-theme 'solarized-light t)
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

(load-init-file "ace-jump-mode.el")
(load-init-file "smarttabs.el")
(load-init-file "helm.el")
(load-init-file "auto-complete.el")
(load-init-file "color-theme-solarized.el")
(load-init-file "key-map.el")
