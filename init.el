(load "~/.emacs.d/elpa/package.el")
(require 'package)
(dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
                  ("elpa" . "http://tromey.com/elpa/")))
  (add-to-list 'package-archives source t))
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/site-lisp/")


(require 'auto-install)
(setq-default url-proxy-services '(("http" . "localhost:3128")))


(setq server-name "terminal")

(when window-system
  (global-hl-line-mode 0)
  (require 'color-theme)
  (add-to-list 'load-path "~/.emacs.d/site-lisp/color-theme-github")
  (require 'color-theme-github)
  (setq server-name "gui"))


(require 'undo-tree)


(winner-mode t)
(setq compilation-finish-functions 'compile-autoclose)
(defun compile-autoclose (buffer string)
  (cond ((string-match "finished" string)
         (kill-buffer "*compilation*")
         (message "Build successful."))
        (t
         (message "Compilation exited abnormally: %s" string))))


(add-to-list 'load-path "~/.emacs.d/site-lisp/elscreen")
(setq-default elscreen-prefix-key "`")
(setq-default elscreen-startup-command-line-processing nil)
(load "elscreen" "ElScreen" t)


(add-to-list 'load-path "~/.emacs.d/site-lisp/org-mode/lisp")
(add-to-list 'load-path "~/.emacs.d/site-lisp/org-mode/contrib/lisp")
(add-to-list 'load-path "~/.emacs.d/site-lisp/org-mode/EXPERIMENTAL")
(require 'org-install)
(require 'org-mw)
(setq-default initial-major-mode 'org-mode)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))


(add-to-list 'load-path "~/.emacs.d/site-lisp/anything-config")
(add-to-list 'load-path "~/.emacs.d/site-lisp/anything-config/extensions")
(require 'anything-startup)
(require 'anything-gtags)
(setq-default anything-gtags-enable-initial-pattern t)
(setq-default gtags-path-style 'relative)


(require 'paredit)
(add-hook 'scheme-mode-hook           (lambda () (paredit-mode t)))
(add-hook 'lisp-mode-hook             (lambda () (paredit-mode t)))
(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode t)))
(add-hook 'slime-repl-mode-hook       (lambda () (paredit-mode t)))
(add-hook 'minibuffer-setup-hook      (lambda () (paredit-mode t)))


(add-to-list 'load-path "~/.emacs.d/site-lisp/vim-mode")
(require 'vim)
(vim-mode 1)
(require 'vim-elscreen)

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
  (if argument
      (compile (concat "make " argument))
    (recompile)))

(vim:defcmd vim:cmd-next-error (nonrepeatable count)
  "Moves to the `count'th next error."
  (next-error count))

(vim:defcmd vim:cmd-prev-error (nonrepeatable count)
  "Moves to the `count'th previous error."
  (next-error (- (or count 1))))

(defun comment-uncomment-line ()
  (interactive)
  (vim:visual-toggle-linewise)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position))
  (vim:visual-toggle-linewise)
  (vim:motion-down :count 1))


(defun find-tags-file ()
  "recursively searches each parent directory for a file named `TAGS' and returns the
path to that file or nil if a tags file is not found. Returns nil if the buffer is
not visiting a file"
  (labels
      ((find-tags-file-r (path)
         (let* ((parent (file-name-directory path))
                (possible-tags-file (concat parent "TAGS")))
           (cond
             ((file-exists-p possible-tags-file) (throw 'found-it possible-tags-file))
             ((string= "/TAGS" possible-tags-file) (error "no TAGS file found"))
             (t (find-tags-file-r (directory-file-name parent)))))))

    (if (buffer-file-name)
        (catch 'found-it
          (find-tags-file-r (buffer-file-name)))
        (error "buffer is not visiting a file"))))

(defun set-tags-file-path ()
  "calls `find-tags-file' to recursively search up the directory tree to find
a file named `tags'. If found, calls `visit-tags-table' with that path as an argument
otherwise raises an error."
  (interactive)
  (visit-tags-table (find-tags-file)))

(defun anything-find-file-other-window ()
  (interactive)
  (other-window 1)
  (anything-find-file))

(defun indent-line ()
  (interactive)
  (let ((tab-always-indent t))
    (indent-for-tab-command nil)))

(defun fix-server ()
  (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function))

(defun start-server ()
  (interactive)
  (server-mode t)
  (fix-server))

(defun find-tag-at-point ()
 (interactive)
 (find-tag (thing-at-point 'symbol)))

(defun find-tag-at-point-other-window ()
 (interactive)
 (find-tag-other-window (thing-at-point 'symbol)))

(defun generate-tab-stop-list ()
  (let ((result (list)))
    (dotimes (n 10 result)
      (setq result (cons (* (+ 1 n) standard-indent) result)))
    (reverse result)))

(autoload 'scheme-smart-complete "scheme-complete" nil t)
(autoload 'scheme-get-current-symbol-info "scheme-complete" nil t)

(setq-default scheme-indent-function 'scheme-smart-indent-function)
(setq-default indent-line-function 'indent-according-to-mode)
(setq-default indent-tabs-mode nil)
(setq-default standard-indent 4)
(setq-default tab-width 4)
;; (add-hook 'completion-at-point-functions 'hippie-expand nil)
(setq-default tab-always-indent 'complete)
(setq-default tab-stop-list (generate-tab-stop-list))


(require 'gud)
(gud-def gud-kill "k" nil)
(gud-def gud-yes "y" nil)

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
  (gud-interrupt)
  (sleep-for 0.1)
  (gud-run-yes))

(defun paredit-change ()
  (interactive)
  (paredit-kill)
  (vim:insert-mode))

(defvar meta-t-map (make-sparse-keymap))
(setq my-tab-map meta-t-map)

(define-key my-tab-map "c"    'vim:cmd-tab-new)
(define-key my-tab-map "d"    'vim:cmd-tab-close)
(define-key my-tab-map "o"    'vim:cmd-tab-close-other)
(define-key my-tab-map "n"    'vim:cmd-tab-next)
(define-key my-tab-map "p"    'vim:cmd-tab-previous)
(define-key my-tab-map "t"    'elscreen-toggle)
(define-key my-tab-map "\M-c" 'vim:cmd-tab-new)
(define-key my-tab-map "\M-d" 'vim:cmd-tab-close)
(define-key my-tab-map "\M-o" 'vim:cmd-tab-close-other)
(define-key my-tab-map "\M-n" 'vim:cmd-tab-next)
(define-key my-tab-map "\M-p" 'vim:cmd-tab-previous)
(define-key my-tab-map "\M-t" 'elscreen-toggle)

(defvar meta-shift-h-map (make-sparse-keymap))
(setq my-haskell-map meta-shift-h-map)

(define-key my-haskell-map "d" 'inferior-haskell-find-definition)
(define-key my-haskell-map "i" 'inferior-haskell-info)
(define-key my-haskell-map "l" 'inferior-haskell-load-file)
(define-key my-haskell-map "t" 'inferior-haskell-type)

(define-key anything-map "\M-n" 'anything-next-line)
(define-key anything-map "\C-n" 'next-history-element)
(define-key anything-map "\M-o" 'anything-select-2nd-action)
(define-key anything-map "\M-p" 'anything-previous-line)
(define-key anything-map "\C-p" 'previous-history-element)
(define-key anything-map "\M-r" 'anything-select-3rd-action)

(define-key minibuffer-local-map "\C-p" 'previous-history-element)
(define-key minibuffer-local-map "\C-n" 'next-history-element)
(define-key minibuffer-local-map "\M-p" 'anything-previous-line)
(define-key minibuffer-local-map "\M-n" 'anything-next-line)
(define-key minibuffer-local-map "\M-h" 'anything-minibuffer-history)


(vim:imap (kbd "RET") 'newline-and-indent)

(vim:nmap ";" 'vim:ex-read-command)
(vim:vmap ";" 'vim:ex-read-command)

(vim:imap "`" 'self-insert-command)

(vim:nmap "-" 'comment-uncomment-line)
(vim:vmap "-" 'comment-or-uncomment-region)
(vim:nmap "_" 'paredit-comment-dwim)

(vim:imap [C-tab] 'tab-to-tab-stop)
(vim:vmap [tab] 'vim:cmd-indent)

(vim:nmap "\M-a" 'align-current)
(vim:vmap "\M-a" 'align)
(vim:nmap "\M-A" 'align-regexp)
(vim:vmap "\M-A" 'align-regexp)
(vim:nmap "\C-B" 'gud-break)
(vim:nmap "C"    'paredit-change)
(vim:nmap "\M-c" 'vim:scroll-line-to-center)
(vim:nmap "\C-C" 'gud-cont)
(vim:nmap "D"    'paredit-kill)
(vim:nmap "\M-d" 'kill-this-buffer)
(vim:nmap "\C-d" 'paredit-forward-delete)
(vim:nmap "\C-D" 'gud-down)
(vim:nmap "\M-e" 'anything-find-file)
(vim:nmap "\C-F" 'gud-finish)
(vim:nmap "\M-g" 'grep)
(vim:nmap "H"    'windmove-left)
(vim:imap "\M-h" 'paredit-backward)
(vim:nmap "\M-h" 'paredit-backward)
(vim:nmap "\M-H"  my-haskell-map)
(vim:imap "\C-h" 'paredit-backward-delete)
(vim:imap "\C-H" 'paredit-backward-delete)
(vim:nmap "\M-i" 'imenu)
(vim:nmap "\C-I" 'gud-interrupt)
(vim:nmap "J"    'windmove-down)
(vim:imap "\M-j" 'paredit-forward-down)
(vim:nmap "\M-j" 'paredit-forward-down)
(vim:imap "\M-J" 'paredit-backward-down)
(vim:nmap "\M-J" 'paredit-backward-down)
(vim:nmap "\C-j" 'paredit-join-sexps)
(vim:nmap "K"    'windmove-up)
(vim:imap "\M-k" 'paredit-forward-up)
(vim:nmap "\M-k" 'paredit-forward-up)
(vim:imap "\M-K" 'paredit-backward-up)
(vim:nmap "\M-K" 'paredit-backward-up)
(vim:nmap "\C-K" 'gud-kill-yes)
(vim:nmap "L"    'windmove-right)
(vim:imap "\M-l" 'paredit-forward)
(vim:nmap "\M-l" 'paredit-forward)
(vim:nmap "\M-m" 'vim:cmd-make)
(vim:nmap "\C-M" 'gud-restart)
(vim:nmap "\M-n" 'vim:cmd-next-error)
(vim:nmap "\M-N" 'vim:cmd-next-jump)
(vim:imap "\M-N" 'vim:cmd-next-jump)
(vim:nmap "\C-N" 'gud-next)
(vim:nmap "\M-o" 'occur)
(vim:nmap "\M-O" 'ff-find-other-file)
(vim:nmap "\M-p" 'vim:cmd-prev-error)
(vim:imap "\M-P" 'vim:cmd-prev-jump)
(vim:nmap "\M-P" 'vim:cmd-prev-jump)
(vim:nmap "\C-P" 'gud-print)
(vim:nmap "\M-r" 'switch-to-buffer)
(vim:nmap "\C-R" 'gud-remove)
(vim:nmap "\M-s" 'paredit-splice-string)
(vim:nmap "\M-S" 'paredit-split-sexp)
(vim:nmap "\C-S" 'gud-step)
(vim:nmap "ts"   'anything-gtags-select)
(vim:nmap "tt"   'anything-gtags-resume)
(vim:nmap "Tc"   'transpose-chars)
(vim:nmap "Tl"   'transpose-lines)
(vim:nmap "Tp"   'transpose-paragraphs)
(vim:nmap "Ts"   'transpose-sentences)
(vim:nmap "Tw"   'transpose-words)
(vim:nmap "\M-t"  my-tab-map)
(vim:nmap "\C-T" 'gud-tbreak)
(vim:nmap "\C-u" 'gud-until)
(vim:nmap "\C-U" 'gud-up)
(vim:nmap "\M-w" 'save-buffer)
(vim:imap "\C-w" 'paredit-backward-kill-word)
(vim:nmap "Y"    "y$")
(vim:imap "\M-z" 'vim:insert-mode-exit)
(vim:vmap "\M-z" 'vim:visual-mode-exit)
(vim:nmap "zh"   'split-window-horizontally)
(vim:nmap "zk"   'kill-compilation)
(vim:nmap "zl"   'vim:cmd-nohighlight)
(vim:nmap "zq"   'save-buffers-kill-terminal)
(vim:nmap "zs"   'start-server)
(vim:nmap "zv"   'split-window-vertically)
(vim:nmap "zx"   'delete-window)
(vim:imap "\M-1" 'paredit-backward-slurp-sexp)
(vim:nmap "\M-1" 'paredit-backward-slurp-sexp)
(vim:imap "\M-2" 'paredit-forward-slurp-sexp)
(vim:nmap "\M-2" 'paredit-forward-slurp-sexp)
(vim:imap "{"    'paredit-open-curly)
(vim:imap "\M-{" 'paredit-backward-barf-sexp)
(vim:nmap "\M-{" 'paredit-backward-barf-sexp)
(vim:imap "}"    'paredit-close-curly)
(vim:imap "\M-}" 'paredit-forward-barf-sexp)
(vim:nmap "\M-}" 'paredit-forward-barf-sexp)
(vim:imap "\M- " 'yas/expand)
(vim:emap "make" 'vim:cmd-make)
(vim:emap "m"    "make")

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'my-haskell-mode-hook)

(defun my-org-mode-hook ()
  (vim:local-nmap [tab]  'org-cycle)
  (vim:local-nmap "C"    "c$")
  (vim:local-nmap "D"    "d$")
  (vim:local-nmap "\C-d" 'vim:cmd-delete-char)
  (vim:local-imap "\M-h" 'org-metaleft)
  (vim:local-nmap "\M-h" 'org-metaleft)
  (vim:local-imap "\C-h" 'delete-backward-char)
  (vim:local-imap "\C-H" 'delete-backward-char)
  (vim:local-imap "\M-k" 'org-metaup)
  (vim:local-nmap "\M-k" 'org-metaup)
  (vim:local-nmap "\M-l" 'org-metaright)
  (vim:local-imap "\M-l" 'org-metaright)
  (vim:local-imap "\M-j" 'org-metadown)
  (vim:local-nmap "\M-j" 'org-metadown)
  (vim:local-imap "\M-n" 'outline-next-visible-heading)
  (vim:local-nmap "\M-n" 'outline-next-visible-heading)
  (vim:local-imap "\M-p" 'outline-previous-visible-heading)
  (vim:local-nmap "\M-p" 'outline-previous-visible-heading)
  (vim:local-imap "\M-u" 'outline-up-heading)
  (vim:local-nmap "\M-u" 'outline-up-heading)
  (vim:local-imap "\M-f" 'org-forward-same-level)
  (vim:local-nmap "\M-f" 'org-forward-same-level)
  (vim:local-imap "\M-b" 'org-backward-same-level)
  (vim:local-nmap "\M-b" 'org-backward-same-level)
  (vim:local-imap "\C-w" 'vim:cmd-delete-bwd-word))

(add-hook 'org-mode-hook 'my-org-mode-hook)

(defun common-lisp-hook ()
  (setq standard-indent 2)
  (setq tab-stop-list (generate-tab-stop-list)))

(add-hook 'lisp-mode-hook       'common-lisp-hook)
(add-hook 'emacs-lisp-mode-hook 'common-lisp-hook)

(defun my-scheme-mode-hook ()
  (mapc (lambda (sym)
          (put sym 'scheme-indent-function 'defun))
        (list 'call/cc 'c-lambda))

  (define-key scheme-mode-map "\t" 'scheme-complete-or-indent)

  (make-local-variable 'eldoc-documentation-function)
  (setq eldoc-documentation-function 'scheme-get-current-symbol-info)
  (eldoc-mode))

(add-hook 'scheme-mode-hook 'common-lisp-hook)
(add-hook 'scheme-mode-hook 'my-scheme-mode-hook)

(autoload 'gambit-inferior-mode "gambit" "Hook Gambit mode into cmuscheme.")
(autoload 'gambit-mode "gambit" "Hook Gambit mode into scheme.")
(add-hook 'inferior-scheme-mode-hook (function gambit-inferior-mode))
(add-hook 'scheme-mode-hook (function gambit-mode))
(setq-default scheme-program-name "gsi -:d-")

(defun my-gambit-mode-hook ()
  (vim:local-nmap "\M-C" 'gambit-continue)
  (vim:local-nmap "\M-L" 'gambit-leap-continuation)
  (vim:local-nmap "\M-N" 'gambit-crawl-backtrace-newer)
  (vim:local-nmap "\M-O" 'gambit-crawl-backtrace-older)
  (vim:local-nmap "\M-S" 'gambit-step-continuation))
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

(add-hook 'scheme-mode-hook 'my-gambit-mode-hook)

(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")
(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t "" 'my-keys-minor-mode-map)
(my-keys-minor-mode 1)

(define-key my-keys-minor-mode-map (kbd "C-w") 'ido-delete-backward-word-updir)

(define-key key-translation-map [?\C-h] [?\C-?])
(define-key key-translation-map [?\C-\S-h] [?\C-?])

(require 'cc-mode)
(setq-default c-default-style
              '((java-mode . "java")
                (other . "linux"))
              c-basic-offset 4)

;; (defun my-c-initialization-hook ()
  ;; (define-key c-mode-base-map [tab] 'indent-for-tab-command))
;; (add-hook 'c-initialization-hook 'my-c-initialization-hook)

;; (define-key c-mode-map [tab] 'indent-for-tab-command)
(define-key read-expression-map [(tab)] 'hippie-expand)


(defun now ()
  "Insert string for the current date and time ISO formatted like '2011-08-01 2:34 PM'."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%Y-%m-%d %-I:%M %p")))

(defun today ()
  "Insert string for today's date nicely formatted in ISO style, e.g. 2011-08-01."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%Y-%m-%d")))

(defun short-date ()
  "Insert string for today's date formatted like 110801."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%y%m%d")))


(require 'autopair)
(autopair-global-mode t)

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
;; (define-key ac-completing-map " " 'ac-complete)
(setq-default ac-auto-show-menu 0)

(add-to-list 'load-path "~/.emacs.d/site-lisp/yasnippet-0.6.1c")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/site-lisp/yasnippet-0.6.1c/snippets")
;; (setq-default yas/prompt-functions '(yas/dropdown-prompt))
(yas/global-mode t)

(add-to-list 'load-path "~/.emacs.d/site-lisp/auto-complete-clang")
(require 'auto-complete-clang)
(setq-default ac-clang-auto-save nil)

(defun my-ac-config ()
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
(my-ac-config)


(set-frame-font "Monospace 10")
;; (set-face-attribute 'default nil :weight 'bold)
;; (require 'rainbow-delimiters)
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:foreground "red"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "violet"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "orange"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "purple"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "brown"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "darkblue"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "green"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "blue"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "cyan")))))

(require 'whitespace)
(global-whitespace-mode t)
(setq-default whitespace-style
        '(face tabs tab-mark))


(ido-mode 0)
(iswitchb-mode 0)

(setq-default read-file-name-completion-ignore-case t)
(setq-default backup-inhibited t)
(setq-default auto-save-default nil)
;; (setq-default initial-buffer-choice t)
(setq-default inhibit-read-only t)
(global-auto-revert-mode 1)
(setq-default scroll-margin 10)
(setq-default scroll-step 1)
(setq-default require-final-newline t)
(setq-default next-line-add-newlines nil)
(setq-default visible-bell t)
(fset 'yes-or-no-p 'y-or-n-p)

(setq-default show-paren-delay 0)
(setq-default show-paren-style 'mixed)

(setq-default grep-command "grep --exclude-from=$HOME/.grepignore -niHI -e ")

;; (add-hook 'emacs-lisp-mode-hook
          ;; '(lambda ()
             ;; Automatically byte-compile emacs-lisp files upon save
             ;; (add-hook 'after-save-hook 'emacs-lisp-byte-compile t t)))


(require 'qi-mode)
(add-to-list 'auto-mode-alist '("\\.qml$" . js-mode))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(global-linum-mode t)
(setq-default fill-column 80)


(defun no-junk-please-were-unixish ()
  (let ((coding-str (symbol-name buffer-file-coding-system)))
    (when (string-match "-\\(?:dos\\|mac\\)$" coding-str)
      (setq coding-str
            (concat (substring coding-str 0 (match-beginning 0)) "-unix"))
      (message "CODING: %s" coding-str)
      (set-buffer-file-coding-system (intern coding-str)) )))

(add-hook 'find-file-hooks 'no-junk-please-were-unixish)


(vim:define-keymap vim-gdb-mode "vim gdb mode" :map-command gmap)

(vim:define-mode window "VIM gdb mode"
                 :ident "G"
                 :keymaps '(vim:normal-mode-keymap
                            vim:operator-pending-mode-keymap
                            vim:motion-mode-keymap
                            vim:window-mode-keymap
                            vim:override-keymap)
                 :command-function 'vim:normal-mode-command)


(require 'eldoc)
(require 'c-eldoc)
(setq-default c-eldoc-includes "`pkg-config QtCore QtGui --cflags` -I./ -I../ -I/usr/include")
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
