(load "~/.emacs.d/elpa/package.el")
(require 'package)
(dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
                  ("elpa" . "http://tromey.com/elpa/")))
  (add-to-list 'package-archives source t))
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/site-lisp/")


(when window-system
  (global-hl-line-mode 0)
  (require 'color-theme)
  (add-to-list 'load-path "~/.emacs.d/site-lisp/color-theme-github")
  (require 'color-theme-github))


(require 'undo-tree)


(add-to-list 'load-path "~/.emacs.d/site-lisp/vim-mode")
(require 'vim)
(vim-mode 1)
(vim:imap (kbd "C-S-h") 'delete-backward-char)
(define-key key-translation-map [?\C-h] [?\C-?])

(vim:nmap ";" 'vim:ex-read-command)
(vim:vmap ";" 'vim:ex-read-command)

(vim:nmap "-" (lambda ()
                (interactive)
                (vim:visual-toggle-linewise)
                (comment-or-uncomment-region (line-beginning-position) (line-end-position))
                (vim:visual-toggle-linewise)
                (vim:motion-down :count 1)))
(vim:vmap "-" 'comment-or-uncomment-region)
(vim:nmap "_" 'comment-dwim)

(vim:nmap "zE" 'anything-find-file-other-window)
(vim:nmap "ze" 'anything-find-file)
(vim:nmap "zB" 'switch-to-buffer-other-window)
(vim:nmap "zb" 'switch-to-buffer)
(vim:nmap "zw" 'save-buffer)
(vim:nmap "zq" 'save-buffers-kill-terminal)
(vim:nmap "zc" 'cd)
(vim:nmap "zv" 'split-window-vertically)
(vim:nmap "zh" 'split-window-horizontally)
(vim:nmap "zx" 'delete-window)
(vim:nmap "zd" 'kill-this-buffer)
(vim:nmap "zi" 'imenu)
(vim:nmap "zo" 'other-window)
(vim:nmap "za" 'align-current)
(vim:vmap "za" 'align)
(vim:nmap "zA" 'align-regexp)
(vim:vmap "zA" 'align-regexp)
(vim:nmap "zs" 'start-server)
(vim:nmap "zo" 'occur)
(vim:nmap "zl" 'vim:cmd-nohighlight)

(vim:nmap "H" 'windmove-left)
(vim:nmap "L" 'windmove-right)
(vim:nmap "J" 'windmove-down)
(vim:nmap "K" 'windmove-up)

(vim:nmap "tc" 'transpose-chars)
(vim:nmap "tw" 'transpose-words)
(vim:nmap "tp" 'transpose-paragraphs)
(vim:nmap "ts" 'transpose-sentences)
(vim:nmap "tl" 'transpose-lines)

(vim:defcmd vim:cmd-delete-bwd-word (count register)
  "Deletes the next count characters."
  (vim:cmd-delete :motion (vim:motion-bwd-word :count 1)
                  :register register))

(vim:imap "\C-w" 'vim:cmd-delete-bwd-word)
(vim:nmap "\C-j" 'vim:cmd-join-lines)


(vim:nmap "\M-b" 'vim:cmd-prev-jump)
(vim:nmap "\M-f" 'vim:cmd-next-jump)
(vim:imap "\M-b" 'vim:cmd-prev-jump)
(vim:imap "\M-f" 'vim:cmd-next-jump)

(vim:nmap "\M-l" 'vim:scroll-line-to-center)


(vim:defcmd vim:cmd-make (nonrepeatable argument)
  "Executes compile or recompile."
  (if argument
      (compile (concat "make " argument))
    (recompile)))

(vim:emap "make" 'vim:cmd-make)
(vim:emap "m" "make")
(vim:nmap "zm" 'vim:cmd-make)
(vim:nmap "zk" 'kill-compilation)

(vim:defcmd vim:cmd-next-error (nonrepeatable count)
  "Moves to the `count'th next error."
  (next-error count))

(vim:defcmd vim:cmd-prev-error (nonrepeatable count)
  "Moves to the `count'th previous error."
  (next-error (- (or count 1))))

(vim:emap "cnext" 'vim:cmd-next-error)
(vim:emap "cn" "cnext")
(vim:nmap "zn" 'vim:cmd-next-error)
(vim:emap "cprevious" 'vim:cmd-prev-error)
(vim:emap "cp" "cprevious")
(vim:nmap "zp" 'vim:cmd-prev-error)

(setq-default vim:initial-modes
              '((debugger-mode . window)
                (compilation-mode . normal)
                (grep-mode . window)
                (gud-mode . window)
                (sldb-mode . window)
                (slime-repl-mode . window)
                (reftex-select-bib-mode . window)
                (completion-list-mode . window)
                (help-mode . motion)
                (Info-mode . motion)))

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

(vim:nmap "`" 'elscreen-select-and-goto)
(require 'vim-elscreen)
(vim:nmap "te" 'vim:cmd-tab-new)
(vim:nmap "tc" 'vim:cmd-tab-close)
(vim:nmap "to" 'vim:cmd-tab-close-other)
(vim:nmap "tn" 'vim:cmd-tab-next)
(vim:nmap "tp" 'vim:cmd-tab-previous)
(vim:nmap "tt" 'elscreen-toggle)
(vim:imap "`" 'self-insert-command)

(vim:nmap "zT" 'inferior-haskell-type)
(vim:nmap "zI" 'inferior-haskell-info)
(vim:nmap "zL" 'inferior-haskell-load-file)
(vim:nmap "zD" 'inferior-haskell-find-definition)


(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")
(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t "" 'my-keys-minor-mode-map)
(my-keys-minor-mode 1)

(define-key my-keys-minor-mode-map (kbd "C-w") 'ido-delete-backward-word-updir)

;; (defun my-c-initialization-hook ()
  ;; (define-key c-mode-base-map [tab] 'indent-for-tab-command))
;; (add-hook 'c-initialization-hook 'my-c-initialization-hook)

;; (define-key c-mode-map [tab] 'indent-for-tab-command)
(define-key read-expression-map [(tab)] 'hippie-expand)

(vim:imap [C-tab] 'tab-to-tab-stop)
(vim:vmap [tab] 'vim:cmd-indent)

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

(vim:nmap "zf" 'set-tags-file-path)


(require 'autopair)
(autopair-global-mode t)

(require 'auto-complete-config)
(ac-config-default)
;; (define-key ac-completing-map " " 'ac-complete)
(setq ac-auto-show-menu 0)

(add-to-list 'load-path "~/.emacs.d/site-lisp/anything-config")
(add-to-list 'load-path "~/.emacs.d/site-lisp/anything-config/extensions")
(require 'anything-startup)

(define-key global-map (kbd "\M-s") 'switch-to-buffer)
(defun anything-find-file-other-window ()
  (interactive)
  (other-window 1)
  (anything-find-file))

(defun indent-line ()
  (interactive)
  (let ((tab-always-indent t))
    (indent-for-tab-command nil)))

(vim:imap (kbd "RET") (lambda ()
                        (interactive)
                        (newline)
                        (indent-according-to-mode)))

(add-hook 'completion-at-point-functions 'hippie-expand nil)
(setq-default tab-always-indent 'complete)

(require 'cc-mode)
(setq-default indent-line-function 'indent-according-to-mode)
(setq-default indent-tabs-mode nil)
(setq-default standard-indent 4)
(setq-default tab-width 4)

(setq-default
  c-default-style
  '((java-mode . "java")
    (other . "linux"))
  c-basic-offset 4)

(defun generate-tab-stop-list ()
  (let ((result (list)))
    (dotimes (n 10 result)
      (setq result (cons (* (+ 1 n) standard-indent) result)))
    (reverse result)))

(setq-default tab-stop-list (generate-tab-stop-list))



(setq-default inferior-lisp-program "/usr/bin/sbcl --core /data/Temp/QiII1.07/Lisp/Qi.core")
(require 'slime)
(slime-setup)
;; (require 'slime-autoloads)
;; (slime-setup '(slime-fancy))
;; (add-hook 'slime-mode-hook
;;          (lambda ()
;;            (define-key
;;              viper-insert-diehard-map "\t"
;;              'slime-indent-and-complete-symbol)))

;; (defun qi-init-cmd (port-filename coding-system)
;;   (format "%S\n\n"
;;           `(PROGN
;;             (FUNCALL (READ-FROM-STRING "SWANK:START-SERVER")
;;                      ,port-filename
;;                      :CODING-SYSTEM , (slime-coding-system-cl-name
;;                                        coding-system)))))
;; (defun qi ()
;;   (interactive)
;;   (slime-start :program "/data/Temp/QiII1.07/Lisp/Qi-Linux-SBCL"
;;                :init 'qi-init-cmd ))


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

(defun common-lisp-hook ()
  (setq standard-indent 2)
  (setq tab-stop-list (generate-tab-stop-list)))

(add-hook 'scheme-mode-hook
          (lambda ()
            (common-lisp-hook)))
(add-hook 'lisp-mode-hook
          (lambda ()
            (common-lisp-hook)))
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (common-lisp-hook)))

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(require 'whitespace)
(global-whitespace-mode t)
(setq-default whitespace-style
        '(face trailing tabs tab-mark))


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

;; (add-hook 'emacs-lisp-mode-hook
          ;; '(lambda ()
             ;; Automatically byte-compile emacs-lisp files upon save
             ;; (add-hook 'after-save-hook 'emacs-lisp-byte-compile t t)))

(defun fix-server ()
  (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function))
(defun start-server ()
  (interactive)
  (server-mode t)
  (fix-server))


(require 'qi-mode)
(add-to-list 'auto-mode-alist '("\\.qml$" . js-mode))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(global-linum-mode t)
