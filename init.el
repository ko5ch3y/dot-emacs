(set-frame-font "Monospace 9")

(load "~/.emacs.d/elpa/package.el")
(require 'package)
(dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
                  ("elpa" . "http://tromey.com/elpa/")))
  (add-to-list 'package-archives source t))
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/site-lisp/")


(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")
(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t "" 'my-keys-minor-mode-map)
(my-keys-minor-mode 1)

(define-key my-keys-minor-mode-map (kbd "C-w") 'ido-delete-backward-word-updir)


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
(vim:nmap "zV" 'split-window-vertically)
(vim:nmap "zH" 'split-window-horizontally)
(vim:nmap "zx" 'delete-window)
(vim:nmap "zd" 'kill-this-buffer)
(vim:nmap "zi" 'imenu)
(vim:nmap "zo" 'other-window)
(vim:nmap "zh" 'windmove-left)
(vim:nmap "zl" 'windmove-right)
(vim:nmap "zj" 'windmove-down)
(vim:nmap "zk" 'windmove-up)
(vim:nmap "za" 'align-current)
(vim:vmap "za" 'align)
(vim:nmap "zA" 'align-regexp)
(vim:vmap "zA" 'align-regexp)
(vim:nmap "zs" 'start-server)
(vim:nmap "zo" 'occur)

(vim:nmap "H" 'windmove-left)
(vim:nmap "L" 'windmove-right)
(vim:nmap "J" 'windmove-down)
(vim:nmap "K" 'windmove-up)

(vim:imap [C-tab] 'tab-to-tab-stop)
(vim:vmap [tab] 'vim:cmd-indent)

(vim:defcmd vim:cmd-delete-bwd-word (count register)
  "Deletes the next count characters."
  (vim:cmd-delete :motion (vim:motion-bwd-word :count 1)
                  :register register))

(vim:imap "\C-w" 'vim:cmd-delete-bwd-word)
(vim:nmap "\C-j" 'vim:cmd-join-lines)

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
(define-key global-map (kbd "RET") (lambda ()
                                     (interactive)
                                     (newline)
                                     (indent-according-to-mode)))
(add-hook 'completion-at-point-functions 'hippie-expand nil)
(setq-default tab-always-indent 'complete)
(require 'cc-mode)
(define-key c-mode-map [tab] 'indent-for-tab-command)
(define-key read-expression-map [(tab)] 'hippie-expand)
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



;(setq-default inferior-lisp-program "/usr/bin/ecl")
;(require 'slime-autoloads)
;(slime-setup '(slime-fancy))
;(add-hook 'slime-mode-hook
;          (lambda ()
;            (define-key
;              viper-insert-diehard-map "\t"
;              'slime-indent-and-complete-symbol)))


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

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(global-linum-mode t)

(add-to-list 'load-path "~/.emacs.d/site-lisp/color-theme-6.6.0")
(require 'color-theme)
(require 'color-theme-molokai)
(color-theme-molokai)
