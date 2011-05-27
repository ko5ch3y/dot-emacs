(set-frame-font "Monospace 9")

(load "~/.emacs.d/elpa/package.el")
(require 'package)
(dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
                  ("elpa" . "http://tromey.com/elpa/")))
  (add-to-list 'package-archives source t))
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/site-lisp/")

(add-to-list 'load-path "~/.emacs.d/site-lisp/anything-config")
(add-to-list 'load-path "~/.emacs.d/site-lisp/anything-config/extensions")
(require 'anything-startup)

(add-to-list 'load-path "~/.emacs.d/site-lisp/vimpulse")
(require 'vimpulse)
(define-key viper-vi-diehard-map "\C-v" 'vimpulse-visual-toggle-block)
(define-key viper-vi-basic-map "\C-u" 'viper-scroll-down)
(define-key viper-insert-basic-map (kbd "C-S-h") 'delete-backward-char)
(define-key key-translation-map [?\C-h] [?\C-?])
(define-key global-map (kbd "\M-s") 'switch-to-buffer)
(vimpulse-omap "re" 'find-alternate-file)
(vimpulse-map "_" (lambda ()
                    (interactive)
                    (vimpulse-visual-toggle-line)
                    (comment-or-uncomment-region (line-beginning-position) (line-end-position))
                    (vimpulse-visual-toggle-line)
                    (viper-next-line 1)))
(vimpulse-map "-" 'comment-dwim)
(vimpulse-map "o" (lambda ()
                    (interactive)
                    (viper-open-line 1)
                    (indent-according-to-mode)))
(vimpulse-map "O" (lambda ()
                    (interactive)
                    (viper-Open-line 1)
                    (indent-according-to-mode)))

(defun anything-find-file-other-window ()
  (interactive)
  (other-window 1)
  (anything-find-file))
(vimpulse-map "zE" 'anything-find-file-other-window)
(vimpulse-map "ze" 'anything-find-file)
(vimpulse-map "zB" 'viper-switch-to-buffer-other-window)
(vimpulse-map "zb" 'viper-switch-to-buffer)
(vimpulse-map "zw" 'save-buffer)
(vimpulse-map "zq" 'save-buffers-kill-terminal)
(vimpulse-map "zcd" 'cd)
(vimpulse-map "zV" 'split-window-vertically)
(vimpulse-map "zH" 'split-window-horizontally)
(vimpulse-map "zx" 'delete-window)
(vimpulse-map "zd" 'kill-this-buffer)
(vimpulse-map "zi" 'imenu)
(vimpulse-map "zo" 'other-window)
(vimpulse-map "zh" 'windmove-left)
(vimpulse-map "zl" 'windmove-right)
(vimpulse-map "zj" 'windmove-down)
(vimpulse-map "zk" 'windmove-up)
(vimpulse-map "H" 'windmove-left)
(vimpulse-map "L" 'windmove-right)
(vimpulse-map "J" 'windmove-down)
(vimpulse-map "K" 'windmove-up)

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
(define-key c-mode-map [tab] 'indent-for-tab-command)
(vimpulse-map  [tab] 'indent-line)
(vimpulse-vmap [tab] 'indent-line)
;; (vimpulse-imap [tab] 'indent-for-tab-command)
(define-key viper-vi-basic-map [tab] 'indent-for-tab-command)
(define-key read-expression-map [(tab)] 'hippie-expand)
(vimpulse-imap [C-tab] 'tab-to-tab-stop)
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

(defadvice viper-maybe-checkout (around viper-svn-checkin-fix activate)
      "Advise viper-maybe-checkout to ignore svn files."
      (let ((file (expand-file-name (buffer-file-name buf))))
        (when (and (featurep 'vc-hooks)
                   (not (memq (vc-backend file) '(nil SVN Git))))
          ad-do-it)))

(add-to-list 'load-path "~/.emacs.d/site-lisp/elscreen")
(setq-default elscreen-prefix-key "`")
(load "elscreen" "ElScreen" t)
(define-key viper-vi-basic-map "`" 'elscreen-select-and-goto)


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


(iswitchb-mode 1)
(setq-default read-file-name-completion-ignore-case t)
(setq-default backup-inhibited t)
(setq-default auto-save-default nil)
(setq-default initial-buffer-choice t)
(setq-default inhibit-read-only t)
(global-auto-revert-mode 1)
(setq-default scroll-margin 10)

(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             ;; Automatically byte-compile emacs-lisp files upon save
             (add-hook 'after-save-hook 'emacs-lisp-byte-compile t t)))

(defun fix-server ()
  (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function))
(defun start-server ()
  (server-mode t)
  (fix-server))


(require 'qi-mode)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(global-linum-mode t)
(add-to-list 'auto-mode-alist '("\\.qml$" . js-mode))

(add-to-list 'load-path "~/.emacs.d/site-lisp/color-theme-6.6.0")
(require 'color-theme)
(defun my-light-color-theme ()
  (interactive)
  (color-theme-install
   '(my-light-color-theme
      ((background-color . "#f0f0f0")
      (background-mode . light)
      (border-color . "#969696")
      (cursor-color . "#000000")
      (foreground-color . "#000000")
      (mouse-color . "black"))
     (fringe ((t (:background "#969696"))))
     (mode-line ((t (:foreground "#ffffff" :background "#595959"))))
     (region ((t (:background "#666666"))))
     (font-lock-builtin-face ((t (:foreground "#f820b4"))))
     (font-lock-comment-face ((t (:foreground "#7d827d"))))
     (font-lock-function-name-face ((t (:foreground "#102cc1"))))
     (font-lock-keyword-face ((t (:foreground "#b415c1"))))
     (font-lock-string-face ((t (:foreground "#c77429"))))
     (font-lock-type-face ((t (:foreground"#199915"))))
     (font-lock-variable-name-face ((t (:foreground "#e6a00f"))))
     (minibuffer-prompt ((t (:foreground "#7299ff" :bold t))))
     (font-lock-warning-face ((t (:foreground "Red" :bold t))))
     )))
(color-theme-initialize)
(my-light-color-theme)
