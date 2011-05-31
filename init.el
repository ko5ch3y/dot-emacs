(set-frame-font "Monospace 9")

(load "~/.emacs.d/elpa/package.el")
(require 'package)
(dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
                  ("elpa" . "http://tromey.com/elpa/")))
  (add-to-list 'package-archives source t))
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/site-lisp/")

(add-to-list 'load-path "~/.emacs.d/init/")
(load "vim.el")

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



;; (add-to-list 'load-path "~/.emacs.d/site-lisp/elscreen")
;; (setq-default elscreen-prefix-key "`")
;; (load "elscreen" "ElScreen" t)

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
  (server-mode t)
  (fix-server))


(require 'qi-mode)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(global-linum-mode t)

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
