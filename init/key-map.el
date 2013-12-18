
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


(defun my-helm-map-setup ()
;;;###autoload
  (defun helm-buffer-or-ff-run-switch-other-window ()
    "Run switch to other window action from `helm-c-source-buffer+' or `helm-c-source-find-files'."
    (interactive)
    (if (eq nil (get-buffer (helm-get-selection)))
        (helm-ff-run-switch-other-window)
      (helm-buffer-switch-other-window)))

  ;; ported from v1.3.9 - latest change breaks recentf candidates
  (defun helm-ff-run-switch-other-window ()
    "Run switch to other window action from `helm-c-source-find-files'."
    (interactive)
    (helm-c-quit-and-execute-action 'find-file-other-window))

  (dolist (map (list helm-c-buffer-map
                     helm-kill-ring-map
                     helm-generic-files-map
                     helm-c-grep-map
                     helm-c-read-file-map
                     helm-find-files-map
                     helm-map))
    (define-key map "\C-n" 'next-history-element)
    (define-key map "\C-p" 'previous-history-element)
    (define-key map "\M-n" 'helm-next-line)
    (define-key map "\M-p" 'helm-previous-line)
    (define-key map "\M-h" 'paredit-backward-delete)
    (define-key map "\M-H" 'paredit-backward-delete)
    (define-key map "\M-w" 'paredit-backward-kill-word)
    (define-key map "\M-W" 'paredit-backward-kill-word)
    (define-key map "\M-o" 'helm-buffer-or-ff-run-switch-other-window)
    (define-key map "\M-e" 'helm-execute-persistent-action)
    (define-key map "\M-u" 'helm-find-files-down-one-level))

  (define-key helm-c-buffer-map "\M-r" 'helm-buffer-revert-persistent)
  (define-key helm-c-buffer-map "\M-s" 'helm-buffer-save-persistent)
  (define-key helm-c-buffer-map "\M-k" 'helm-buffer-run-kill-buffers)

  (define-key helm-c-grep-map "\M-s" 'helm-c-grep-run-save-buffer)

  (define-key helm-c-gtags-map "\M-o" 'helm-select-2nd-action)
  (define-key helm-c-gtags-map "\M-r" 'helm-select-3rd-action)

  (define-key helm-find-files-map "\M-g" 'helm-ff-run-grep)
  (define-key helm-find-files-map "\M-l" 'helm-ff-run-switch-to-history))

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
    (define-key map "\M-n" 'helm-next-line)
    (define-key map "\M-p" 'helm-previous-line)
    (define-key map "\M-l" 'helm-minibuffer-history)
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

(defvar my-term-map (make-sparse-keymap))
(defun my-term-map-setup ()
  (defun toggle-term-char-line-mode ()
    (interactive)
    (if (term-in-char-mode)
        (term-line-mode)
      (term-char-mode)))

  (define-key my-term-map "\M-m" 'toggle-term-char-line-mode)
  (define-key my-term-map "\M-s" 'multi-term)
  (define-key my-term-map "\M-n" 'multi-term-next)
  (define-key my-term-map "\M-p" 'multi-term-prev))

(defvar my-haskell-map (make-sparse-keymap))
(defun my-haskell-map-setup ()
  (define-key my-haskell-map "d" 'inferior-haskell-find-definition)
  (define-key my-haskell-map "i" 'inferior-haskell-info)
  (define-key my-haskell-map "l" 'inferior-haskell-load-file)
  (define-key my-haskell-map "t" 'inferior-haskell-type))

(defvar my-gud-map (make-sparse-keymap))
(defun my-gud-map-setup ()
  (defun my-gdb-many-windows ()
    (interactive)
    (let ((previous-buffer (current-buffer)))
      (gdb-many-windows nil)
      (if gdb-many-windows
          (progn
            (windmove-down)
            (split-window-horizontally)
            ;; (replace-buffer-in-windows "*Buffer List*")
            (when gdb-use-separate-io-buffer
              (replace-buffer-in-windows "*input/output of *")
              (switch-to-buffer "*input/output of *"))
            (windmove-right)
            (switch-to-buffer previous-buffer))
        (progn
          (if (not (null (windmove-find-other-window 'left)))
              (windmove-left))
          (switch-to-buffer "*gud*")
            (when gdb-use-separate-io-buffer
              (split-window-vertically)
              (windmove-down)
              (switch-to-buffer "*input/output of *")
              (evil-goto-line))
          (windmove-right)
          (switch-to-buffer previous-buffer)
          (evil-scroll-line-to-center (line-number-at-pos))))))

  (define-key my-gud-map "\M-b" 'gud-break)
  (define-key my-gud-map "\M-c" 'gud-cont)
  (add-hook 'scheme-mode-hook (lambda () (define-key my-gud-map "\M-c" 'gambit-continue)))
  (define-key my-gud-map "\M-d" 'gud-down)
  (define-key my-gud-map "\M-f" 'gud-finish)
  (define-key my-gud-map "\M-g" (lambda () (interactive) (switch-to-buffer-other-window "*gud*")))
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
  (define-key my-gud-map "\M-w" 'my-gdb-many-windows))
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
  (evil-imap "\M-D" 'date)
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
  (evil-nmap "\M-N" '(lambda () (interactive) (next-error 1)))
  (evil-imap "\M-N" 'now)
  (evil-nmap "\M-f" 'evil-jump-forward)
  (evil-nmap "\M-P" '(lambda () (interactive) (next-error -1)))
  (evil-nmap "\M-b" 'evil-jump-backward)
  (evil-imap "\M-s"  nil)
  (evil-nmap "\M-s" my-term-map)
  (evil-nmap "Tc"   'transpose-chars)
  (evil-nmap "Tl"   'transpose-lines)
  (evil-nmap "Tp"   'transpose-paragraphs)
  (evil-nmap "Ts"   'transpose-sentences)
  (evil-nmap "Tw"   'transpose-words)
  (evil-nmap "\M-t"  my-tab-map)
  (evil-imap "\M-T" 'time)
  (evil-nmap "\M-u" 'evil-scroll-up)
  (evil-vmap "\M-u" 'evil-scroll-up)
  (evil-imap "\M-w" 'paredit-backward-kill-word)
  (evil-imap "\M-x" 'helm-M-x)
  (evil-nmap "\M-x" 'helm-M-x)
  (evil-vmap "\M-x" 'helm-M-x)
  (evil-rmap "\M-x" 'helm-M-x)
  (evil-nmap "Y"    "y$")
  (evil-imap "\C-z" 'evil-emacs-state)
  (evil-nmap "za" 'align-current)
  (evil-vmap "za" 'align-current)
  (evil-vmap "za" 'align)
  (evil-nmap "zA" 'align-regexp)
  (evil-vmap "zA" 'align-regexp)
  (evil-nmap "zb" 'helm-for-files)
  (evil-nmap "zB" 'bury-buffer)
  (evil-nmap "zc" 'evil-scroll-line-to-center)
  (evil-nmap "zC" 'compilation-minor-mode)
  (evil-nmap "zd" 'kill-this-buffer)
  (evil-nmap "ze" 'helm-find-files)
  (evil-nmap "zg" 'helm-do-grep)
  (evil-nmap "zh"   'split-window-horizontally)
  (evil-nmap "zi" 'helm-imenu)
  (evil-nmap "zj" 'paredit-join-sexps)
  (evil-nmap "zk"   'kill-compilation)
  (evil-nmap "zl"   'evil-ex-nohighlight)
  (evil-nmap "zm" 'remake)
  (evil-nmap "zM" 'make)
  (evil-nmap "zo" 'helm-occur)
  (evil-nmap "zO" 'ff-find-other-file)
  (evil-nmap "zp"   'pwd)
  (evil-nmap "zq"   'save-buffers-kill-terminal)
  (evil-nmap "zr" 'revert-buffer)
  (evil-nmap "zR" '(lambda () (interactive) (helm-resume t)))
  (evil-nmap "zs" 'paredit-splice-sexp)
  (evil-nmap "zS" 'paredit-split-sexp)
  (evil-nmap "zt" 'helm-gtags-select)
  (evil-nmap "zv"   'split-window-vertically)
  (evil-nmap "zV"   'visual-line-mode)
  (evil-nmap "zw" 'save-buffer)
  (evil-nmap "zx"   'delete-window)
  (evil-nmap "zX"   'delete-other-windows)
  (evil-nmap "zy"   'helm-show-kill-ring)
  (evil-imap "\M-(" 'paredit-backward-slurp-sexp)
  (evil-nmap "\M-(" 'paredit-backward-slurp-sexp)
  (evil-imap "\M-)" 'paredit-forward-slurp-sexp)
  (evil-nmap "\M-)" 'paredit-forward-slurp-sexp)
  (evil-imap "\M-{" 'paredit-backward-barf-sexp)
  (evil-nmap "\M-{" 'paredit-backward-barf-sexp)
  (evil-imap "\M-}" 'paredit-forward-barf-sexp)
  (evil-nmap "\M-}" 'paredit-forward-barf-sexp)
  (evil-imap "\M-[" 'paredit-wrap-square)
  (evil-nmap "\M-[" 'paredit-wrap-square)
  (evil-imap "\M-<" 'paredit-wrap-angled)
  (evil-nmap "\M-<" 'paredit-wrap-angled)
  (evil-imap "\M- " 'yas/expand)
  (evil-imap "\M-/" 'dabbrev-expand)
  (evil-nmap ";"    'evil-ex-read-command)
  (evil-vmap ";"    'evil-ex-read-command)
  (evil-imap "`"    'self-insert-command)
  (evil-nmap "-"    'comment-or-uncomment-line)
  (evil-vmap "-"    'comment-or-uncomment-region)
  (evil-nmap "_"    'paredit-comment-dwim)
  (evil-nmap " "    'ace-jump-char-mode)
  (evil-vmap " "    'ace-jump-char-mode)
  (evil-nmap "\M- " 'ace-jump-mode-pop-mark)
  (evil-vmap "\M- " 'ace-jump-mode-pop-mark)

  (evil-mmap "$"    'evil-end-of-visual-line visual-line-mode-map)
  (evil-mmap "^"    'evil-beginning-of-visual-line visual-line-mode-map)

  (let ((my-escape-key "M-z"))
    (defun my-esc (prompt)
      "Functionality for escaping generally.  Includes exiting Evil insert state and C-g binding."
      (cond
       ((or (evil-insert-state-p) (evil-normal-state-p) (evil-replace-state-p) (evil-visual-state-p)) [escape])
       (t (kbd "C-g"))))

    (define-key key-translation-map (kbd my-escape-key) 'my-esc)

    ;; Works around the fact that Evil uses read-event directly when in operator state, which
    ;; doesn't use the key-translation-map.
    (define-key evil-operator-state-map (kbd my-escape-key) 'keyboard-quit)

    ;; Not sure what behavior this changes, but might as well set it, seeing the Elisp manual's
    ;; documentation of it.
    (set-quit-char my-escape-key))

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
  (evil-nmap "\M-f" 'org-forward-heading-same-level org-mode-map)
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
  (define-key my-keys-minor-mode-map (kbd "M-Z") 'universal-argument)

  (define-key paredit-mode-map "\\" nil)
  (define-key key-translation-map [?\C-h] [?\C-?])
  (define-key key-translation-map [?\C-\S-h] [?\C-?])
  (define-key read-expression-map [(tab)] 'hippie-expand))

(defun my-gud-mode-evil-map-setup ()
  (evil-imap (kbd "RET") 'comint-send-input gud-mode-map)
  (evil-nmap (kbd "RET") 'gdb-frames-select gdb-frames-mode-map)
  (evil-nmap (kbd "RET") 'gdb-goto-breakpoint gdb-breakpoints-mode-map)
  (evil-nmap (kbd "RET") 'gud-watch gdb-locals-mode-map))

(define-key ac-completing-map "\t" 'ac-complete)
(define-key ac-complete-mode-map "\M-s" 'ac-helm2)

(my-helm-map-setup)
(my-evil-map-setup)
(my-gud-map-setup)
(my-gud-mode-evil-map-setup)
(my-haskell-map-setup)
(my-minibuffer-map-setup)
(my-misc-map-setup)
(my-tab-map-setup)
(my-term-map-setup)
(my-org-mode-evil-map-setup)

(add-hook 'scheme-mode-hook
          (lambda ()
            (define-key scheme-mode-map "\t" 'scheme-complete-or-indent)))

(add-hook 'autopair-mode-hook
          (lambda ()
            (define-key (cdr (car autopair-emulation-alist)) [return] nil)
            (define-key (cdr (car autopair-emulation-alist)) (kbd "RET") nil)))

(add-hook 'eshell-mode-hook
            #'(lambda ()
                (define-key eshell-mode-map
                  [remap pcomplete] 'helm-esh-pcomplete)))
