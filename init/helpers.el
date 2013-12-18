
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
  (insert (format-time-string "%Y.%m.%d")))
