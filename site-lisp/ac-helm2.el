;;; ac-helm2.el --- ac-helm.el for the latest version of auto-complete.el
(provide 'ac-helm2)
(progn (mapc 'require '(helm auto-complete))
       (require 'pos-tip-show nil t))
(when (require 'helm-show-completion nil t)
    (use-helm-show-completion 'ac-helm2
                                  '(length ac-prefix)))
(defun ac-helm2 ()
         "Select auto-complete candidate by helm."
         (interactive)
         (let ((items (popup-list ac-menu)))
           (flet ((ac-moge-header-line() (when helm-follow-mode
                                         (with-current-buffer helm-buffer
                                           (helm-aif (popup-item-documentation (helm-get-selection))
                                               (if (fboundp 'pos-tip-show) (not (pos-tip-show it)) it))))))
           (let ((source '(((name . "Popup Items")
                            (init . (lambda () (with-current-buffer (helm-candidate-buffer 'local)
                                             (loop for x in items do (insert x "\n")))))
                            (candidates-in-buffer)
                            (get-line . buffer-substring)
                            (action . (("Select" . (lambda(x) (flet ((popup-selected-item (&rest _) x))
                                                            (call-interactively 'ac-complete))))))
                            (header-line . ac-moge-header-line)
                            (persistent-action . (lambda (x)))))))
             (helm source ac-prefix)))))
