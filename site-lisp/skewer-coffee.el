;;; skewer-coffee.el --- skewer support for live-interactive Coffeescript

(require 's)

(defun skewer-coffee-eval (coffee-code)
  "Requests the browser to evaluate a coffeescipt string."
  (setq coffee "PATH=~/.nvm/v0.11.13/bin:$PATH coffee -pe ")
  (setq command-file "/tmp/skewer.sh")
  (setq fixed-coffee-code (concat "\'" (s-replace "'" "'\\''" (s-trim coffee-code)) "\'"))
  (setq command (concat coffee fixed-coffee-code))
  (with-temp-file command-file (insert command))
  (setq js-code (shell-command-to-string command))
  (skewer-eval js-code #'skewer-post-minibuffer))

(defun skewer-coffee-eval-region ()
  "Sends the coffeescript code the region encloses, or -- if
there's no active region -- sends the current line."
  (interactive)
  (skewer-coffee-eval
   (if (region-active-p)
       (buffer-substring-no-properties (region-beginning) (region-end))
       (thing-at-point 'line))))

(defun skewer-coffee-eval-defun ()
  "Evaluates the current 'sentence', which is usually a complete function."
  (interactive)
  (skewer-coffee-eval (thing-at-point 'sentence)))

(defun skewer-coffee-eval-buffer ()
  "Evaluates the current buffer as CoffeeScript."
  (interactive)
  (skewer-coffee-eval (buffer-substring-no-properties (point-min) (point-max))))

(defvar skewer-coffee-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map (kbd "C-x C-e") 'skewer-coffee-eval-region)
      (define-key map (kbd "C-M-x")   'skewer-coffee-eval-defun)
      (define-key map (kbd "C-c C-k") 'skewer-coffee-eval-buffer)))
  "Keymap for skewer-coffee-mode.")

;;;###autoload
(define-minor-mode skewer-coffee-mode
  "Minor mode for interactively loading coffeescript forms."
  :lighter " skewer-coffee"
  :keymap  skewer-coffee-mode-map
  :group   skewer)

(provide 'skewer-coffee)
