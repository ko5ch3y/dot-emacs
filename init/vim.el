;; everything todo with vim emulation

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

(vim:nmap "`" 'elscreen-select-and-goto)
