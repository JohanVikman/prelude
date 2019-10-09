(setq prelude-theme 'gruvbox)

;; Window move together with tmux
(defun windmove-emacs-or-tmux(dir tmux-cmd)
  (interactive)
  (if (ignore-errors (funcall (intern (concat "windmove-" dir))))
      nil ;; Moving within emacs
    (shell-command tmux-cmd)) ;; At edges, send command to tmux
  )

(global-set-key (kbd "M-P")
                '(lambda ()
                   (interactive)
                   (windmove-emacs-or-tmux "up" "tmux select-pane -U")))
(global-set-key (kbd "M-N")
                '(lambda ()
                   (interactive)
                   (windmove-emacs-or-tmux "down" "tmux select-pane -D")))
(global-set-key (kbd "M-F")
                '(lambda ()
                   (interactive)
                   (windmove-emacs-or-tmux "right" "tmux select-pane -R")))
(global-set-key (kbd "M-B")
                '(lambda ()
                   (interactive)
                   (windmove-emacs-or-tmux "left"  "tmux select-pane -L")))

;; Disable highlight current line mode
(global-hl-line-mode 1)
(set-face-background 'hl-line "#3e4446")
(set-face-foreground 'highlight nil)
