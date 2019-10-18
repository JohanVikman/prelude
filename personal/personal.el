;; Mouse mode
(require 'mouse)
(xterm-mouse-mode t)

;; TAIL-F emacs

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; W is set by the env.sh sourced that should be sourced when we enter a confd branch directory
(when (getenv "W")
  (load-file (substitute-in-file-name "$W/devel_support/lib/emacs/tail-f.el"))
)

;; FCI Mode
(require 'fill-column-indicator)
(setq fci-rule-color "darkblue")
(setq-default fci-rule-column 80)
(add-hook 'erlang-mode-hook 'fci-mode)

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


;; Notera hur ovanstående keybindings matchar "send-keys" i tmux.
;; Jag har "stulit
