;; Mouse mode
(require 'mouse)
(xterm-mouse-mode t)

;; TAIL-F emacs

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (setq package-check-signature nil)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
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


;; Ace-window and other-window configurations
(smartrep-define-key global-map "C-x"
  '(("o" . other-window)
    ("O" . (lambda () (other-window -1)))))


;; org-journal configuration
(setq org-journal-dir "~/org/journal")

(progn
  ;; Make whitespace-mode with very basic background coloring for whitespaces.
  ;; http://ergoemacs.org/emacs/whitespace-mode.html
  (setq whitespace-style (quote (face spaces tabs newline space-mark tab-mark newline-mark )))

  ;; Make whitespace-mode and whitespace-newline-mode use “¶” for end of line char and “▷” for tab.
  (setq whitespace-display-mappings
        ;; all numbers are unicode codepoint in decimal. e.g. (insert-char 182 1)
        '(
          (space-mark 32 [183] [46]) ; SPACE 32 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
          (newline-mark 10 [182 10]) ; LINE FEED,
          (tab-mark 9 [9655 9] [92 9]) ; tab
          )))


;; Prelude uses 'ace-window' for the other-window functionality
;; which is annoying. This setting reverts to "default" emacs
;; other-window behavior.
(global-set-key [remap other-window] 'other-window)
