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

(setq ace-window nil)
(smartrep-define-key global-map "C-x"
  '(("o" . other-window)
    ("O" . (lambda () (other-window -1)))))

(global-nlinum-mode -1)

(defun cursor-down-some-lines ()
  "Move cursor down 1 logical lines"
  (interactive)
  (forward-line 1)
  )

(defun cursor-up-some-lines ()
  "Move cursor up 1 logical lines"
  (interactive)
  (forward-line -1))

(global-set-key (kbd "<mouse-4>") 'cursor-up-some-lines) ; wheel up
(global-set-key (kbd "<mouse-5>") 'cursor-down-some-lines) ; wheel down

;; Emacs Language server


(require 'lsp-mode)
(add-hook 'c-mode-hook #'lsp)
(add-hook 'python-mode-hook #'lsp)
(add-hook 'rust-mode-hook #'lsp)
(add-hook 'erlang-mode-hook #'lsp)

;; (add-hook 'hack-local-variables-hook
;;           (lambda() (when (derived-mode-p 'erlang-mode) (lsp))))

(setq lsp-log-io t)
(setq lsp-ui-sideline-enable t)
(setq lsp-ui-doc-enable t)
(setq lsp-ui-doc-position 'bottom)

;; (add-to-list 'load-path
;;              "~/path-to-yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

(setq blink-matching-paren-distance nil)

;; Common YANG layout:
(defun my-yang-mode-hook ()
  "Configuration for YANG Mode. Add this to `yang-mode-hook'."
  ;; (if window-system
      (progn
        (c-set-style "BSD")
        (setq indent-tabs-mode nil)
        (setq c-basic-offset 2)
        (setq font-lock-maximum-decoration t)
        (font-lock-mode t)))
;;)

(add-hook 'yang-mode-hook 'my-yang-mode-hook)

(defun show-onelevel ()
  "show entry and children in outline mode"
  (interactive)
  (outline-show-entry)
  (outline-show-children))

(defun my-outline-bindings ()
  "sets shortcut bindings for outline minor mode"
  (interactive)
  (local-set-key [?\C-,] 'hide-body)
  (local-set-key [?\C-.] 'show-all)
  (local-set-key [C-up] 'outline-previous-visible-heading)
  (local-set-key [C-down] 'outline-next-visible-heading)
  (local-set-key [C-left] 'hide-subtree)
  (local-set-key [C-right] 'show-onelevel)
  (local-set-key [M-up] 'outline-backward-same-level)
  (local-set-key [M-down] 'outline-forward-same-level)
  (local-set-key [M-left] 'hide-subtree)
  (local-set-key [M-right] 'show-subtree))

(add-hook
 'outline-minor-mode-hook
 'my-outline-bindings)

(defconst sort-of-yang-identifier-regexp "[-a-zA-Z0-9_\\.:]*")

(add-hook
 'yang-mode-hook
 '(lambda ()
    (outline-minor-mode)
    (setq outline-regexp
          (concat "^ *" sort-of-yang-identifier-regexp " *"
                  sort-of-yang-identifier-regexp
                  " *{"))))


(setq tab-width 4)

(defun er-smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(global-set-key [(shift return)] #'er-smart-open-line)


;; Enable LSP Origami Mode (for folding ranges)
(require 'lsp-origami)
(add-hook 'origami-mode-hook #'lsp-origami-mode)
(add-hook 'erlang-mode-hook #'origami-mode)

;; Provide commands to list workspace symbols:
;; - helm-lsp-workspace-symbol
;; - helm-lsp-global-workspace-symbol
;;(package-install 'helm-lsp)

;; Which-key integration
(require 'which-key)
(add-hook 'erlang-mode-hook 'which-key-mode)
(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

;; Always show diagnostics at the bottom, using 1/3 of the available space
(add-to-list 'display-buffer-alist
             `(,(rx bos "*Flycheck errors*" eos)
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (side            . bottom)
               (reusable-frames . visible)
               (window-height   . 0.33)))

;; disable automatic whitespace
(setq prelude-whitespace nil)
(setq prelude-flyspell nil)
(global-company-mode 0)
(setq prelude-format-on-save nil)

;; Disable flycheck for some types.
(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
(setq-default flycheck-disabled-checkers '(c/c++-clang))
(setq-default flycheck-disabled-checkers '(erlang))
