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
