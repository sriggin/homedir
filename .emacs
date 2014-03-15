(require 'package)

;;;; Global Things

;; Define repositories
(dolist (source '(("melpa" . "http://melpa.milkbox.net/packages/")
                  ("marmalade" . "http://marmalade-repo.org/packages/")))
  (add-to-list 'package-archives source))

;; Refreshing!
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Fn to auto-install packages
(defun require-package (p)
  (unless (package-installed-p p)
    (package-install p))
  (require p))

;; Some important packages
(dolist (pkg '(js2-mode
               auto-complete
               yasnippet
               js-comint
               haskell-mode
               sublime-themes
               minimap
               rainbow-delimiters
               powerline))
  (require-package pkg))

;;;; Global Config

(setq-default inhibit-startup-screen t)
(setq-default inhibit-scratch-message nil)
(show-paren-mode 1)
(setq ring-bell-function 'ignore) 
(defalias 'yes-or-no-p 'y-or-n-p) 
(setq-default indent-tabs-mode nil) 
(setq-default tab-width 4) ; Anyone else is wrong.
(setq-default tab-always-indent (quote complete))

;; Lets configure some auto-complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories
             "~/.emacs.d/elpa/auto-complete/dict")

(ac-config-default)

(set-default 'ac-sources
             '(ac-source-abbrev
               ac-source-dictionary
               ac-source-yasnippet
               ac-source-words-in-buffer
               ac-source-words-in-same-mode-buffers
               ac-source-semantic))

(dolist (m '(js2-mode))
  (add-to-list 'ac-modes m))

(global-auto-complete-mode t)

;; let's configure some rainbow delimiter colors
(add-hook 'prog-mode-hook
          (lambda ()
            (rainbow-delimiters-mode)
            (linum-mode)))

(require 'cl-lib)
(require 'color)
(cl-loop
 for index from 1 to rainbow-delimiters-max-face-count
 do
 (let ((face
        (intern
         (format "rainbow-delimiters-depth-%d-face" index))))
   (cl-callf color-saturate-name (face-foreground face) 30)))
(set-face-attribute 'rainbow-delimiters-unmatched-face nil
                    :foreground 'unspecified
                    :inherit 'error
                    :strike-through t)

(require 'ido)
(ido-mode t)

(add-hook 'js2-mode-hook (lambda ()
                           (flymake-jshint-load)
                           (local-set-key (kbd "C-c f") 'jstidy)))

(defun jstidy ()
  "Run js-beautify on the current region or buffer."
  (interactive)
  (save-excursion
    (unless mark-active (mark-defun))
    (shell-command-on-region (point) (mark) "js-beautify --good-stuff -f -" nil t)))

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;;(require 'ecb)
;;(setq stack-trace-on-error t)

;; Enable useful things
(electric-indent-mode t)
(electric-pair-mode t)
(electric-layout-mode t)

(require 'js-comint)
(setq inferior-js-program-command "node --interactive")

;; Emacs-manged stuff
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (spolsky)))
 '(custom-safe-themes (quote ("e26780280b5248eb9b2d02a237d9941956fc94972443b0f7aeec12b5c15db9f3" default)))
 '(haskell-mode-hook (quote (turn-on-haskell-decl-scan turn-on-haskell-doc turn-on-haskell-indentation))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Time for some haskell
(autoload 'ghc-init "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init) (flymake-mode)))

;; Backup Configuration
(setq backup-by-copying t)
(setq backup-directory-alist '(("." . "~/.saves"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Clojure Config
;;;; Cider config
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(setq nrepl-hide-special-buffers t)

;;; nudity
(blink-cursor-mode 0)
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(setq visible-bell t)
(setq inhibit-startup-echo-area-message "sriggin")
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)

