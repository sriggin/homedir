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

;; Now I'm having too much fun
(defun require-packages (ps)
  (mapc 'require-package ps))

;; Some important packages
(require-packages '(js2-mode
                    auto-complete
                    yasnippet
                    markdown-mode))

;;;; Global Config

(global-linum-mode t) ; This may be overkill
(setq-default inhibit-startup-screen t)
(setq-default inhibit-scratch-message nil)
(show-paren-mode 1)
(setq ring-bell-function 'ignore) ; Stop that!!
(defalias 'yes-or-no-p 'y-or-n-p) ; Ain't nobody got time for this
(setq-default indent-tabs-mode nil) ; Go to the right place and nowhere else.
(setq-default tab-width 4) ; Anyone else is wrong.
(setq-default tab-always-indent (quote complete))

;; Lets configure some auto-complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete/dict")

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

(require 'ido)
(ido-mode t)

(require 'flycheck)
(flycheck-define-checker 
    javascript-jslint-reporter
  "A JavaScript syntax and style checker based on JSLint Reporter

See URL `https://github.com/FND/jslint-reporter'."
  :command ("~/.emacs/jslint-reporter" source)
  :error-patterns 
  ((error line-start (1+ nonl) "@" line "[" column "] - " (message) line-end))
  :modes (js2-mode))
(add-hook 'j2-mode-hook (lambda () 
                          (flycheck-select-checker 'javascript-jslint-reporter)
                          (flycheck-mode)
                          (dolist (binding js2-keymap)
                            (local-set-key (kbd (car binding)) (cdr binding)))))
(setq js2-keymap '(("C-xC-e" . js-send-last-sexp)
                   ("C-M-x" . js-send-last-sexp-and-go)
                   ("C-cb" . js-send-buffer)
                   ("C-cC-b" . js-send-buffer-and-go)
                   ("C-cl" . js-load-file-and-go)))


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
 '(custom-enabled-themes (quote (deeper-blue))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Backup Configuration
(setq backup-by-copying t)
(setq backup-directory-alist '(("." . "~/saves"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Global Key bindings
(global-set-key (kbd "C-DEL") 'undo)
