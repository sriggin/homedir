(require 'package)

;;;; Global Things

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")))

;; Refreshing!
(package-initialize)

;; Let's get naked
(show-paren-mode t)
(scroll-bar-mode 0)
(blink-cursor-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)

(setq-default inhibit-scratch-message nil
              initial-scratch-message ""
              indent-tabs-mode nil
              tab-width 4
              tab-always-indent (quote complete))
(setq inhibit-startup-message t
      inhibit-startup-screen t
      ring-bell-function 'ignore
      visible-bell t
      inhibit-startup-echo-area-message "sriggin"
      backup-by-copying t
      backup-directory-alist '(("." . "~/.saves"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(electric-indent-mode t)
(electric-pair-mode t)
(electric-layout-mode t)
(drag-stuff-mode t)
(global-hl-line-mode t)
(powerline-default-theme)

(defalias 'yes-or-no-p 'y-or-n-p) 

;; Some important packages
(dolist (pkg '(js2-mode
               haskell-mode
               sublime-themes
               rainbow-delimiters
               scala-mode
               drag-stuff
               powerline
               fill-column-indicator))
  (require pkg))

(use-package ensime
  :pin melpa-stable
  :init (setq ensime-startup-snapshot-notification nil))

(use-package projectile
  :demand
  :init   (setq projectile-use-git-grep t)
  :config (projectile-global-mode t)
  :bind   (("C-c f" . projectile-find-file)
           ("C-c C-f" . projectile-grep)))

(use-package undo-tree
  :diminish undo-tree-mode
  :config (global-undo-tree-mode)
  :bind ("C-?" . undo-tree-visualize))

(use-package flx-ido
  :demand
  :init (setq
         ido-enable-flex-matching t
         ;; C-d to open directories
         ;; C-f to revert to find-file
         ido-show-dot-for-dired nil
         ido-enable-dot-prefix t)6
  :config
  (ido-mode 1)
  (ido-everywhere 1)
  (flx-ido-mode 1))

;; Allows highlighting the current symbol
(use-package highlight-symbol
  :diminish highlight-symbol-mode
  :commands highlight-symbol
  :bind ("C-c h" . highlight-symbol))

;; Go to last change after moving around (i.e. while reading bad code)
(use-package goto-chg
  :commands goto-last-change
  ;; complementary to
  ;; C-x r m / C-x r l
  ;; and C-<space> C-<space> / C-u C-<space>
  :bind (("C-." . goto-last-change)
         ("C-," . goto-last-change-reverse)))

(use-package magit
  :commands magit-status magit-blame
  :init (setq magit-revert-buffers nil
              magit-auto-revert-mode nil
              magit-las-seen-setup-instruction "1.4.0")
  :bind (("C-c C-g s" . magit-status)
         ("C-c C-g b" . magit-blame)))

(use-package company
  :diminish company-mode
  :commands company-mode
  :init (setq company-dabbrev-ignore-case nil
              company-dabbrev-code-ignore-case nil
              company-dabbrev-downcase nil
              company-idle-delay 0
              company-minimum-prefix-length 4))

(use-package yasnippet
  :diminish yas-minor-mode
  :commands yas-minor-mode
  :config (yas-reload-all))

(use-package etags-select
  :commands etags-select-find-tag)

(use-package ace-jump-mode
  :commands ace-jump-mode
  :init (define-key global-map (kbd "C-c SPC") 'ace-jump-mode))

;; This is going to save lives
(defun contextual-backspace ()
  "Hungry whitespace or delete work depending on context."
  (interactive)
  (if (looking-back "[[:space:]\n]\\{2,\\}" (- (point) 2))
      (while (looking-back "[[:space:]\n]" (- (point) 1))
        (delete-char -1))
    (cond
     ((and (boundp 'smart-parens-strict-mode)
           smartparens-strict-mode)
      (sp-backward-kill-work 1))
     ((and (boundp 'subword-mode)
           subword-mode)
      (subword-backward-kill 1))
     (t
      (backward-kill-word 1)))))

(global-set-key (kbd "C-<backspace>") 'contextual-backspace)

(defun ensime-edit-definition-with-fallback ()
  "Variant of `ensime-edit-definition' with ctags if ENSIME is not available."
  (interactive)
  (unless (and (ensime-connection-or-nil)
               (ensime-edit-definition))
    (projectile-find-tag)))

(bind-key "M-." 'ensime-edit-definition-with-fallback ensime-mode-map)
(global-set-key (kbd "M-.") 'projectile-find-tag)
(global-set-key (kbd "M-,") 'pop-tag-mark)

(defun scala-mode-newline-comments ()
  "Custom newline appropriate for `scala-mode'."
  ;; shouldn't this be in a post-insert hook?
  (interactive)
  (newline-and-indent)
  (scala-indent:insert-asterisk-on-multiline-comment))

(bind-key "RET" 'scala-mode-newline-comments scala-mode-map)

;; let's configure some rainbow delimiter colors
(add-hook 'prog-mode-hook
          (lambda ()
            (rainbow-delimiters-mode)
            (linum-mode)
            (yas-minor-mode)
            (hs-minor-mode)
            (subword-mode)))

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

(add-hook 'js2-mode-hook (lambda ()
                           (flymake-jshint-load)
                           (local-set-key (kbd "C-c f") 'jstidy)))

(add-hook 'hs-minor-mode-hook (lambda ()
                                (local-set-key (kbd "C-c C-h") 'hs-hide-block)
                                (local-set-key (kbd "C-c C-s") 'hs-show-block)
                                (local-set-key (kbd "C-c C-S-h") 'hs-hide-all)
                                (local-set-key (kbd "C-c C-S-s") 'hs-show-all)))

(defun jstidy ()
  "Run js-beautify on the current region or buffer."
  (interactive)
  (save-excursion
    (unless mark-active (mark-defun))
    (shell-command-on-region (point) (mark) "js-beautify --good-stuff -f -" nil t)))

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; Clojure Config
;;;; Cider config
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(setq nrepl-hide-special-buffers t)

(add-hook 'scala-mode-hook
          (lambda ()
            (company-mode)
            (ensime-mode)))

(setq inferior-js-program-command "node --interactive")
;; Time for some haskell
(autoload 'ghc-init "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init) (flymake-mode)))

(setq-default fill-column 120)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customizations.. leave it alone
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Emacs-manged stuff
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (spolsky)))
 '(custom-safe-themes
   (quote
    ("0c29db826418061b40564e3351194a3d4a125d182c6ee5178c237a7364f0ff12" "e26780280b5248eb9b2d02a237d9941956fc94972443b0f7aeec12b5c15db9f3" default)))
 '(ensime-log-events t t)
 '(haskell-mode-hook
   (quote
    (turn-on-haskell-decl-scan turn-on-haskell-doc turn-on-haskell-indentation)))
 '(js2-basic-offset 2)
 '(projectile-global-mode t)
 '(projectile-tags-backend (quote ggtags))
 '(projectile-use-git-grep t)
 '(sbt:ansi-support t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#161A1F" :foreground "#DEDEDE" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "unknown" :family "Ubuntu Mono"))))
 '(ensime-errline-highlight ((t (:inherit flymake-errline))))
 '(hl-line ((t (:inherit highlight :background "#151515" :underline nil))))
 '(sbt:error ((t (:inherit error)))))
(put 'downcase-region 'disabled nil)

;; Load Golang Config
(load-file "./.emacs.d/golang.el")
