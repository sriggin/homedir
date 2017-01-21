;; Golang stuff
(add-to-list 'exec-path "~/dev/golang/bin")

(add-hook 'go-mode-hook
          (lambda ()
            ;; Call Gofmt before saving                                                    
            (add-hook 'before-save-hook 'gofmt-before-save)
            ;; Godef jump key binding                                                      
            (local-set-key (kbd "M-.") 'godef-jump)
            (local-set-key (kbd "M-*") 'pop-tag-mark)
            (setq compile-command "go build && go test && go vet")
            (define-key (current-local-map) "\C-c\C-c" 'compile)
            (set (make-local-variable 'company-backends) '(company-go))
            (company-mode)))
