# browse-register
emacs library for browsing register.

(require 'browse-register)

;; all below is optional.

(setq browse-register-move-cursor-after-inserted-text t)

(set-register ?e '(file . "~/.emacs.el"))

(add-hook 'browse-register-mode-hook
  (lambda ()
    (linum-mode 0)
    (hl-line-mode)))

(global-set-key (kbd "<f10>") 'browse-register)
(global-set-key (kbd "C-<f10>") ctl-x-r-map)
