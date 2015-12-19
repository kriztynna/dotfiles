(require 'package)
(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/")
 t)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(setq package-list '(flymake-jshint flymake-cursor coffee-mode flymake-coffee haml-mode flymake-haml flymake-ruby projectile projectile-rails helm magit smartparens))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(require 'flymake-jshint)
(add-hook 'js-mode-hook 'flymake-jshint-load)
(require 'flymake-coffee)
(add-hook 'coffee-mode-hook 'flymake-coffee-load)
(require 'flymake-haml)
(add-hook 'haml-mode-hook 'flymake-haml-load)
(require 'flymake-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)
(require 'flymake-cursor)

(require 'projectile-rails)
(add-hook 'projectile-mode-hook 'projectile-rails-on)

(require 'helm-config)
(require 'smartparens-config)
(require 'smartparens-ruby)
(smartparens-global-mode)
(show-smartparens-global-mode t)
(sp-with-modes '(rhtml-mode)
  (sp-local-pair "<" ">")
     (sp-local-pair "<%" "%>"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("f5eb916f6bd4e743206913e6f28051249de8ccfd070eae47b5bde31ee813d55f" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(defun fontify-frame (frame)
  (interactive)
  (if window-system
      (progn
        (if (> (x-display-pixel-width) 2000)
            (set-face-attribute 'default nil :height 135) ;; cinema
          (set-face-attribute 'default nil :height 115))))
 )

;; Fontify current frame
(fontify-frame nil)

;; Fontify any future frames
(push 'fontify-frame after-make-frame-functions)

(tool-bar-mode -1)

(global-set-key (kbd "s-p") 'helm-projectile)


(add-hook 'before-save-hook 'delete-trailing-whitespace)

(global-set-key (kbd "C-x b")   #'helm-mini)
(global-set-key (kbd "C-x C-b") #'helm-buffers-list)
(global-set-key (kbd "C-x C-m") #'helm-M-x)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(global-set-key (kbd "C-x C-r") #'helm-recentf)
