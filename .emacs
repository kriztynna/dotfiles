;;; package --- Summary
;;; Commentary:

;;; Code:
(require 'package)
(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/")
 t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar package-list)
(setq package-list
      '(
        ag
        flycheck ;; npm install -g coffee-script coffeelint
        flycheck-typescript-tslint ;; npm install -g tslint
        coffee-mode
        haml-mode
        projectile
        projectile-rails
        helm
        helm-ag
        helm-projectile
        magit
        sass-mode
        scss-mode
        smartparens
        tide
        typescript-mode
        yaml-mode
        zenburn-theme
        ))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(add-hook 'after-init-hook #'global-flycheck-mode)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-typescript-tslint-setup))

(require 'projectile-rails)
(add-hook 'projectile-mode-hook 'projectile-rails-on)
(add-hook 'typescript-mode-hook 'tide-setup)

;;; ispell setup
;;; Dependencies:
;;; brew install ispell
(defvar ispell-program-name)
(setq ispell-program-name "/usr/local/Cellar/ispell/3.4.00/bin/ispell")

(require 'helm-config)
(require 'smartparens-config)
(require 'smartparens-ruby)
(smartparens-global-mode)
(show-smartparens-global-mode t)
(sp-with-modes '(rhtml-mode)
  (sp-local-pair "<" ">")
     (sp-local-pair "<%" "%>"))

(setq-default indent-tabs-mode nil
	      c-basic-offset 2
	      tab-width 2
	      )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("85c59044bd46f4a0deedc8315ffe23aa46d2a967a81750360fb8600b53519b8a" "f5eb916f6bd4e743206913e6f28051249de8ccfd070eae47b5bde31ee813d55f" default)))
 '(tab-width 2))
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

;;; .emacs ends here
