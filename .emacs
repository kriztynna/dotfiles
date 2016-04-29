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
        auto-complete
        flycheck ;; npm install -g coffee-script coffeelint
        flycheck-typescript-tslint ;; npm install -g tslint
        flycheck-haskell
        coffee-mode
        haml-mode
        haskell-mode
        projectile
        projectile-rails
        helm
        helm-ag
        helm-projectile
        magit
        rspec-mode
        rvm
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

(ac-config-default)

(add-hook 'after-init-hook #'global-flycheck-mode)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-typescript-tslint-setup))

(require 'projectile-rails)
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(setq projectile-switch-project-action 'helm-projectile)
(add-hook 'projectile-mode-hook 'projectile-rails-on)
(add-hook 'typescript-mode-hook 'tide-setup)

;; ispell setup
;; Dependencies:
;; brew install ispell
;; This last part makes it so that ispell works in emacs launched from the dock
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "/usr/bin")

;; To add a Spanish-language dictionary, get the COES dictionary from
;; http://www.datsi.fi.upm.es/~coes/
;; Follow the installation instructions, then change the names of the resulting
;; files from espa~nol.hash and espa~nol.aff to castellano.hash and castellano.aff
;; Ispell has a built-in list of dictionaries that it searches for, and it looks
;; for castellano rather than espa~nol, so it won't load the files until renamed.

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

;; show line numbers on all files
(global-linum-mode t)

;; copy buffer-file-name to clipboard
(defun c ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

;; keybinding to open magit
(global-set-key (kbd "C-c g") 'magit-status)

(defvar css-indent-offset)
(setq css-indent-offset 2)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("f3d6a49e3f4491373028eda655231ec371d79d6d2a628f08d5aa38739340540b" "316d29f8cd6ca980bf2e3f1c44d3a64c1a20ac5f825a167f76e5c619b4e92ff4" "85c59044bd46f4a0deedc8315ffe23aa46d2a967a81750360fb8600b53519b8a" "f5eb916f6bd4e743206913e6f28051249de8ccfd070eae47b5bde31ee813d55f" default)))
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

(require 'rvm)
(rvm-use-default) ;; use rvm's default ruby for the current Emacs session

(require 'rspec-mode)

(setq require-final-newline t)

;;; .emacs ends here
