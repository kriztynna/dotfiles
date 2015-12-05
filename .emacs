(require 'package)
(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/")
 t)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(setq package-list '(flymake-jshint flymake-cursor))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(require 'flymake-jshint)
(add-hook 'js-mode-hook 'flymake-jshint-load)
(require 'flymake-cursor)
