;You need to define an environment variable that indicates where
;the primary install root of the dev tree.  The system expects
;slime, clojure, clojure-contrib swank-clojure, clojure-mode
;and berger-emacs all to be defined there.
;Berger-emacs is only required if you aren't running
;aquamacs.  I am not sure how to check that except
;assuming that darwin os is aquamacs

(defvar dev-install-root "/home/chrisn/dev")
(defun dev-dir (str)
  (concat dev-install-root "/" str))

(mapc 
 (lambda (item) (add-to-list 'load-path (dev-dir item)))
 '( "berger-emacs"))

(when (not (eq 'darwin system-type))
  (add-to-list 'load-path (dev-dir "berger-emacs/non-aquamacs")))


(require 'linum)
(global-linum-mode 1)


(require 'tabbar)
(dolist (func '(tabbar-mode tabbar-forward-tab tabbar-forward-group tabbar-backward-tab tabbar-backward-group))
      (autoload func "tabbar" "Tabs at the top of buffers and easy control-tab navigation"))
     
(defmacro defun-prefix-alt (name on-no-prefix on-prefix &optional do-always)
  `(defun ,name (arg)
     (interactive "P")
     ,do-always
     (if (equal nil arg)
	 ,on-no-prefix
       ,on-prefix)))
     
(defun-prefix-alt shk-tabbar-next (tabbar-forward-tab) (tabbar-forward-group) (tabbar-mode 1))
(defun-prefix-alt shk-tabbar-prev (tabbar-backward-tab) (tabbar-backward-group) (tabbar-mode 1))
     
(global-set-key [(control tab)] 'shk-tabbar-next)
(global-set-key [(control shift tab)] 'shk-tabbar-prev)

(tool-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-splash-screen t)
;(set-face-attribute 'default nil :font "Anonymous Pro-11")
(setq make-backup-files nil)

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
;(projectile-global-mode)
(global-auto-revert-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("dd4db38519d2ad7eb9e2f30bc03fba61a7af49a185edfd44e020aa5345e3dca7" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(load-theme 'zenburn t)
