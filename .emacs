;You need to define an environment variable that indicates where
;the primary install root of the dev tree.  The system expects
;slime, clojure, clojure-contrib swank-clojure, clojure-mode
;and berger-emacs all to be defined there.
;Berger-emacs is only required if you aren't running
;aquamacs.  I am not sure how to check that except
;assuming that darwin os is aquamacs

(defvar dev-install-root (getenv "DEV_INSTALL_ROOT"))
(defun dev-dir (str)
  (concat dev-install-root "/" str))

(mapc 
 (lambda (item) (add-to-list 'load-path (dev-dir item)))
 '( "slime"
    "clojure-mode"
    "swank-clojure"
    "berger-emacs"))

(when (not (eq 'darwin system-type))
  (add-to-list 'load-path (dev-dir "berger-emacs/non-aquamacs")))

(require 'slime)
(require 'clojure-mode)

;I expect any necessary jars to bin the classpath variable.
(defun get-classpath-list ()
  (if 
      (or
       (eq 'windows-nt system-type)
       (eq 'ms-dos system-type))
      (split-string (getenv "CLASSPATH") ";")
    (split-string (getenv "CLASSPATH") ":")))

(setq swank-clojure-jar-path 
      (dev-dir "clojure/clojure.jar"))
(setq swank-clojure-extra-classpaths 
      (get-classpath-list))


(require 'swank-clojure-autoload)
(global-set-key [f5] 'slime)
(slime-setup '(slime-repl))
(add-hook 'slime-connected-hook (lambda ()
				  (interactive)
				  (slime-redirect-inferior-output)))

(require 'linum)
(global-linum-mode 1)


(require 'color-theme)
(color-theme-initialize)
(setq color-theme-is-global t)
(color-theme-subtle-hacker)


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
