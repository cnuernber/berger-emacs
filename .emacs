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
(setq auto-save-default nil)

(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(setq package-list '(cider paredit zenburn-theme rainbow-delimiters company))

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
      (package-install package)))



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

(setq cider-show-error-buffer nil)
(show-paren-mode 1)
(setq cider-repl-use-pretty-printing t)
(setq cider-pprint-fn 'puget)
(setq cider-repl-display-in-current-window t)
(windmove-default-keybindings)


;; handle tmux's xterm-keys
;; put the following line in your ~/.tmux.conf:
;;   setw -g xterm-keys on


(add-hook 'clojure-mode-hook (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))
(add-hook 'clojure-mode-hook #'paredit-mode)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'cider-repl-mode-hook #'eldoc-mode)
(add-hook 'cider-mode-hook #'eldoc-mode)
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)

(delete-selection-mode 1)



;Unset keys I care about that i want to use for paredit
(dolist (key (list (kbd "M-.") (kbd "M-,") (kbd "M-<") (kbd "M->")))
  (global-unset-key key))


(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "<C-right>") nil)
     (define-key paredit-mode-map (kbd "<C-left>")  nil)
     (define-key paredit-mode-map (kbd "M-.") 'paredit-forward-slurp-sexp)
     (define-key paredit-mode-map (kbd "M-,") 'paredit-backward-slurp-sexp)
     (define-key paredit-mode-map (kbd "M->") 'paredit-forward-barf-sexp)
     (define-key paredit-mode-map (kbd "M-<") 'paredit-backward-barf-sexp)
     ))

(eval-after-load 'cider
  '(progn
     (define-key cider-mode-map (kbd "M-.") nil)
     (define-key cider-mode-map (kbd "M-,") nil)
     (define-key cider-mode-map (kbd "M-/") 'cider-find-var)
     (define-key cider-mode-map (kbd "M-?") 'cider-pop-back)
     ))

(if (getenv "TMUX")
    (progn
      (let ((x 2) (tkey ""))
	(while (<= x 8)
	  ;; shift
	  (if (= x 2)
	      (setq tkey "S-"))
	  ;; alt
	  (if (= x 3)
	      (setq tkey "M-"))
	  ;; alt + shift
	  (if (= x 4)
	      (setq tkey "M-S-"))
	  ;; ctrl
	  (if (= x 5)
	      (setq tkey "C-"))
	  ;; ctrl + shift
	  (if (= x 6)
	      (setq tkey "C-S-"))
	  ;; ctrl + alt
	  (if (= x 7)
	      (setq tkey "C-M-"))
	  ;; ctrl + alt + shift
	  (if (= x 8)
	      (setq tkey "C-M-S-"))
	  (define-key key-translation-map (kbd (format "M-[ 1 ; %d A" x)) (kbd (format "%s<up>" tkey)))
	  (define-key key-translation-map (kbd (format "M-[ 1 ; %d B" x)) (kbd (format "%s<down>" tkey)))
	  (define-key key-translation-map (kbd (format "M-[ 1 ; %d C" x)) (kbd (format "%s<right>" tkey)))
	  (define-key key-translation-map (kbd (format "M-[ 1 ; %d D" x)) (kbd (format "%s<left>" tkey)))
	  (setq x (+ x 1))))))
