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

(setq package-list '(cider paredit zenburn-theme rainbow-delimiters company use-package markdown-mode yaml-mode))

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


(require 'cl-lib)


;; Tell use-package to always install missing packages if necessary.
(setq use-package-always-ensure t)

;; Tell use-package to always load packages lazily unless told
;; otherwise.
(setq use-package-always-defer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; User-specific package management

;;; The following code sets up a simple way for users to selectively
;;; disable Radian's default packages in their init.before.local.el.
;;;
;;; Here we are using the defvar-nil-setq pattern described in [1],
;;; which makes it so that changes to this declaration will be picked
;;; up by a reload of init.el (M-RET r).
;;;
;;; [1]: http://ergoemacs.org/emacs/elisp_defvar_problem.html

(defvar radian-disabled-packages nil
  "Association list from packages to integers. Any package with
an entry greater than zero will not be automatically loaded by
Radian. Missing packages are treated as having entries of zero.

You can manipulate this list using `radian-disable-package',
which increments the entries of packages, and
`radian-reenable-package', which decrements the entries of
packages.

Note that this variable only affects the loading of packages that
are normally loaded automatically by Radian. If you want to
install your own package, add a `use-package' call to your
init.local.el.

Also note that you must make any modifications to this list
*before* packages are loaded, in order for your changes to have
an effect. This means your modifications should be done in
init.before.local.el.")
(setq radian-disabled-packages ())

(defun radian-disable-package (&rest packages)
  "Disables packages that would otherwise be loaded automatically
by Radian, by incrementing their entries in
`radian-disabled-packages'.

You can undo the effects of this function using
`radian-reenable-package'. In fact, you can even call
`radian-reenable-package' before this function is called, in
order to cancel out the effect preemptively.

Note that you must call this function *before* packages are
loaded, in order for your change to have an effect. This means
the call must be done in init.before.local.el."
  ;; Passing `t' to `add-to-list' makes the addition happen at the end
  ;; of the list.
  (dolist (package packages)
    (let ((association (assoc package radian-disabled-packages)))
      (if association
          (setcdr (assoc package radian-disabled-packages)
                  (1+ (cdr association)))
        (push (cons package 1) radian-disabled-packages)))))

(defun radian-reenable-package (&rest packages)
  "Undoes the effects of `radian-disable-package', by
decrementing the entries of the provided packages in
`radian-disabled-packages'.

In fact, you can even call this function before
`radian-disable-package' is called, in order to cancel out its
effect preemptively.

Note that you must call this function *before* packages are
loaded, in order for your change to have an effect. This means
the call must be done in init.before.local.el."
  (dolist (package packages)
    (let ((association (assoc package radian-disabled-packages)))
      (if association
          (setcdr (assoc package radian-disabled-packages)
                  (1+ (cdr association)))
        (push (cons package -1) radian-disabled-packages)))))


(defun radian-package-enabled-p (&rest packages)
  "Returns `t' if none of the given packages are disabled (for example,
by `radian-disable-package'), and `nil' otherwise.

See also `radian-disabled-packages'."
  (cl-every (lambda (package)
              (let ((association (assoc package radian-disabled-packages)))
                (or (not association)
                    (<= (cdr association) 0))))
            packages))

(defun radian-package-disabled-p (&rest packages)
  "Returns `t' if all of the given packages are disabled (for
example, by `radian-disable-package'), and `nil' otherwise.

See also `radian-disabled-packages'."
  (cl-every (lambda (package)
              (let ((association (assoc package radian-disabled-packages)))
                (and association
                     (> (cdr association) 0))))
            packages))


(defvar radian-quelpa-overrides nil
  "Association list from packages to quelpa forms. This alist
allows you to override where packages are loaded from. If the
entry for a package is nil (as distinct from a package not being
in the alist), then quelpa is not used to load the package even
if `:quelpa' is specified in the `use-package' form. If a package
has a non-nil entry, the package is loaded using quelpa no matter
what, and the entry is used as the recipe to pass to the
`:quelpa' keyword.

You can manipulate this list using `radian-override-quelpa' and
`radian-disable-quelpa-override'.

Note that this variable only affects the loading of packages that
are normally loaded automatically by Radian. If you want to load
your own package from quelpa, just put a `:quelpa' keyword in
your `use-package' form for that package.

Also note that you must make any modifications to this list
*before* packages are loaded, in order for your changes to have
an effect. This means your modifications should be done in
init.before.local.el.")
(setq radian-quelpa-overrides ())



(defun radian--use-package-allow-quelpa-overrides (use-package name &rest args)
  "Allow the value of `radian-quelpa-overrides' to have an effect
on the `:quelpa' keyword.

Note that this advice is not implemented very intelligently, so
it will only work correctly if `:quelpa' is specified as the
first keyword in the `use-package' form (except that it can be
after `:dependencies')."
  ;; First we want to check if there's an entry for the package in
  ;; `radian-quelpa-overrides'.
  (let ((association (assoc name radian-quelpa-overrides)))
    (if association
        ;; If there is an association, we'll extract the recipe.
        (let ((recipe (cdr association)))
          (if recipe
              ;; If the recipe is non-nil it means the user wants to
              ;; use quelpa for the package.
              (if (equal (car args) :quelpa)
                  (if (keywordp (cadr args))
                      ;; This happens for (use-package name :quelpa
                      ;; :config ...).
                      (apply use-package name :quelpa recipe (cdr args))
                    ;; This happens for (use-package name
                    ;; :quelpa (recipe) :config ...).
                    (apply use-package name :quelpa recipe (cddr args)))
                ;; This happens for (use-package name :config ...).
                (apply use-package name :quelpa recipe args))
            ;; If the recipe is nil it means the user doesn't want to
            ;; use quelpa for the package.
            (if (equal (car args) :quelpa)
                (if (keywordp (cadr args))
                    ;; This happens for (use-package name :quelpa
                    ;; :config ...).
                    (apply use-package name (cdr args))
                  ;; This happens for (use-package name
                  ;; :quelpa (recipe) :config ...).
                  (apply use-package name (cddr args)))
              ;; This happens for (use-package name :config ...), in
              ;; which case there's nothing that needs to be changed.
              (apply use-package name args))))
      ;; If there's no special behavior requested, we can just pass
      ;; control to `use-package'.
      (apply use-package name args))))


(advice-add 'use-package
            :around 'radian--use-package-allow-quelpa-overrides
            '((:depth . -1)))


(defun radian--use-package-add-dependencies (use-package name &rest args)
  "Only initialize and load a package if it is enabled (according
to `radian-package-enabled-p'). If the first argument specified
after the package name is :dependencies, then also require any
packages specified in the list of symbols immediately
following :dependencies to be enabled.

Note that this advice is not implemented very intelligently, so
it will only work correctly if `:dependencies' is specified as
the first keyword in the `use-package' form."
  (when (radian-package-enabled-p name)
    (if (equal (car args) :dependencies)
        (when (cl-every 'radian-package-enabled-p (cadr args))
          (apply use-package name (cddr args)))
      (apply use-package name args))))

(advice-add 'use-package
            :around 'radian--use-package-add-dependencies
            '((:depth . -2)))



;(projectile-global-mode)
(global-auto-revert-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("dd4db38519d2ad7eb9e2f30bc03fba61a7af49a185edfd44e020aa5345e3dca7" default)))
 '(package-selected-packages
   (quote
    (yaml-mode csharp-mode zenburn-theme use-package undo-tree smex rainbow-delimiters paredit markdown-mode flx counsel cider))))
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

(setq-default fill-column 88)


(add-hook 'clojure-mode-hook (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))
(add-hook 'clojure-mode-hook #'paredit-mode)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook (lambda () (set-fill-column 88)))
(add-hook 'cider-repl-mode-hook #'eldoc-mode)
(add-hook 'cider-mode-hook #'eldoc-mode)
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)
(add-hook 'c-mode-hook (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

(delete-selection-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; C-like languages

;; Get rid of the submode indicators in the mode line. This transforms
;; e.g. "C++/l" into "C++". Since we are overriding a function
;; provided by `cc-mode', which is not initially loaded, we have to
;; make sure to do so *after* it is loaded and not before.
(eval-after-load 'cc-mode
  (lambda ()
    (advice-add #'c-update-modeline :override #'ignore)))

;; Switch to a better indentation-and-braces style. This turns the
;; following code:
;;
;; if (condition)
;;   {
;;     statement;
;;   }
;;
;; Into this:
;;
;; if (condition) {
;;   statement;
;; }
(eval-after-load 'cc-mode
  '(progn
     (if (assoc 'other c-default-style)
         (setcdr (assoc 'other c-default-style)
                 "k&r")
       (push '(other . "k&r") c-default-style))
     (setq-default c-basic-offset 2)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Packages: Completion systems

;; Sorts M-x completions by usage. Automatically used by Ivy if
;; available.
(use-package smex
  :bind (;; Use smex for M-x.
         ("M-x" . smex)))

;; Provides intelligent fuzzy matching and sorting mechanisms that
;; can be used by various other packages, including Ivy.
(use-package flx)

;; Provides undo/redo commands that are both more intuitive and more
;; powerful than the Emacs defaults. Allows you to visualize the
;; undo/redo tree, which uses a branching model to ensure that you can
;; never lose changes.
(use-package undo-tree
  :demand t
  :config

  ;; Enable Undo Tree everywhere.
  (global-undo-tree-mode 1)

  ;; Don't show Undo Tree in the mode line.
  (setq undo-tree-mode-lighter nil)

  ;; Suppress the message saying that the undo history file was
  ;; saved (because this happens every single time you save a file).

  (defun radian--undo-tree-suppress-undo-history-saved-message
      (undo-tree-save-history &rest args)
    (let ((inhibit-message t))
      (apply undo-tree-save-history args)))

  (advice-add #'undo-tree-save-history :around
              #'radian--undo-tree-suppress-undo-history-saved-message)

  ;; Suppress the message saying that the undo history could not be
  ;; loaded because the file changed outside of Emacs.

  (defun radian--undo-tree-suppress-buffer-modified-message
      (undo-tree-load-history &rest args)
    (let ((inhibit-message t))
      (apply undo-tree-load-history args)))

  (advice-add #'undo-tree-load-history :around
              #'radian--undo-tree-suppress-buffer-modified-message)

  :bind (;; By default, `undo' (and by extension `undo-tree-undo') is bound
         ;; to C-_ and C-/, and `undo-tree-redo' is bound to M-_. It's
         ;; logical to also bind M-/ to `undo-tree-redo'. This overrides the
         ;; default binding of M-/, which is to `dabbrev-expand'.
         ("M-/" . undo-tree-redo)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Mode line

;;; The following code customizes the mode bar to something like:
;;; [*] init.el        72% (389,30)  [radian]  (Emacs-Lisp Paredit AggrIndent)

(defvar mode-line-modified-radian
  '(:eval (propertize (if (and (buffer-modified-p)
                               (buffer-file-name))
                          "[*]" "   ")
                      ;; make sure to show it in the same color as the
                      ;; buffer name
                      'face 'mode-line-buffer-id))
  "Construct for the mode line that shows [*] if the buffer
has been modified, and whitespace otherwise.")


(setq-default mode-line-format
              (list
               ;; Show a warning if Emacs is low on memory.
               "%e"
               ;; Show [*] if the buffer is modified.
               mode-line-modified-radian
               " "
               ;; Show the name of the current buffer.
               mode-line-buffer-identification
               "   "
               ;; Show the row and column of point.
               mode-line-position
               ;; Show the active major and minor modes.
               "  "
               mode-line-modes))

;; Make `mode-line-position' show the column, not just the row.
(column-number-mode 1)




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
     (define-key cider-mode-map (kbd "M-?") 'cider-find-var)
     ))

;; (if (getenv "TMUX")
;;     (progn
;;       (let ((x 2) (tkey ""))
;; 	(while (<= x 8)
;; 	  ;; shift
;; 	  (if (= x 2)
;; 	      (setq tkey "S-"))
;; 	  ;; alt
;; 	  (if (= x 3)
;; 	      (setq tkey "M-"))
;; 	  ;; alt + shift
;; 	  (if (= x 4)
;; 	      (setq tkey "M-S-"))
;; 	  ;; ctrl
;; 	  (if (= x 5)
;; 	      (setq tkey "C-"))
;; 	  ;; ctrl + shift
;; 	  (if (= x 6)
;; 	      (setq tkey "C-S-"))
;; 	  ;; ctrl + alt
;; 	  (if (= x 7)
;; 	      (setq tkey "C-M-"))
;; 	  ;; ctrl + alt + shift
;; 	  (if (= x 8)
;; 	      (setq tkey "C-M-S-"))
;; 	  (define-key key-translation-map (kbd (format "M-[ 1 ; %d A" x)) (kbd (format "%s<up>" tkey)))
;; 	  (define-key key-translation-map (kbd (format "M-[ 1 ; %d B" x)) (kbd (format "%s<down>" tkey)))
;; 	  (define-key key-translation-map (kbd (format "M-[ 1 ; %d C" x)) (kbd (format "%s<right>" tkey)))
;; 	  (define-key key-translation-map (kbd (format "M-[ 1 ; %d D" x)) (kbd (format "%s<left>" tkey)))
;; 	  (setq x (+ x 1))))))
(put 'erase-buffer 'disabled nil)
