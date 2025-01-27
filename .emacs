;;; You need to define an environment variable that indicates where
;;; the primary install root of the dev tree.  The system expects
;;; slime, clojure, clojure-contrib swank-clojure, clojure-mode
;;; and berger-emacs all to be defined there.
;;; Berger-emacs is only required if you aren't running
;;; aquamacs.  I am not sure how to check that except
;;; assuming that darwin os is aquamacs

(defvar dev-install-root "/User/chrispnuernberger/dev")
(defun dev-dir (str)
  (concat dev-install-root "/" str))

(mapc
 (lambda (item) (add-to-list 'load-path (dev-dir item)))
 '( "berger-emacs"))

(when (not (eq 'darwin system-type))
  (add-to-list 'load-path (dev-dir "berger-emacs/non-aquamacs")))

(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))


;(tool-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-splash-screen t)
;(set-face-attribute 'default nil :font "Anonymous Pro-11")
(setq make-backup-files nil)
(setq auto-save-default nil)

(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(setq package-list '(cider paredit zenburn-theme rainbow-delimiters company use-package markdown-mode yaml-mode flycheck-joker))

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


;; Tell use-package to always install missing packages if necessary.
(setq use-package-always-ensure t)

;; Tell use-package to always load packages lazily unless told
;; otherwise.
(setq use-package-always-defer t)


(require 'cider)

(require 'cl-lib)


;(projectile-global-mode)
(global-auto-revert-mode)
(auto-image-file-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("dd4db38519d2ad7eb9e2f30bc03fba61a7af49a185edfd44e020aa5345e3dca7" default))
 '(package-selected-packages
   '(clojure-mode 0blayout julia-mode flycheck-julia julia-repl flycheck-joker cider yaml-mode csharp-mode zenburn-theme use-package undo-tree smex rainbow-delimiters paredit markdown-mode flx counsel)))
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

(setq-default fill-column 84)

(add-hook 'after-init-hook #'global-flycheck-mode)
(require 'flycheck-joker)
;(add-hook 'clojure-mode-hook (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))
(add-hook 'clojure-mode-hook #'paredit-mode)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook (lambda () (set-fill-column 88)))
(add-hook 'cider-repl-mode-hook #'eldoc-mode)
(add-hook 'cider-mode-hook #'eldoc-mode)
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)
;(add-hook 'c-mode-hook (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

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


(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))


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

(put 'erase-buffer 'disabled nil)
(put 'downcase-region 'disabled nil)

;; buffer full file paths
(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (kill-new (buffer-file-name))
  (message (buffer-file-name)))

(global-set-key [C-f1] 'show-file-name)
