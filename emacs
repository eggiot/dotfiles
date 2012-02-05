;; --------------------------------------------------
;; file: ~/.emacs
;; author: Eliot Walker
;; modified: October 2011
;; --------------------------------------------------

;; --------------------------------------------------
;; General Setup
;; --------------------------------------------------

;; when in xemacs
(when (featurep 'xemacs)
  (error "This machine runs xemacs, install GNU Emacs first."))

;; set up load path
(add-to-list 'load-path "~/elisp/")

;; no splash screen
(setq inhibit-splash-screen t)

;; make scripts executabe on save
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; stop \C-Z from backgrounding / minimising emacs
(global-set-key "\C-Z" nil)

;; --------------------------------------------------
;; Display
;; --------------------------------------------------

;;; Frame
;; Set initial size and shape
(defun set-frame-size-shape (w h x y)
  "Set the width, height and x/y position of the
current frame"
  (let ((frame (selected-frame)))
    (delete-other-windows)
    (set-frame-position frame x y)
    (set-frame-size frame w h)))

(set-frame-size-shape 65 30 0 0)

;;; Colours

(load-theme 'zenburn)

;; load color-theme
;(require 'color-theme)
;(require 'minimalist-theme)
;(color-theme-initialize)
;(setq color-theme-is-global nil)

;(if (not window-system)
;    nil
;  (color-theme-taylor)
;)

;;; UI

;; modeline modifications
; show the time
(display-time-mode t)
; show column number
(column-number-mode t)
; set italics
(set-face-italic-p 'modeline t)


;; cursor
(require 'bar-cursor)
(bar-cursor-mode 1)

;; fullscreen
(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
       (set-frame-parameter nil 'fullscreen
                            (if (equal 'fullboth current-value)
                                (if (boundp 'old-fullscreen) old-fullscreen nil)
                                (progn (setq old-fullscreen current-value)
				       'fullboth)))))

(global-set-key [f11] 'toggle-fullscreen)

;; --------------------------------------------------
;; Modes
;; --------------------------------------------------

;;; Default mode - text mode and auto-fill
(setq default-major-mode 'text-mode)
(add-hook 'text-mode-hook 'visual-line-mode)

;;; Distraction-free writing in markdown mode
;; hide modeline
(require 'hide-mode-line)
;; sentence detection
(require 'sentence-highlight)

(defun distraction-free ()
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (toggle-fullscreen)
  (load-theme 'minimalist)
  (set-face-attribute 'default nil :font "Adobe Caslon Pro-19"))

(add-hook 'markdown-mode-hook 'distraction-free)
(add-hook 'markdown-mode-hook 'sentence-highlight-mode)
(add-hook 'markdown-mode-hook 'hide-mode-line)
(cons '("\\.markdown" . markdown-mode) auto-mode-alist)

;; --------------------------------------------------
;; Programming languages
;; --------------------------------------------------

;;; General

;; change comint keys
(require 'comint)
(define-key comint-mode-map (kbd "M-") 'comint-next-input)
(define-key comint-mode-map (kbd "M-") 'comint-previous-input)
(define-key comint-mode-map [down] 'comint-next-matching-input-from-input)
(define-key comint-mode-map [up] 'comint-previous-matching-input-from-input)

;; autocomplete
(add-to-list 'load-path "~/elisp/auto-complete")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/elisp/auto-complete/ac-dict")
(ac-config-default)

;; auto-indentation
(defun set-newline-and-indent ()
  (local-set-key (kbd "RET") 'newline-and-indent))
(add-hook 'lisp-mode-hook 'set-newline-and-indent)

;;; LISP

;; slime
(add-to-list 'load-path "~/elisp/slime/")
(setq slime-backend "~/elisp/slime/swank-loader.lisp")
(require 'slime)
(add-hook 'lisp-mode-hook (lambda() (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda() (inferior-slime-mode t)))

(setq inferior-lisp-program "sbcl")

(add-to-list 'auto-mode-alist '("\\.lisp$" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.cl$" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.asd$" . lisp-mode))

(slime-setup '(slime-fancy))

;; Paredit - all forms of lisp should use this rather than autopair

; enable paredit
(autoload 'paredit-mode "paredit" t)
(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'lisp-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'scheme-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'slime-mode-hook (lambda() (paredit-mode +1)))
(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))

; stop slime's repl from grabbing DEL
; which is annying when backspacing over a '('
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))

(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)