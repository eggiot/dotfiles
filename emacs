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

(require 'color-theme)
(color-theme-initialize)
(color-theme-snow)

;;; UI

;; set font
(set-frame-font "-microsoft-Consolas-normal-normal-normal-*-19-*-*-*-m-0-iso10646-1")

;; modeline modifications
; show the time
(display-time-mode t)
; show column number
(column-number-mode t)
; set italics
(set-face-italic-p 'modeline t)

;; no toolbar
(tool-bar-mode -1)

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

(require 'markdown-mode)

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

;(add-hook 'markdown-mode-hook 'distraction-free)
;(add-hook 'markdown-mode-hook 'sentence-highlight-mode)
;(add-hook 'markdown-mode-hook 'hide-mode-line)
;(cons '("\\.markdown" . markdown-mode) auto-mode-alist)

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

;; pretty symbols - individual languages to be implemented in respective sections
(defun unicode-symbol (name)
    "Translate a symbolic name for a Unicode character -- e.g., LEFT-ARROW
  or GREATER-THAN into an actual Unicode character code. "
    (decode-char 'ucs (case name
                        ;; arrows
                        ('left-arrow 8592)
                        ('up-arrow 8593)
                        ('right-arrow 8594)
                        ('down-arrow 8595)
                        ;; boxes
                        ('double-vertical-bar #X2551)
                        ;; relational operators
                        ('equal #X003d)
                        ('not-equal #X2260)
                        ('identical #X2261)
                        ('not-identical #X2262)
                        ('less-than #X003c)
                        ('greater-than #X003e)
                        ('less-than-or-equal-to #X2264)
                        ('greater-than-or-equal-to #X2265)
                        ;; logical operators
                        ('logical-and #X2227)
                        ('logical-or #X2228)
                        ('logical-neg #X00AC)
                        ;; misc
                        ('nil #X2205)
                        ('horizontal-ellipsis #X2026)
                        ('double-exclamation #X203C)
                        ('prime #X2032)
                        ('double-prime #X2033)
                        ('for-all #X2200)
                        ('there-exists #X2203)
                        ('element-of #X2208)
                        ;; mathematical operators
                        ('square-root #X221A)
                        ('squared #X00B2)
                        ('cubed #X00B3)
                        ;; letters
                        ('lambda #X03BB)
                        ('alpha #X03B1)
                        ('beta #X03B2)
                        ('gamma #X03B3)
                        ('delta #X03B4))))
                        
(defun substitute-pattern-with-unicode (pattern symbol)
  "Add a font lock hook to replace the matched part of PATTERN with the 
  Unicode symbol SYMBOL looked up with UNICODE-SYMBOL."
  (interactive)
  (font-lock-add-keywords
   nil `((,pattern (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                            ,(unicode-symbol symbol))
                            nil))))))
  
(defun substitute-patterns-with-unicode (patterns)
  "Call SUBSTITUTE-PATTERN-WITH-UNICODE repeatedly."
  (mapcar #'(lambda (x)
              (substitute-pattern-with-unicode (car x)
                                               (cdr x)))
          patterns))

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

;;; HASKELL

;; Pretty symbols

(defun haskell-unicode ()
  (interactive)
  (substitute-patterns-with-unicode
   (list (cons "\\(<-\\)" 'left-arrow)
         (cons "\\(->\\)" 'right-arrow)
         (cons "\\(==\\)" 'identical)
         (cons "\\(/=\\)" 'not-identical)
         (cons "\\(()\\)" 'nil)
         (cons "\\<\\(sqrt\\)\\>" 'square-root)
         (cons "\\(&&\\)" 'logical-and)
         (cons "\\(||\\)" 'logical-or)
         (cons "\\<\\(not\\)\\>" 'logical-neg)
         (cons "\\(>\\)\\[^=\\]" 'greater-than)
         (cons "\\(<\\)\\[^=\\]" 'less-than)
         (cons "\\s \\(>=\\)\\s " 'greater-than-or-equal-to)
         (cons "\\s \\(<=\\)\\s " 'less-than-or-equal-to)
         (cons "\\<\\(alpha\\)\\>" 'alpha)
         (cons "\\<\\(beta\\)\\>" 'beta)
         (cons "\\<\\(gamma\\)\\>" 'gamma)
         (cons "\\<\\(delta\\)\\>" 'delta)
         (cons "\\(''\\)" 'double-prime)
         (cons "\\('\\)" 'prime)
         (cons "\\(!!\\)" 'double-exclamation)
         (cons "\\(\\.\\.\\)" 'horizontal-ellipsis)
	 (cons "\\s (?\\(\\\\\\)\\s *\\(\\w\\|_\\).*?\\s *->" 'lambda))))
  
(add-hook 'haskell-mode 'haskell-unicode)
