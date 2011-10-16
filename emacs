;; --------------------------------------------------
;; Emacs configuration
;;
;; Requires:
;;
;;     emacs-goodies.el package
;;     auto-complete package
;;     Python packages:
;;         python-mode
;;         python-ropemacs
;;         pyflakes
;;         pylint
;;         pep8
;;         python-doc
;;         ipython
;; --------------------------------------------------

;; --------------------------------------------------
;; General Setup
;; --------------------------------------------------

;; first, avoid the evil
(when (featurep 'xemacs)
  (error "This machine runs xemacs, install GNU Emacs first."))

;; set up load path

(add-to-list 'load-path "~/elisp/")

;; no splash screen

(setq inhibit-splash-screen t)

;; get a list of recent files when I start

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(recentf-open-files)

;; make scripts executabe on save

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; handle .gz files
(auto-compression-mode t)



;; --------------------------------------------------
;; Modes
;; --------------------------------------------------

;; Default mode - text mode and auto-fill

(setq default-major-mode 'text-mode)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; w3m

(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
(global-set-key "\C-xm" 'browse-url-at-point) ; keybinding - go to url at cursor
(setq w3m-use-cookies t)

;; mc
(require 'mc)

;; ido
(require 'ido)


;; -------------------------------------------------
;; Organisation
;; -------------------------------------------------

; setup org
(add-to-list 'load-path "~/elisp/org-mode/lisp")
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

; define key combinations
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

; set org-directory

(setq org-directory "~/org")

;; capture mode (remember mode)

(setq org-defaults-notes-file (concat org-directory "/notes.org"))
(define-key global-map "\C-cc" 'org-capture)

;; -------------------------------------------------
;; Programming languages
;; -------------------------------------------------

;; General

; parenthesis matching
(autoload 'autopair-global-mode "autopair" nil t)
(autopair-global-mode)
(add-hook 'lisp-mode-hook
	  #'(lambda () (setq autopair-dont-activate t))) ;except lisp - paredit

; change comint keys
(require 'comint)
(define-key comint-mode-map (kbd "M-") 'comint-next-input)
(define-key comint-mode-map (kbd "M-") 'comint-previous-input)
(define-key comint-mode-map [down] 'comint-next-matching-input-from-input)
(define-key comint-mode-map [up] 'comint-previous-matching-input-from-input)

; autocomplete
(require 'anything)

; autoindentation
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region-activates)
	   (and (not current-prefix-arg)
		(member major-mode '(emacs-lisp-mode slime
				     python-mode     c++-mode))
		(let ((mark-even-if-inactive transient-mark-mode))
		  (indent-region (region-beginning) (region-end) nil))))))

;; Python

; setup
(require 'python-mode)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(require 'ipython)

; pretty lambda symbol instead of the word lambda
(require 'lambda-mode)
(add-hook 'python-mode-hook #'lambda-mode 1)

; code completion with anything
(require 'anything-ipython)
(when (require 'anything-show-completion nil t)
  (use-anything-show-completion 'anything-ipython-complete
				'(length initial-pattern)))

; single and triple quote matching
(add-hook 'python-mode-hook
	  #'(lambda ()
	      (push '(?' . ?')
		    (getf autopair-extra-pairs :code))
	      (setq autopair-handle-actions-fns
		    (list #'autopair-default-handle-action
			  #'autopair-python-triple-quote-action))))

; pep8 and pylint
(require 'python-pep8)
(require 'python-pylint)

(add-hook 'before-save-hook 'delete-trailing-whitespace) ; delete trailing space - pep8


;; LISP

; slime
(add-to-list 'load-path "~/elisp/slime/")
(setq slime-backend "~/elisp/slime/swank-loader.lisp")
(require 'slime)
(add-hook 'lisp-mode-ook (lambda() (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda() (inferior-slime-mode t)))
; set lisp program as sbcl
(setq inferior-lisp-program "sbcl")

(add-to-list 'auto-mode-alist '("\\.lisp$" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.cl$" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.asd$" . lisp-mode))

(slime-setup '(slime-fancy))



;; --------------------------------------------------
;; Display
;; --------------------------------------------------

;;; COLOURS

;; load color-theme and choose one of my favorites at random, but only
;; if the emacs is a windowed variety

(require 'color-theme)
(color-theme-initialize)

(defvar current-color-theme
  "the current color-theme")            ;so I can tell current theme

(if (not window-system)
    nil
  (progn
    (require 'color-theme)
    (setq favorite-color-themes
          '((color-theme-gray30)
            (color-theme-hober)
            (color-theme-midnight)
            (color-theme-parus)
            (color-theme-sataram-solaris)
            (color-theme-taming-mr-arneson)))
    (random t)                          ;set the seed according to the
                                        ;system clock
    (setq current-color-theme
          (nth (random (length favorite-color-themes))
               favorite-color-themes))
    (eval current-color-theme)))


;;; UI

;; remove scrollbar
(scroll-bar-mode -1)

;; remove toolbar
(tool-bar-mode -1)


;; font
(set-default-font "-microsoft-Consolas-normal-normal-normal-*-16-*-*-*-m-9-iso10646-1")
