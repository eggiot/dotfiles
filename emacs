;; --------------------------------------------------
;; file: ~/.emacs
;; author: Eliot Walker
;; modified: September 2018
;; --------------------------------------------------


(require 'cl)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(load-file "~/.emacs.d/sensible-defaults.el")
(sensible-defaults/use-all-settings)
(sensible-defaults/use-all-keybindings)
(sensible-defaults/backup-to-temp-directory)

(setq ido-enable-flex-matching t)

(setq ido-everywhere t)

(ido-mode 1)

(flx-ido-mode 1)

(setq ido-create-new-buffer 'always)

(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

(smex-initialize)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(defun ejw/add-hook-to-all (l f)
  (dolist (mode l)
    (add-hook mode f)))

(defun ejw/cycle-set (v l)
  ;; if the variable is already bound
  (if (boundp v)
      (let* (
              ;; the index of the last element
              (end-index (- (length l) 1))
              ;; the index of v within the list (nil if v is not in the list)
              (v-pos (position (eval v) l))
              ;; the index of the next value in the list
              (next-index (cond
                           ;; v is in the list and not at the end -> next element
                           ((and v-pos (< v-pos end-index)) (+ 1 v-pos))
                           ;; v is in the list and at the end -> beginning
                           ((and v-pos (= v-pos end-index)) 0)
                           ;; v is not in the list -> beginning
                           ((not v-pos) 0)))
              ;; get the next value in the list
              (next-value (nth next-index l)))
         ;; set the value of v to next-value
         (set v next-value))
    ;; if the variable is not already bound
    (set v (first l))))

(defmacro ejw/cycle-setq (v l)
  `(ejw/cycle-set (quote ,v) ,l))

(setf user-full-name "Eliot J. Walker"
      user-mail-address "eliot@eliotjwalker.com"
      calendar-location-name "Solihull, U.K."
      calendar-latitude 52.3
      calendar-longitude -1.8)

(setq inhibit-startup-message t)
(tool-bar-mode -1)
(toggle-scroll-bar -1)

(setq ring-bell-function 'ignore)

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq-default cursor-type 'bar)

(setq scroll-conservatively 100)

(defmacro diminish-minor-mode (filename mode &optional abbrev)
  `(eval-after-load (symbol-name ,filename)
     '(diminish ,mode ,abbrev)))

(defmacro diminish-major-mode (mode-hook abbrev)
  `(add-hook ,mode-hook
             (lambda () (setq mode-name ,abbrev))))

(diminish-minor-mode 'abbrev 'abbrev-mode)
(diminish-minor-mode 'simple 'auto-fill-function)
(diminish-minor-mode 'company 'company-mode)
(diminish-minor-mode 'eldoc 'eldoc-mode)
(diminish-minor-mode 'flycheck 'flycheck-mode)
(diminish-minor-mode 'flyspell 'flyspell-mode)
(diminish-minor-mode 'global-whitespace 'global-whitespace-mode)
(diminish-minor-mode 'projectile 'projectile-mode)
(diminish-minor-mode 'ruby-end 'ruby-end-mode)
(diminish-minor-mode 'subword 'subword-mode)
(diminish-minor-mode 'undo-tree 'undo-tree-mode)
(diminish-minor-mode 'yard-mode 'yard-mode)
(diminish-minor-mode 'yasnippet 'yas-minor-mode)
(diminish-minor-mode 'wrap-region 'wrap-region-mode)

(diminish-minor-mode 'paredit 'paredit-mode " π")

(diminish-major-mode 'emacs-lisp-mode-hook "el")
(diminish-major-mode 'haskell-mode-hook "λ=")
(diminish-major-mode 'lisp-interaction-mode-hook "λ")
(diminish-major-mode 'python-mode-hook "Py")

(setq sml/theme 'powerline)

(sml/setup)

(setq default-font-size 24
      default-font "Inconsolata-18")

(when (member default-font (font-family-list))
  (add-to-list 'initial-frame-alist '(font . default-font))
  (add-to-list 'default-frame-alist '(font . default-font)))

(set-face-attribute 'default nil :height (* default-font-size 10))

(require 'theme-changer)
(require 'color-theme-sanityinc-tomorrow)

(setf day-theme 'sanityinc-tomorrow-day
      night-theme 'sanityinc-tomorrow-eighties)

(change-theme day-theme night-theme)

(evil-mode 1)

(global-evil-surround-mode 1)

(projectile-global-mode)

(add-hook 'text-mode-hook 'flyspell-mode)

(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

(global-prettify-symbols-mode +1)
(setq prettify-symbols-unprettify-at-point t)

(defun unicode-symbol (name)
  (case name
        ;; arrows
        ('left-arrow "←")
        ('up-arrow "↑")
        ('right-arrow "→")
        ('down-arrow "↓")
        ;; boxes
        ('double-vertical-bar "║")
        ;; relational operators
        ('equal "=")
        ('not-equal "≠")
        ('identical "≡")
        ('not-identical "≢")
        ('less-than "<")
        ('greater-than ">")
        ('less-than-or-equal-to "≤")
        ('greater-than-or-equal-to "≥")
        ;; logical operators
        ('logical-and "∧")
        ('logical-or "∨")
        ('logical-neg "¬")
        ;; misc
        ('nil "∅")
        ('horizontal-ellipsis "…")
        ('double-exclamation "‼")
        ('prime "′")
        ('double-prime "″")
        ('for-all "∀")
        ('there-exists "∃")
        ('element-of "∈")
        ;; mathematical operators
        ('square-root "√")
        ('squared "²")
        ('cubed "³")
        ;; letters
        ('lambda "λ")
        ('alpha "α")
        ('beta "β")
        ('gamma "γ")
        ('delta "δ")))

(defmacro add-pretty-symbol (ascii-string unicode-name)
  `(push (append '(,ascii-string) (unicode-symbol ,unicode-name)) prettify-symbols-alist))

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(elpy-enable)

(setq lisp-hooks '(emacs-lisp-mode-hook
                   lisp-mode-hook
                   scheme-mode-hook
                   slime-mode-hook
                   slime-repl-hook))

(autoload 'paredit-mode "paredit" t)

(ejw/add-hook-to-all lisp-hooks 'paredit-mode)

(add-to-list 'auto-mode-alist '("\\.lisp$" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.cl$" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.asd$" . lisp-mode))

(add-hook 'lisp-mode-hook (lambda() (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda() (inferior-slime-mode t)))

(setq inferior-lisp-program "/opt/local/bin/sbcl")

(setq slime-contribs '(slime-fancy))

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)

(add-hook 'prog-mode-hook
          (lambda ()
            (add-pretty-symbol "->" 'right-arrow)
            (add-pretty-symbol "<-" 'left-arrow)
            (add-pretty-symbol "==" 'identical)
            (add-pretty-symbol "/=" 'not-identical)
            (add-pretty-symbol "<=" 'less-than-or-equal-to)
             (add-pretty-symbol ">=" 'greater-than-or-equal-to)))

(add-hook 'org-mode-hook
          (lambda ()
            (org-bullets-mode t)))

(setq org-ellipsis "⤶")

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (lisp . t)
   (python . t)
   (haskell . t)
   (js . t)))

(setq org-src-fontify-natively t
      org-src-preserve-indentation t
      org-src-tab-acts-natively t)

(add-to-list 'org-structure-template-alist
             '("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"))

(require 'ox-md)
(require 'ox-beamer)

(setq org-export-with-smart-quotes t)

(setq org-html-postamble nil)

(setenv "BROWSER" "open /Applications/Google\ Chrome.app")

(setq org-latex-pdf-process
      '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)

(defvar custom-bindings-map (make-keymap)
  "A keymap for custom bindings.")

(define-key custom-bindings-map (kbd "C->")  'er/expand-region)
(define-key custom-bindings-map (kbd "C-<")  'er/contract-region)

(define-minor-mode custom-bindings-mode
  "A mode that activates custom-bindings."
  t nil custom-bindings-map)
