(require 'generic-x)

(defun remove-distractions ()
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (toggle-fullscreen))

(defun change-appearance ()
  (load-theme 'minimalist)
  (set-face-attribute 'default nil :font "Adobe Caslon Pro-19"))

(define-generic-mode
  'emacsroom
  ()
  ()
  ()
  ()
  ;functions to call

  (remove-distractions)
  (change-appearance)

  "A distraction-free mode"
  )