;;; config/yas-keymaps.el -*- lexical-binding: t; -*-

(defconst yas-maybe-skip-and-clear-field
  '(menu-item "" yas-skip-and-clear-field
              :filter yas--maybe-clear-field-filter)
  "A conditional key definition.
This can be used as a key definition in keymaps to bind a key to
`yas-skip-and-clear-field' only when at the beginning of an
unmodified snippet field.")

(defconst yas-maybe-clear-field
    '(menu-item "" yas-clear-field
                :filter yas--maybe-clear-field-filter)
    "A conditional key definition.
This can be used as a key definition in keymaps to bind a key to
`yas-clear-field' only when at the beginning of an
unmodified snippet field.")

(defun yas-filtered-definition (def)
  "Return a condition key definition.
The condition will respect the value of `yas-keymap-disable-hook'."
  `(menu-item "" ,def
              :filter ,(lambda (cmd) (unless (run-hook-with-args-until-success
                                         'yas-keymap-disable-hook)
                                  cmd))))
(defvar yas-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [(tab)]       (yas-filtered-definition 'yas-next-field-or-maybe-expand))
    (define-key map (kbd "TAB")   (yas-filtered-definition 'yas-next-field-or-maybe-expand)) ;; TAB is C-i
    (define-key map (kbd "C-S-i") (yas-filtered-definition 'yas-prev-field))
    (define-key map [(shift tab)] (yas-filtered-definition 'yas-prev-field))
    (define-key map [backtab]     (yas-filtered-definition 'yas-prev-field))
    (define-key map (kbd "C-g")   (yas-filtered-definition 'yas-abort-snippet))
    (define-key map (kbd "C-d")   (yas-filtered-definition yas-maybe-skip-and-clear-field))
    (define-key map (kbd "DEL")   (yas-filtered-definition yas-maybe-clear-field))
    map))

