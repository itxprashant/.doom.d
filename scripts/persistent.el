

;;; persistent.el --- Create persistent menu buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;; Keywords: convenience, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'widget)
(eval-when-compile (require 'wid-edit))

(defvar-local widget-example-repeat nil)

(defface persistent-variable '((t :inherit custom-variable-tag
                                :height 1.2
                                :weight semi-bold))
  "Face for Persistent menu headers.")

(custom-set-faces
 `(widget-button ((t (:foreground nil)))))

(defmacro with-visible-org-buffer (body)
  `(if-let
       ((win (seq-find (lambda (w)
                         (eq
                          (buffer-mode (window-buffer w))
                          'org-mode))
              (window-list))))
       (with-current-buffer (window-buffer win)
        (progn ,body))
     (message "No org-buffer visible!")))

(defmacro persistent-choice (desc val choices notify-func)
  `(progn
    (widget-create 'menu-choice
     :format
     (concat "%{%t%}"
      (propertize " " 'display
       '(space :align-to 20))
      "%[%v%]")
     :tag ,desc
     :sample-face 'persistent-variable
     :value ,val
     ;; :help-echo "Choose color theme"
     :notify #',notify-func
     ,@(cl-loop for (choice-tag . choice-val) in choices
        collect
        `'(choice-item :tag ,choice-tag :value ,choice-val)))
    (widget-insert "\n")))

(defmacro persistent-toggler (desc var &optional var-values on-string off-string)
  `(progn
     (widget-insert (propertize ,desc 'face 'persistent-variable))
     (widget-insert (propertize " " 'display '(space :align-to 20)))
     ,(if (not var-values)
          `(widget-create 'toggle
            :value (with-visible-org-buffer ,var)
            :on (concat
                 (propertize ,(or on-string " on ")
                  'face '(:inherit success :box t
                          :weight semi-bold :slant italic
                          :height 1.2)))
            :off (concat
                  (propertize ,(or off-string " off ")
                   'face '(:inherit error :box t
                           :weight semi-bold
                           :height 1.2 )))
            :notify
            (lambda (widget &rest ignore)
              (with-visible-org-buffer
               (if (commandp ',var)
                   (,var (if (widget-value widget) 1 0))
                 (setq ,var (widget-value widget))))))
        `(widget-create 'toggle
          :value (eq ,var ',(caar var-values))
          :on
          (concat
           (propertize ,(cdr (car var-values))
            'face '(:box t :weight semi-bold :slant italic
                    :inherit success :height 1.2))
           "  "
           (propertize ,(cdr (cadr var-values))
            'face 'shadow
                  ;; '(:box t :weight semi-bold
                  ;;   :inherit shadow :height 1.2)
            ))
          :off (concat
                (propertize ,(cdr (car var-values))
                 'face 'shadow
                       ;; '(:box t :weight semi-bold
                       ;;   :inherit shadow :height 1.2)
                       )
                "  "
                (propertize ,(cdr (cadr var-values))
                 'face '(:box t :weight semi-bold :slant italic
                         :inherit success :height 1.2)))
          :notify (lambda (widget &rest _)
                    (setq org-latex-preview-default-process
                     (if (widget-value widget)
                         ',(caar var-values) ',(caadr var-values))))))
     (widget-insert "\n")))

(defun persistent-toggle ()
  "Show or hide the persistent menu."
  (interactive)
  (if-let ((win (cl-some (lambda (w)
                           (and (string= (buffer-name (window-buffer w))
                                   "*persistent*")
                                w))
                         (window-list))))
      (delete-window win)
    (persistent-make-buffer)))

(define-key org-mode-map (kbd "<f6>") #'persistent-toggle)

(defun persistent-make-buffer ()
  "TODO: "
  (interactive)
  (with-current-buffer (get-buffer-create "*persistent*")
    (let ((display-buffer-mark-dedicated t))
      (display-buffer (current-buffer)
                      '(display-buffer-in-side-window
                        (slot . -20)
                        (direction . left)
                        (side . left)
                        (window-width . 50)
                        (window-parameters
                         (dedicated . t)
                         (no-delete-other-windows . t)))))
    (let ((inhibit-read-only t)) (erase-buffer))
    (remove-overlays)
    (widget-insert "\n\n           ")
    (widget-create 'push-button
                   :format "%{%[[PREVIEW!]%]%}"
                   ;; :button-prefix "       "
                   :sample-face '(:inherit custom-button-unraised
                                  :height 2.0 :box (:line-width 2))
                   :help-echo "Preview LaTeX fragments in document"
                   :notify (lambda (widget &rest _)
                             (with-visible-org-buffer
                              (org-latex-preview '(16)))))
    (widget-insert "\n\n      ")
    (widget-create 'push-button
                   :format "%{%[[Clear Preview]%]%}"
                   :sample-face 'custom-button-unraised
                   :help-echo "Clear out LaTeX fragment previews in document."
                   :notify (lambda (widget &rest _)
                             (with-visible-org-buffer
                              (org-latex-preview '(64)))))
    (widget-insert "  ")
    (widget-create 'push-button
                   :format "%{%[[Clear Cache]%]%}"
                   :sample-face 'custom-button-unraised
                   :help-echo "Clear out LaTeX preview cache."
                   :notify (lambda (widget &rest _)
                             (with-visible-org-buffer
                              (org-latex-preview-clear-cache (point-min) (point-max)))))
    (widget-insert "\n\n\n\n")
    (persistent-toggler "Image type"
                        org-latex-preview-default-process
                        ((dvisvgm . " SVG ")
                         (dvipng . " PNG ")))
    (persistent-toggler "Image cache" org-latex-preview-persist
                        nil " Persist " " Session ")
    (persistent-toggler "Auto-mode" org-latex-preview-auto-mode)
    (persistent-toggler "Eq tracking" org-latex-preview-numbered)
    (widget-insert
     (concat
      (propertize "Org File" 'face 'persistent-variable)
      "\n"))
    (widget-create
     'radio-button-choice
     :entry-format (propertize "%b  %v" 'face '(:height 1.25 :weight semi-bold))
     :value "demo.org"
     :notify (lambda (widget &rest ignore)
               (message "Switch to: %s"
                        (widget-value widget))
               (display-buffer
                (find-file-noselect
                 (file-name-concat
                  "~/.local/share/git/straight/repos/org-mode/testing/"
                  (widget-value widget)))
                '((display-buffer-reuse-window
                   display-buffer-reuse-mode-window
                   display-buffer-use-some-window))))
     :offset 1
     '(item :tag "This demo                             " :value "demo.org")
     '(item :tag "A short report   (  70 LaTeX fragments)" :value "test-0070.org")
     '(item :tag "A long report    ( 160 LaTeX fragments)" :value "test-0157.org")
     '(item :tag "A research paper ( 330 LaTeX fragments)" :value "test-0330.org")
     '(item :tag "A book chapter   ( 660 LaTeX fragments)" :value "test-0660.org")
     '(item :tag "A full course    (1200 LaTeX fragments)" :value "test-1200.org"))
    (widget-insert "\n")
    (persistent-choice
     "Visual style" nil
     (("Normal" . nil)
      ("Olivetti" . my/olivetti-mode)
      ("Fill" . visual-fill-column-mode)
      ("Present" . my/org-presentation-mode))
     (lambda (widget &rest _)
       (let ((mode (widget-value widget)))
         (with-visible-org-buffer
          (if (and mode (null (symbol-value mode)))
              (funcall mode 1)
            (when my/olivetti-mode (my/olivetti-mode -1))
            (when my/org-presentation-mode (my/org-presentation-mode -1))
            (when visual-fill-column-mode (visual-fill-column-mode -1))
            (org-fold-show-all '(blocks headings)))))))
    (persistent-choice
     "Color theme"
     (car custom-enabled-themes)
     (("modus-operandi" . modus-operandi)
      ("ef-day" . ef-day)
      ("doom-rouge" . doom-rouge)
      ("doom-gruvbox" . doom-gruvbox)
      ("doom-solarized-dark" . doom-solarized-dark)
      ("ef-winter" . ef-winter)
      ("ef-light" . ef-light))
     (lambda (widget &rest ignore)
       (my/toggle-theme
        (widget-value widget))))
    (widget-insert "\n")

    (use-local-map
     (make-composed-keymap (list (let ((map (make-sparse-keymap)))
                                   (define-key map (kbd "q") 'kill-buffer-and-window)
                                   (define-key map (kbd "<f6>") 'kill-buffer-and-window)
                                   map))
                           widget-keymap))
    (my/hide-cursor-mode 1)
    (widget-setup)))

 (provide 'persistent)
;;; persistent.el ends here


