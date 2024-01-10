;;; scripts/org-latex-preview.el -*- lexical-binding: t; -*-

(defun org-latex-preview-clear-cache-except-preamble (&optional beg end clear-entire-cache)
  "Clear LaTeX preview cache WITHOUT PREAMBLE for fragments between BEG and END."
  (interactive
   (if current-prefix-arg
       (list nil nil (y-or-n-p "This will clear the systemwide LaTeX preview cache, continue? "))
     (let ((context (if (derived-mode-p 'org-mode)
                        (org-element-context)
                      (user-error "This command must be run in an org-mode buffer"))))
       (cond
        ((use-region-p)
         (list (region-beginning) (region-end)))
        ((memq (org-element-type context)
               '(latex-fragment latex-environment))
         (list (org-element-property :begin context)
               (org-element-property :end context)))
        (t (list nil nil))))))
  ;; Clear the precompile cache if clearing the whole buffer or everything.
  (when (or clear-entire-cache (not (or beg end)))
    (or org-latex-preview--preamble-content
        (setq org-latex-preview--preamble-content
              (org-latex-preview--get-preamble))))
  (org-latex-preview-clear-overlays beg end)
  (if clear-entire-cache
      (let ((n 0))
        (dolist (collection org-persist--index)
          (when (equal (cadar (plist-get collection :container))
                       org-latex-preview--cache-name)
            (org-latex-preview--remove-cached
             (plist-get (plist-get collection :associated) :key))
            (cl-incf n)))
        (if (= n 0)
            (message "The Org LaTeX preview cache was already empty.")
          (org-persist-gc)
          (message "Cleared all %d entries fom the Org LaTeX preview cache." n)))
    (let ((imagetype
           (or (plist-get (alist-get org-latex-preview-process-default
                                     org-latex-preview-process-alist)
                          :image-output-type)
               "png"))
          (numbering-table
           (and org-latex-preview-numbered
                (org-latex-preview--environment-numbering-table))))
      (dolist (element (org-latex-preview-collect-fragments beg end))
        (pcase-let* ((begin (or (org-element-property :post-affiliated element)
                                (org-element-property :begin element)))
                     (end (- (org-element-property :end element)
                             (or (org-element-property :post-blank element) 0)
                             (if (eq (char-before (org-element-property :end element))
                                     ?\n)
                                 1 0)))
                     (`(,fg ,bg) (org-latex-preview--colors-around begin end))
                     (value (org-element-property :value element))
                     (number (and numbering-table
                                  (eq (org-element-type element)
                                      'latex-environment)
                                  (gethash element numbering-table))))
          (org-latex-preview--remove-cached
           (org-latex-preview--hash
            org-latex-preview-process-default
            org-latex-preview--preamble-content
            value imagetype fg bg number))))
      (message "Cleared LaTeX preview cache for %s."
               (if (or beg end) "region" "buffer")))))


(defun regenerate-org-latex-cache-and-preamble ()
  "This command regenerates org latex previews"
  (interactive)
  (setq org-startup-with-latex-preview t)
  (save-buffer) 
  (save-excursion (goto-char (point-min))
                  (org-latex-preview-clear-cache)
                  (revert-buffer)))


(defun regenerate-org-latex-cache-only ()
  "This command regenerates org latex previews"
  (interactive)
  (setq org-startup-with-latex-preview t)
  (save-buffer) 
  (save-excursion (goto-char (point-min))
                  (org-latex-preview-clear-cache-except-preamble)
                  (revert-buffer)))
