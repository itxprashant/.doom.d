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



;; Fix html-export for latex fragment (path)
(defun org-html-latex-image--data (image-path-info hash info &optional block-p)
  "Obtaine the image source for IMAGE-PATH-INFO as a string.
This can take the form of a path, data URI, or <svg> element
depending on HASH and INFO.  BLOCK-P signals that the image
should be a block element."
  (let* ((image-options (plist-get info :html-latex-image-options))
         (inline-condition (plist-get image-options :inline))
         (image-dir (plist-get image-options :image-dir))
         (image-format (plist-get (cdr image-path-info) :image-type))
         (source-file (car image-path-info)))
    (cond
     ((or inline-condition
          (member (file-name-extension source-file)
                  (org-ensure-list inline-condition)))
      (let ((coding-system-for-read 'utf-8)
            (file-name-handler-alist nil))
        (with-temp-buffer
          (insert-file-contents-literally source-file)
          (cond
           ((and (eq inline-condition 'svg-embed)
                 (eq image-format 'svg))
            (goto-char (point-min))
            (let ((svg-closing-tag (and (search-forward "<svg" nil t)
                                        (search-forward ">" nil t))))

              (dolist (search '("<!-- This file was generated by dvisvgm [^\n]+ -->"
                                " height=['\"][^\"']+[\"']"
                                " width=['\"][^\"']+[\"']"))
                (goto-char (point-min))
                (when (re-search-forward search svg-closing-tag t)
                  (replace-match "")))
              (goto-char (point-min))
              (when (re-search-forward "viewBox=['\"][^\"']+[\"']" svg-closing-tag t)
                (insert
                 " style=\""
                 (let ((scaling (org-html-latex-image--scaling image-path-info info)))
                   (if block-p
                       (format "height: %.4fem; display: block" (plist-get scaling :height))
                     (format "height: %.4fem; vertical-align: -%.4fem; display: inline-block"
                             (plist-get scaling :height) (plist-get scaling :depth))))
                 "\" class=\"org-latex org-latex-"
                 (if block-p "block" "inline")
                 "\"")))
            (buffer-string))
           ((eq image-format 'svg)
            ;; Modelled after <https://codepen.io/tigt/post/optimizing-svgs-in-data-uris>.
            (concat "data:image/svg+xml,"
                    (url-hexify-string
                     (subst-char-in-string ?\" ?\' (buffer-string))
                     '(?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n
                       ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z ?A ?B
                       ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M ?N ?O ?P
                       ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z ?0 ?1 ?2 ?3
                       ?4 ?5 ?6 ?7 ?8 ?9 ?- ?_ ?. ?~
                       ;;Special additions
                       ?\s ?= ?: ?/))))
           (t
            (base64-encode-region (point-min) (point-max))
            (goto-char (point-min))
            (insert "data:image/" (symbol-name image-format) ";base64,")
            (buffer-string))))))
     ((stringp image-dir)
      (let* ((image-path (file-name-with-extension
                          (file-name-concat image-dir (substring hash 0 11))
                          (file-name-extension source-file))))
        (unless (file-directory-p image-dir)
          (mkdir image-dir t))
        (unless (file-exists-p image-path)
          (copy-file source-file image-path))
        image-path))
     (t source-file))))
