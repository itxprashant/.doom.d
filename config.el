(require 'org)
(require 'org-latex-preview)

(setq user-full-name "Prashant Tiwari" 
         user-mail-address "prashantt492@gmail.com")

(setq doom-theme 'modus-operandi)
(setq doom-theme 'ef-maris-dark)
(setq fancy-splash-image "~/.doom.d/banner.png")
(setq-default cursor-type 'bar)

(setq display-line-numbers-type t)

(setq org-directory "~/org/")
;; (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

;; Enables company all the time
(add-hook 'after-init-hook 'global-company-mode)
(after! company
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 2)
  (setq company-dabbrev-downcase 0)
  (setq company-show-quick-access nil)
  ;;(add-hook 'evil-normal-state-entry-hook #'company-abort)
  ) ;; make aborting less annoying
(add-hook! 'company-mode-hook 'company-quickhelp-mode)

;; company-dabbrev is rather distracting
(set-company-backend! 'org-mode
  '(:separate company-files company-capf company-yasnippet))

(setq company-files-chop-trailing-slash t)

;; Enable org-modern globally
;; (require 'org) must have been called
;; or use `(with-eval-after-load 'org (global-org-modern-mode))`
(global-org-modern-mode)

;; to enable org-modern mode locally
;;(add-hook 'org-mode-hook #'org-modern-mode)

(load "~/.doom.d/scripts/org-latex-preview-health")

;; (setq org-latex-preview-default-process 'dvipng)
;; (setq org-latex-preview-default-process 'imagemagick)

;; Fixed in the recent commit
;; (setq org-latex-preview-default-process 'dvisvgm)
;; (setf (plist-get (alist-get 'dvisvgm org-latex-preview-process-alist)
;;                  :image-converter)
;;       '("dvisvgm --page=1- --optimize --clipjoin --relative --no-fonts --bbox=preview -o %B-%%9p.svg %f"))

(define-key org-mode-map (kbd "C-c z f") '+org/close-all-folds)
(define-key org-mode-map (kbd "C-c z o") '+org/open-all-folds)

(add-to-list 'warning-suppress-types '(org))

;; (defun my-open-current-file-in-nvim ()
;;   (interactive)
;;   (save-buffer)
;;   (shell-command "~/.local/bin/kclip")
;;   (shell-command
;;   (format "urxvt -geometry 100x25 -e nvim +%d %s '+startinsert!'"
;;        (+ (if (bolp) 1 0) (count-lines 1 (point)))
;;        (shell-quote-argument buffer-file-name)))
;;   (evil-force-normal-state))
;; (global-set-key "\M-e" 'my-open-current-file-in-nvim)

(load "~/.doom.d/scripts/ink")
(map! :leader
      :desc "ink-create-figure" "i i" 'ink-make-figure
      :mode 'org-mode :desc "org-download-clipboard" "i c" 'org-download-clipboard)

(require 'org-download)
(defun my-org-download-method (link)
  (let ((filename
         (file-name-nondirectory
          (car (url-path-and-query
                (url-generic-parse-url link)))))
        (dirname (concat "img/" (file-name-sans-extension (buffer-name)))))
    (make-directory dirname :parents)
    (expand-file-name (concat (format-time-string "%Y%m%d%H%M%S-") filename) dirname)))
(setq org-download-method 'my-org-download-method)
;; (setq-default org-download-image-dir "./img/")

;; alignment of tables in org latex or image preview
;; (add-hook 'org-mode-hook #'valign-mode)
;; (add-hook 'org-mode-hook 'org-cdlatex-mode)

(add-hook 'olivetti-mode-on-hook (lambda () (olivetti-set-width 66)))
(add-hook 'olivetti-mode-on-hook (lambda () (visual-line-mode 1)))
(add-hook 'org-mode-hook 'olivetti-mode)

;; (setq org-image-align 'center)
;; (add-hook 'org-mode-hook (lambda () (display-line-numbers-mode -1)))

(global-visual-line-mode 1)

(load "~/.doom.d/snippets/aas/basic")
(load "~/.doom.d/snippets/aas/latex-aas")
(load "~/.doom.d/snippets/aas/chemistry-aas")
(add-hook 'LaTeX-mode-hook 'laas-mode)
(add-hook 'org-mode-hook 'laas-mode)

(setq doom-font (font-spec :family "Hack Nerd Font" :size 15)
      doom-variable-pitch-font (font-spec :family "Hack Nerd Font" :size 15)
      doom-big-font (font-spec :family "Hack Nerd Font" :size 24))
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

(set-default 'preview-default-document-pt 12)
(set-default 'preview-scale-function 1.5)

 (defun my-buffer-face-mode-variable ()
   "Set font to a variable width (proportional) fonts in current buffer"
   (interactive)
   (setq buffer-face-mode-face '(:family "DejaVu Serif" :height 130))
   (set-face-attribute 'org-table nil  :inherit 'fixed-pitch) ;; For tables alignment
   (buffer-face-mode))

(add-hook! 'org-mode-hook 'my-buffer-face-mode-variable)

(add-hook 'python-mode-hook 'anaconda-mode)

;; neotree (moved to treemacs)
;; (after! neotree
;;   (setq neo-smart-open t
;;         neo-window-fixed-size nil))
;; (after! doom-themes
;;   (setq doom-neotree-enable-variable-pitch t))
;; (map! :leader
;;       :desc "Toggle neotree file viewer" "e" #'neotree-toggle
;;       :desc "Open directory in neotree"  "d n" #'neotree-dir)

(add-hook! 'treemacs-mode-hook 'treemacs-follow-mode)
(setq treemacs-width 40)

;;(add-hook! 'org-mode-hook 'evil-tex-mode)

(setq org-html-htmlize-output-type 'css)

(vertico-reverse-mode 1)

(defun save-and-revert-buffer ()
  "Saves and reverts the buffer"
  (interactive)
  (save-buffer)
  (revert-buffer))

(map! :leader
     :desc "Save and revert buffer" "b j" 'save-and-revert-buffer)
(map! :leader
      :desc "Browse other project" ">" 'doom/browse-in-other-project)

(map! :mode 'ranger-mode :desc "New file" "; n" 'dired-create-empty-file)

(defun org-latex-preview/dvipng ()
    "Sets dvipng as default latex process"
        (interactive)
        (setq org-latex-preview-process-default 'dvipng))

(defun org-latex-preview/dvisvgm ()
    "Sets dvipng as default latex process"
        (interactive)
        (setq org-latex-preview-process-default 'dvisvgm))

(defun org-latex-preview/imagemagick ()
    "Sets dvipng as default latex process"
        (interactive)
        (setq org-latex-preview-process-default 'imagemagick))

;; latex preview options
(setq org-startup-with-inline-images t
      org-startup-with-latex-preview t
      +org-startup-with-animated-gifs t)
(add-hook 'org-mode-hook 'tooltip-mode)

(use-package! org-latex-preview
  :after org
  :hook ((org-mode . org-latex-preview-auto-mode))
  :config
  (pushnew! org-latex-preview--ignored-faces 'org-list-dt 'fixed-pitch)
  (setq org-latex-preview-numbered     t
        org-startup-with-latex-preview t
        org-latex-preview-width 1.0
        org-latex-preview-processing-indicator 'face
        ;;live previewing
        org-latex-preview-live-preview-fragments t
        org-latex-preview-auto-generate 'live
        org-latex-preview-debounce 1.0
        org-latex-preview-throttle 0.8
        org-latex-preview-live-preview-fragments t
        ;;previewing preamble
        org-latex-preview-preamble
        "\\documentclass{article}\n[DEFAULT-PACKAGES]\n[PACKAGES]
        \\usepackage[dvipsnames,svgnames]{xcolor}
        \\usepackage{amsmath,amssymb}"
))

(load "~/.doom.d/scripts/ink.el")
(load "~/.doom.d/scripts/org-latex-preview")

(map! :leader
      :desc "Toggle org latex preview auto mode" "t o" 'org-latex-preview-auto-mode
      :desc "Toggle lsp mode" "t L" 'lsp-mode
      :desc "Regenerate latex cache and preamble" "r L" 'regenerate-org-latex-cache-and-preamble
      :desc "Regenerate latex cache" "r l" 'regenerate-org-latex-cache-only)

(use-package! yasnippet
  ;; :ensure t
  :hook ((LaTeX-mode . yas-minor-mode)
         (post-self-insert . my/yas-try-expanding-auto-snippets))
  :config
  (use-package warnings
    :config
    (cl-pushnew '(yasnippet backquote-change)
                warning-suppress-types
                :test 'equal))

  (setq yas-triggers-in-field t)

  ;; Function that tries to autoexpand YaSnippets
  ;; The double quoting is NOT a typo!
  (defun my/yas-try-expanding-auto-snippets ()
    (when (and (boundp 'yas-minor-mode) yas-minor-mode)
      (let ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
        (yas-expand)))))

(remove-hook 'doom-first-buffer-hook #'ws-butler-global-mode)
(setq require-final-newline nil)

(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (latex "https://github.com/latex-lsp/tree-sitter-latex")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(when init-file-debug
  (require 'benchmark-init)
  (add-hook 'doom-first-input-hook #'benchmark-init/deactivate))

(setq treemacs-expand-added-projects nil
      treemacs-expand-after-init nil)

(add-hook 'org-mode-hook 'org-appear-mode)
(setq org-hide-emphasis-markers t)
(setq org-appear-inside-latex nil
      org-appear-autoemphasis t
      org-appear-autolinks t
      org-appear-autosubmarkers t
      org-appear-autoentities t
      org-appear-autokeywords t)

(setq org-highlight-latex-and-related '(latex script entities))

(setq org-export-preserve-breaks nil)

(setq gptel-default-mode #'org-mode)
(load "~/Documents/credentials/gptel-gemini-key")
(map! :leader
      :desc "GPTEL" "g p p" 'gptel
      :desc "GPTEL send" "g p s" 'gptel-send
      :desc "GPTEL menu" "g p m" 'gptel-menu)

;; (add-hook 'yas-before-expand-snippet-hook (lambda () (smartparens-mode -1)))
;; (add-hook 'yas-after-exit-snippet-hook (lambda () (smartparens-mode 1)))

(defun initialize-org-mode-with-daemon ()
  "Initializes org mode with daemon to reduce load time"
  (with-temp-buffer (org-mode)))

(add-hook 'emacs-startup-hook #'initialize-org-mode-with-daemon)

(setq rmh-elfeed-org-files (list "~/.doom.d/elfeed.org"))

(setq org-latex-pdf-process '("latexmk -f -pdf -%latex -shell-escape -interaction=nonstopmode -output-directory=%o %f"))

(defun itxp/kill-save-line ()
  "Kill-saves the current line"
  (interactive)
  (save-excursion
    (end-of-line) (push-mark-command (point) t) (beginning-of-line)
    (kill-ring-save (point) (mark))))

(defun itxp/yank-below-line ()
  "Yanks below line"
  (interactive)
  (save-excursion (end-of-line) (newline) (yank)))

(defun itxp/duplicate-line ()
  "Duplicates line"
  (interactive)
  (save-excursion (itxp/kill-save-line) (itxp/yank-below-line)))

(global-set-key (kbd "C-M-y") 'itxp/duplicate-line)

(map! :leader
      :desc "kill-save line" "y w" 'itxp/kill-save-line
      :desc "yank below line" "y y" 'itxp/yank-below-line
      :desc "duplicate line" "y d" 'itxp/duplicate-line)

(map! :leader
         :desc "Kill workspace" "TAB x" '+workspace/kill-session
      :desc "Kill buffer" "b k" 'kill-this-buffer
      :desc "Maximize buffer" "w m" 'doom/window-maximize-buffer
      :desc "Find file in current project" "." 'find-file)

(map! :after org
      :map org-mode-map
      :leader
      :desc "org-latex-preview" "x l" 'org-latex-preview
      :desc "org-image-preview" "x v" 'org-toggle-inline-images)

(setq git-gutter:update-interval 0.1)

(setq dictionary-server "dict.org")

(setq initial-scratch-message nil)

(load (concat doom-user-dir "config/yas-keymaps"))

(map! :map emacs-everywhere-mode-map
      "C-c C-c" 'emacs-everywhere--finish-or-ctrl-c-ctrl-c)

(global-set-key (kbd "C-x o") 'ace-window)
;; Split window and focus
(global-set-key (kbd "C-x 2") (lambda () (interactive) (split-window-vertically) (other-window 1) ))
(global-set-key (kbd "C-x 3") (lambda () (interactive) (split-window-horizontally) (other-window 1) ))
(global-set-key (kbd "C-x w m") 'doom/window-maximize-buffer)
(global-set-key (kbd "C-x w u") 'winner-undo)
(global-set-key (kbd "C-x w U") 'winner-redo)

(global-set-key (kbd "C-:") 'avy-goto-char-2)
(global-set-key (kbd "M-g f") 'avy-goto-line)

(org-mode)
(emacs-lisp-mode) ;; Revert
