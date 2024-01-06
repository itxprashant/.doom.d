;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'modus-operandi)
(setq doom-theme 'doom-dracula)
(setq fancy-splash-image "~/.doom.d/banner.png")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
;; Enable org-modern globally
(with-eval-after-load 'org (global-org-modern-mode))
;; to org-modern mode locally
;; (add-hook 'org-mode-hook #'org-modern-mode)
;; (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Open nvim instance while writing
;; kclip is a workaround for an unknown jammer triggering on kill ring
(defun my-open-current-file-in-nvim ()
  (interactive)
  (save-buffer)
  (shell-command "~/.local/bin/kclip")
  (shell-command
  (format "urxvt -geometry 100x25 -e nvim +%d %s '+startinsert!'"
       (+ (if (bolp) 1 0) (count-lines 1 (point)))
       (shell-quote-argument buffer-file-name)))
  (evil-force-normal-state))
(global-set-key "\M-e" 'my-open-current-file-in-nvim)


;; Inkscape figures
(load "~/.doom.d/scripts/ink.el")

;; org-download
(require 'org-download)
;; alignment of tables in org latex or image preview
;; (add-hook 'org-mode-hook #'valign-mode)
;; (add-hook 'org-mode-hook 'org-cdlatex-mode)


(add-hook 'olivetti-mode-on-hook (lambda () (olivetti-set-width 110)))
(add-hook 'olivetti-mode-on-hook (lambda () (visual-line-mode 1)))
(add-hook 'org-mode-hook 'olivetti-mode)
;; (add-hook 'org-mode-hook (lambda () (display-line-numbers-mode -1)))

(global-visual-line-mode 1)


(load "~/.doom.d/snippets/aas/basic.el")
(load "~/.doom.d/snippets/aas/latex-aas.el")
(load "~/.doom.d/snippets/aas/chemistry-aas.el")
(add-hook 'LaTeX-mode-hook 'laas-mode)
(add-hook 'org-mode-hook 'laas-mode)


;; fonts and theming
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



(add-hook 'python-mode-hook 'anaconda-mode)

(load "~/.doom.d/scripts/org-latex-preview-health.el")

;; (setq org-latex-preview-default-process 'dvipng)
;; (setq org-latex-preview-default-process 'imagemagick)

;; Fixed in the recent commit
;; (setq org-latex-preview-default-process 'dvisvgm)
;; (setf (plist-get (alist-get 'dvisvgm org-latex-preview-process-alist)
;;                  :image-converter)
;;       '("dvisvgm --page=1- --optimize --clipjoin --relative --no-fonts --bbox=preview -o %B-%%9p.svg %f"))

;; neotree (moved to treemacs)
;; (after! neotree
;;   (setq neo-smart-open t
;;         neo-window-fixed-size nil))
;; (after! doom-themes
;;   (setq doom-neotree-enable-variable-pitch t))
;; (map! :leader
;;       :desc "Toggle neotree file viewer" "e" #'neotree-toggle
;;       :desc "Open directory in neotree"  "d n" #'neotree-dir)
(map! :leader
      :desc "Toggle org latex preview auto mode" "t o" 'org-latex-preview-auto-mode
      :desc "Toggle lsp mode" "t L" 'lsp-mode)

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
        \\usepackage[dvipsnames,svgnames]{xcolor}"
))

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


;; Enables company all the time
(add-hook 'after-init-hook 'global-company-mode)
(after! company
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 2)
  (setq company-dabbrev-downcase 0)
  (setq company-show-quick-access nil)
  (add-hook 'evil-normal-state-entry-hook #'company-abort)) ;; make aborting less annoying.


;; Need to get this working
;; Use context aware backends before :with 
(set-company-backend! 'org-mode
  '(company-files company-capf :with company-yasnippet))

;; Disable ws-butler, responsible for omitting leading whitespaces or empty newlines
(remove-hook 'doom-first-buffer-hook #'ws-butler-global-mode)

(setq ranger-show-hidden t)

;; Tree-sitter sources
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

;; Org-appear
(add-hook 'org-mode-hook 'org-appear-mode)
(setq org-hide-emphasis-markers t)
(setq org-appear-inside-latex nil
      org-appear-autoemphasis t
      org-appear-autolinks t
      org-appear-autosubmarkers t)

(setq org-highlight-latex-and-related '(latex script entities))

(defun open-latex-without-lsp (file)
  "A function to open certain latex files
         without lsp to reduce load time"
  (interactive)
  (remove-hook! 'tex-mode-local-vars-hook 'lsp!)
  (remove-hook! 'latex-mode-local-vars-hook 'lsp!)
  (find-file file))

