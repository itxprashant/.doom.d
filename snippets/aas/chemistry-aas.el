(use-package laas
  :hook (LaTeX-mode . laas-mode)
  :config ; do whatever here
  (aas-set-snippets 'laas-mode
                    ;; set condition!
                    :cond #'texmathp ; expand only while in math
                    "ce" (lambda () (interactive) (yas-expand-snippet "\\ce{$1} $0"))
                    "cfig" (lambda () (interactive) (yas-expand-snippet "\\chemfig{$1} $0"))
                    "cubr" (lambda () (interactive (yas-expand-snippet "$\\underbrace{\\ce{$1}}_{$2}$ $0")))

                    ;; add accent snippets
                    :cond #'laas-object-on-left-condition
                    ;; "qq" (lambda () (interactive) (laas-wrap-previous-object "sqrt"))
                    ))
