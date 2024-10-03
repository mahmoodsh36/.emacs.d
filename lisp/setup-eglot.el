;; configure eglot (builtin)
;; https://www.gnu.org/software/emacs/manual/html_mono/eglot.html
;; https://andreyor.st/posts/2023-09-09-migrating-from-lsp-mode-to-eglot/

(require 'eglot)

(use-package eglot
  :ensure nil
  :bind (:map eglot-mode-map
              ("C-h ." . eldoc))
  :hook ((eglot-managed-mode . my/eglot-eldoc-settings))
  :config
  ;; some inline hints
  (add-hook 'eglot-managed-mode-hook 'eglot-inlay-hints-mode)
  (defun my/eglot-eldoc-settings ()
    (setq eldoc-documentation-strategy
          'eldoc-documentation-compose-eagerly))
  ;; (setq eglot-put-doc-in-help-buffer nil)
  (setq eglot-extend-to-xref t)
  (setq eglot-sync-connect 0) ;; make eglot-ensure non-blocking (async)
  )

(with-eval-after-load 'eglot
  ;; (add-hook 'c-mode-hook #'eglot-ensure)

  (setq-default
   eglot-workspace-configuration
   '((:pylsp . (:configurationSources ["flake8"] :plugins (:pycodestyle (:enabled nil) :mccabe (:enabled nil) :flake8 (:enabled t))))))

  (add-hook 'prog-mode-hook #'eglot-ensure)

  ;; disable the minibuffer hinting, distracting
  ;; (add-hook 'eglot-managed-mode-hook (lambda () (eldoc-mode -1)))
  (setq eldoc-echo-area-prefer-doc-buffer t
        eldoc-echo-area-use-multiline-p nil)

  ;; https://www.reddit.com/r/emacs/comments/106oq11/eglot_flymake_eldoc/
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              ;; Show flymake diagnostics first.
              (setq eldoc-documentation-functions
                    (cons #'flymake-eldoc-function
                          (remove #'flymake-eldoc-function eldoc-documentation-functions)))
              ;; Show all eldoc feedback.
              (setq eldoc-documentation-strategy #'eldoc-documentation-compose)))
  )

(provide 'setup-eglot)