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
  ;; (add-to-list 'eglot-server-programs
  ;;              '(python-mode . ("ruff-lsp")))
  (defun my/eglot-eldoc-settings ()
    (setq eldoc-documentation-strategy
          'eldoc-documentation-compose-eagerly))
  ;; (setq eglot-put-doc-in-help-buffer nil)
  (setq eglot-extend-to-xref t)
  (setq eglot-sync-connect 0) ;; make eglot-ensure non-blocking (async)
  )

(with-eval-after-load 'eglot
  (add-hook 'c-mode-hook #'eglot-ensure)

  ;; for python
  (add-hook 'python-mode-hook #'eglot-ensure)
  (add-to-list 'eglot-server-programs '(python-mode . ("pylsp")))
  (setq-default eglot-workspace-configuration
                '((:pylsp . (:configurationSources ["flake8"] :plugins (:pycodestyle (:enabled nil) :mccabe (:enabled nil) :flake8 (:enabled t))))))

  ;; for typescript
  (add-to-list 'eglot-server-programs
               '((typescript-mode) "typescript-language-server" "--stdio"))
  (add-hook 'tsx-ts-mode #'eglot-ensure)


  ;; disable the minibuffer hinting, distracting
  ;; (add-hook 'eglot-managed-mode-hook (lambda () (eldoc-mode -1)))
  (setq eldoc-echo-area-prefer-doc-buffer t
        eldoc-echo-area-use-multiline-p nil)
  )

(provide 'setup-eglot)