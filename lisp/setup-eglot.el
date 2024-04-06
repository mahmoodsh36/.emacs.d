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
  (add-hook 'prog-mode-hook #'eglot-ensure)
  ;; (add-to-list 'eglot-server-programs
  ;;              '(python-mode . ("ruff-lsp")))
  (defun my/eglot-eldoc-settings ()
    (setq eldoc-documentation-strategy
          'eldoc-documentation-compose-eagerly))
  ;; (setq eglot-put-doc-in-help-buffer nil)
  (setq eglot-extend-to-xref t)
  )

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(python-mode . ("pylsp")))
  (setq-default eglot-workspace-configuration
                '((:pylsp . (:configurationSources ["flake8"] :plugins (:pycodestyle (:enabled nil) :mccabe (:enabled nil) :flake8 (:enabled t))))))
  )

(provide 'setup-eglot)