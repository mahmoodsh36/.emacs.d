;; fails to compile on android, also default recipe fails when used with elpaca
(when (not (is-android-system))
  (use-package auctex
    :elpaca '(auctex
              :pre-build (("./autogen.sh")
                          ("./configure"
                           "--without-texmf-dir"
                           "--with-packagelispdir=./"
                           "--with-packagedatadir=./")
                          ("make"))
              :build (:not elpaca--compile-info) ;; Make will take care of this step
              :files ("*.el" "doc/*.info*" "etc" "images" "latex" "style")
              :version (lambda (_) (require 'tex-site) AUCTeX-version))))

;; set of org-contrib packages
(use-package org-contrib)

;; makes binding keys less painful
(use-package general)

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package general)

;; epub reader
(use-package nov
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  ;; (defun my-nov-font-setup ()
  ;;   (face-remap-add-relative 'variable-pitch :family "Liberation Serif"
  ;;                            :height 1.0))
  ;; (add-hook 'nov-mode-hook 'my-nov-font-setup)
  ;; (setq nov-text-width 80)
  )

;; (formerly om.el) A functional library for org-mode
(use-package org-ml)

;; typescript setup
(use-package typescript-mode)
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))
;; formats the buffer before saving
;; (add-hook 'before-save-hook 'tide-format-before-save)
;; if you use typescript-mode
(add-hook 'typescript-mode-hook #'setup-tide-mode)
;; if you use treesitter based typescript-ts-mode (emacs 29+)
(add-hook 'typescript-ts-mode-hook #'setup-tide-mode)

;; side tree
(use-package treemacs
  :config
  (treemacs-resize-icons 15)
  (setq treemacs-width 30)
  (treemacs-follow-mode -1))
(defun treemacs-remove-project-at-point-force ()
  (interactive)
  "force removal of project at point, even if its the last one"
  (treemacs-do-remove-project-from-workspace (treemacs-project-at-point) t))

;; projectile, for project browsing/management
;; (use-package projectile
;;   :elpaca (:host github :repo "bbatsov/projectile")
;;   :config
;;   ;; (setq projectile-completion-system 'ivy)
;;   (projectile-mode +1))

;; auto completion
;; im sticking with company for now as corfu keeps crashing with org mode, plus slime doesnt work with corfu (for now)
(setq enable-company nil)
(setq completion-ignore-case t) ;; case-insensitivity
(if enable-company
    (progn
      (use-package company
        :elpaca (:host github :repo "company-mode/company-mode")
        :config
        (add-hook 'after-init-hook 'global-company-mode)
        (global-set-key (kbd "M-/") 'company-complete-common-or-cycle)
        ;; disable non-smart completion
        (delete 'company-dabbrev company-backends)
        (setq company-idle-delay 0
              company-require-match nil
              company-tooltip-limit 20
              company-tooltip-align-annotations t
              company-dabbrev-downcase nil
              company-show-quick-access t
              company-format-margin-function #'company-text-icons-margin)
        (eval-after-load 'company
          '(progn
             ;; (define-key company-active-map (kbd "TAB") 'company-complete-selection)
             ;; (define-key company-active-map [tab] 'company-complete-selection)
             (unbind-key "RET" company-active-map)
             (unbind-key "<return>" company-active-map))))

      ;; popup documentation for quick help for company
      (use-package company-quickhelp
        :config
        (company-quickhelp-mode))

      ;; anaconda for python
      ;; (use-package company-anaconda
      ;;   :config
      ;;   (eval-after-load "company"
      ;;     '(add-to-list 'company-backends 'company-anaconda))
      ;;   (add-hook 'python-mode-hook 'anaconda-mode))

      ;; company for web mode
      (use-package company-web)

      ;; company for shell scripting
      ;; (use-package company-shell
      ;;   :config
      ;;   (add-to-list 'company-backends '(company-shell company-shell-env)))

      ;; latex company backend
      (use-package company-auctex
        :config
        (company-auctex-init))

      ;; (use-package company-prescient
      ;; :config
      ;; (company-prescient-mode))
      )
  (progn ;; corfu autocompletion
    (use-package corfu
      :init
      (global-corfu-mode)
      :custom
      (corfu-cycle t)
      (corfu-auto t) ;; i feel like this gets in the way so i wanna disable it
      (corfu-quit-no-match t)
      (corfu-auto-delay 0.1) ;; never set it to 0, makes emacs very laggy and hogs cpu
      ;; (corfu-separator ?_) ;; set to orderless separator, if not using space
      ;; (corfu-separator " ") ;; set to orderless separator, if not using space
      (corfu-count 10)
      (corfu-indexed-mode t)
      (corfu-echo-mode t) ;; display brief documentation in echo area
      (corfu-popupinfo-mode t) ;; display documentation in popup
      (corfu-quit-at-boundary nil)
      (corfu-on-exact-match nil) ;; dont auto insert when there is an exact match
      (corfu-popupinfo-delay (cons 0 0)) ;; dont auto insert when there is an exact match
      :config
      ;; (unbind-key "RET" corfu-map)
      ;; (unbind-key "TAB" corfu-map)
      ;; (define-key corfu-map [tab] nil)
      ;; (define-key corfu-map "\t" nil)
      ;; (bind-key "C-TAB" #'corfu-complete corfu-map)
      ;; (define-key corfu-map "\M-q" #'corfu-quick-complete)
      ;; (define-key corfu-map "\M-q" #'corfu-quick-insert)
      )

    (use-package kind-icon
      :ensure t
      :after corfu
      :custom
      ;; (kind-icon-blend-background t) ;; to compute blended backgrounds correctly
      ;; (kind-icon-default-face 'corfu-default) ;; only needed with blend-background
      (kind-icon-use-icons nil) ;; only needed with blend-background
      :config
      (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
      )

    (use-package cape
      :init
      (add-to-list 'completion-at-point-functions #'cape-dabbrev)
      (add-to-list 'completion-at-point-functions #'cape-file)
      (add-to-list 'completion-at-point-functions #'cape-tex)
      ;; (add-to-list 'completion-at-point-functions #'cape-line)
      (add-to-list 'completion-at-point-functions #'cape-keyword)
      (add-to-list 'completion-at-point-functions #'cape-elisp-block)
      )

    ;; for some reason ispell completion is enabled in org mode
    (defun remove-ispell-cap ()
      (interactive)
      (setq-local completion-at-point-functions (delete 'ispell-completion-at-point completion-at-point-functions)))
    (add-hook 'org-mode-hook #'remove-ispell-cap)

    ;; (use-package orderless
    ;;   :init
    ;;   ;; configure a custom style dispatcher (see the consult wiki)
    ;;   ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
    ;;   ;;       orderless-component-separator #'orderless-escapable-split-on-space)
    ;;   (setq completion-styles '(orderless partial-completion basic);; '(orderless basic)
    ;;         completion-category-defaults nil
    ;;         completion-category-overrides nil))

    ;; corfu completion in the minibuffer
    (with-eval-after-load 'corfu
      (defun corfu-enable-in-minibuffer ()
        "enable corfu in the minibuffer if `completion-at-point' is bound."
        (when (where-is-internal #'completion-at-point (list (current-local-map)))
          ;; (setq-local corfu-auto nil) enable/disable auto completion
          (corfu-mode 1)))
      (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)
      )

    (use-package pcmpl-args)

    ;; (use-package corfu-doc)
    )
  )

;; vertical completion interface
;; (use-package counsel
;;   :config
;;   (ivy-mode)
;;   (setq ivy-height 20)
;;   (global-set-key (kbd "M-x") 'counsel-M-x)
;;   (global-set-key (kbd "C-x C-f") 'counsel-find-file))

;; colorful delimiters
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; icons for dired
(use-package all-the-icons
  ;; :after (vertico)
  :custom
  (all-the-icons-dired-monochrome nil))

(use-package all-the-icons-dired
  ;; :after (vertico)
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;; web-mode doesnt work with tree-sitter
;; (use-package web-mode
;;   :config
;;   (setq web-mode-enable-auto-quoting nil)
;;   (setq web-mode-enable-auto-closing nil)
;;   (setq web-mode-enable-auto-expanding nil)
;;   (setq web-mode-enable-auto-pairing nil)
;;   (setq web-mode-enable-auto-indentation nil)
;;   (setq web-mode-enable-auto-opening nil)
;;   (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
;;   (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
;;   (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
;;   (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
;;   (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
;;   (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
;;   (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
;;   (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;;   (add-to-list 'auto-mode-alist '("\\.js?\\'" . web-mode))
;;   (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))
;;   (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
;;   (setq web-mode-markup-indent-offset 2)
;;   (setq web-mode-markup-indent-offset 2)
;;   (setq web-mode-css-indent-offset 2)
;;   (setq web-mode-code-indent-offset 2))

(use-package lua-mode
  :config
  (setq lua-indent-level 2))

;; package to help making http requests
(use-package request)

;; highlights color names with the corresponding color
(use-package rainbow-mode
  :config
  (add-hook 'text-mode-hook 'rainbow-mode))

;; log elisp commands
;; (use-package command-log-mode
;;   :config
;;   (global-command-log-mode))

;; executing sage in org babel
(use-package ob-sagemath
  :config
  ;; ob-sagemath supports only evaluating with a session.
  (setq org-babel-default-header-args:sage '((:session . t)
                                             (:results . "drawer")))
  (setq sage-shell:input-history-cache-file (from-brain "/sage_history"))
  (add-hook 'sage-shell-after-prompt-hook #'sage-shell-view-mode))

;; better built-in help/documentation
(use-package helpful
  :config
  (define-key help-map (kbd "f") #'helpful-callable)
  (define-key help-map (kbd "v") #'helpful-variable)
  (define-key help-map (kbd "a") #'helpful-symbol)
  (define-key help-map (kbd "k") #'helpful-key)
  ;; stop helpful buffers from jumping between windows
  (setq helpful-switch-buffer-function #'pop-to-buffer-same-window))

;; yasnippet
;; (use-package yasnippet-snippets)
(use-package yasnippet
  :config
  ;; disable builtin snippets
  (setq yas-snippet-dirs `(,(concat user-emacs-directory "snippets")))
  ;; enable nested snippet expansion
  (setq yas-triggers-in-field t)
  (yas-global-mode 1)
  ;; prevent warnings about snippets using elisp
  (require 'warnings)
  (add-to-list 'warning-suppress-types '(yasnippet backquote-change))
  ;; enable latex snippets in org mode
  (defun my-org-latex-yas ()
    "activate org and latex yas expansion in org-mode buffers."
    ;; (yas-minor-mode)
    (yas-activate-extra-mode 'latex-mode))
  (add-hook 'org-mode-hook #'my-org-latex-yas)
  )
;; disable the default tab binding
;; (define-key yas-minor-mode-map [(tab)] nil)
;; (define-key yas-minor-mode-map (kbd "TAB") nil)
;; bind C-h to snippet expand
;; (define-key yas-minor-mode-map (kbd "C-h") #'yas-expand))
;; Bind `SPC' to `yas-expand' when snippet expansion available (it will still call `self-insert-command' otherwise).
;; (define-key yas-minor-mode-map (kbd "SPC") yas-maybe-expand))

;; function that tries to autoexpand yasnippets
;; the double quoting is not a typo!
;; (defun my/yas-try-expanding-auto-snippets ()
;;   (when (bound-and-true-p yas-minor-mode)
;;     (message "test")
;;     ;; (let ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
;;       (yas-expand)))
;; ;; try after every insertion
;; (with-eval-after-load 'yasnippet
;;   (add-hook 'post-self-insert-hook #'my/yas-try-expanding-auto-snippets))

;; highlight errors in code
(use-package flycheck
  :config
  (global-flycheck-mode))

;; edit multiple instances of a word simulataneously
;; (use-package iedit)

;; highlight surrounding parentheses
(use-package highlight-parentheses
  :config
  (add-hook 'prog-mode-hook #'highlight-parentheses-mode))

;; generating linear ranges quickly
(use-package tiny
  :config
  (global-set-key (kbd "C-c t") 'tiny-expand))

;; modern API for working with files/dirs
(use-package f)

;; language server protocol support
;; (use-package lsp-mode
;;   :config
;;   (add-hook 'prog-mode-hook 'lsp-deferred)
;;   ;; gets rid of some annoying prompts to add project root when visiting definition of symbol
;;   ;; (setq lsp-auto-guess-root t)
;;   ;; another annoying warning
;;   (setq lsp-warn-no-matched-clients nil)
;;   )
;; (if (not enable-company)
;;     (use-package lsp-mode
;;       :custom
;;       (lsp-completion-provider :none) ;; we use corfu!
;;       :init
;;       ;; optionally configure the cape-capf-buster.
;;       (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point)))
;;       )
;;   )

;; show simple info on the right
;; (use-package lsp-ui
;;   :config
;;   (add-hook 'lsp-mode-hook 'lsp-ui-mode)
;;   (setq lsp-ui-doc-delay 0
;;         lsp-ui-sideline-delay 0
;;         lsp-ui-sideline-show-diagnostics t
;;         lsp-ui-sideline-show-hover t
;;         lsp-ui-sideline-show-symbol t
;;         lsp-ui-sideline-show-code-actions t
;;         lsp-ui-doc-enable t
;;         lsp-ui-sideline-enable t
;;         lsp-lens-enable t
;;         lsp-completion-show-detail t)
;;   (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
;;   (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

;; lsp support for treemacs
;; (use-package lsp-treemacs
;;   :config
;;   (lsp-treemacs-sync-mode 1))

;; ensure the PATH variable is set according to the users shell, solves some issues on macos
;; (use-package exec-path-from-shell
;;   :config
;;   (exec-path-from-shell-initialize))

;; display available keybindings
(use-package which-key
  :config
  (which-key-mode 1))

;; small flash when evaluating a sexp
;; (use-package eval-sexp-fu)

;; flutter setup
(use-package highlight-indent-guides
  :config
  (setq highlight-indent-guides-method 'character)
  ;; highlight-indent-guides-responsive 'stack)
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))
(use-package flutter)
;; (use-package lsp-dart)

;; pdf viewer
(when (not (is-android-system))
  (use-package pdf-tools
    :config
    (pdf-tools-install t)
    (add-hook 'pdf-view-mode-hook 'pdf-view-themed-minor-mode)))

;; history for ivy completion
;; (use-package ivy-prescient
;;   :config
;;   (ivy-prescient-mode)
;;   (prescient-persist-mode 1)
;;   (setq prescient-history-length 10000)

;; ;; more featureful ivy menus, it may cause some error when switching buffers
;; (use-package ivy-rich
;;   :config
;;   (ivy-rich-mode 1)
;;   (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

;; icons for ivy
;; (use-package all-the-icons-ivy-rich
;;   :config (all-the-icons-ivy-rich-mode 1))

;; ;; auto indentation
;; (use-package aggressive-indent
;;   :config
;;   (aggressive-indent-global-mode))

;; modify/add common delimiters around text
;; (use-package smartparens
;;   :config
;;   ;; dont insert pair when cursor is before text
;;   (sp-pair "(" nil :unless '(sp-point-before-word-p))
;;   (sp-pair "[" nil :unless '(sp-point-before-word-p))
;;   (sp-pair "{" nil :unless '(sp-point-before-word-p))
;;   (sp-pair "\"" nil :unless '(sp-point-before-word-p))
;;   (sp-local-pair '(latex-mode org-mode) "$" "$" :unless '(sp-point-before-word-p))
;;   (smartparens-global-mode))

;; provides syntax highlighting when exporting from org mode to html
(use-package htmlize)

;; static website generation for org mode
(use-package ox-hugo
  :config
  (setq org-hugo-base-dir (file-truename "~/work/blog/"))
  ;; (setq org-hugo-section "blog")
  ;; (setq org-more-dir (expand-file-name "~/work/blog/static/more/"))
  (setq *org-static-dir* (format "%s/static" org-hugo-base-dir))
  ;; (ignore-errors (make-directory org-more-dir))
  (add-to-list 'org-hugo-external-file-extensions-allowed-for-copying "webp")
  (add-to-list 'org-hugo-external-file-extensions-allowed-for-copying "html")
  ;; (plist-put org-html-latex-image-options :image-dir (file-truename (concat user-emacs-directory "html_ltximg")))
  ;;(plist-put org-html-latex-image-options :image-dir (file-truename (concat *org-static-dir* "/ltximg/"))))
  )

(use-package dap-mode)

(use-package mixed-pitch
  :hook
  (text-mode . mixed-pitch-mode))
;; (add-hook 'text-mode-hook #'variable-pitch-mode)

;; ;; give org mode a better look
;; (use-package org-modern
;;   :config
;;   (setq
;;    org-modern-hide-stars nil
;;    org-auto-align-tags nil
;;    org-tags-column 0
;;    org-catch-invisible-edits 'show-and-error
;;    org-special-ctrl-a/e t
;;    org-insert-heading-respect-content t
;;    ;; Agenda styling
;;    ;; org-agenda-tags-column 0
;;    ;; org-agenda-block-separator ?─
;;    ;; org-agenda-time-grid
;;    ;; '((daily today require-timed)
;;    ;;   (800 1000 1200 1400 1600 1800 2000)
;;    ;;   " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
;;    ;; org-agenda-current-time-string
;;    ;; "⭠ now ─────────────────────────────────────────────────"
;;    )
;;   (global-org-modern-mode))

;; show hidden elements when cursor is over them like links/markers etc
(use-package org-appear
  :after org
  :config
  (setq org-appear-autoemphasis t
        org-appear-autoentities t
        org-appear-autokeywords t
        org-appear-autolinks t
        org-appear-autosubmarkers t
        ;; org-hide-emphasis-markers t
        )
  (add-hook 'org-mode-hook 'org-appear-mode))

;; (use-package dape)

;; (use-package elfeed-tube
;;   :quelpa (:host github :repo "karthink/elfeed-tube")
;;   :after elfeed
;;   :demand t
;;   :config
;;   ;; (setq elfeed-tube-auto-save-p nil) ;; t is auto-save (not default)
;;   ;; (setq elfeed-tube-auto-fetch-p t) ;;  t is auto-fetch (default)
;;   (elfeed-tube-setup)
;;   :bind (:map elfeed-show-mode-map
;;               ("F" . elfeed-tube-fetch)
;;               ([remap save-buffer] . elfeed-tube-save)
;;               :map elfeed-search-mode-map
;;               ("F" . elfeed-tube-fetch)
;;               ([remap save-buffer] . elfeed-tube-save)))

;; (use-package dumb-jump)
(use-package format-all)
;; (use-package plantuml-mode)

;; for latex references
;; (use-package org-ref
;;   :config
;;   (setq bibtex-completion-bibliography (list org-cite-global-bibliography))
;;   (define-key org-mode-map (kbd "C-c ]") 'org-ref-insert-link-hydra/body))

;; (use-package code-compass)

                                        ;(use-package vterm
                                        ;:custom
                                        ;(vterm-always-compile-module t))

;; check which keys i press most
(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  (setq keyfreq-file (from-brain "emacs_keyfreq")))

;; has issues with transient versions
;; (use-package magit)

;; need the "global" package for gtags binary
(use-package ggtags
  :config
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
                (ggtags-mode 1)))))
;; (use-package counsel-gtags)

(use-package git-auto-commit-mode)

;; (use-package auto-yasnippet)

(use-package avy)

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ;;("C-;" . embark-dwim)        ;; good alternative: M-.
   ;; ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
   )
  :init
  ;; optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; hide the mode line of the embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  ;; not sure why defining this map doesnt work as intended tho
  (defvar-keymap embark-file-map
    :doc "custom file actions"
    :parent embark-general-map
    "k" #'kill-this-buffer)
  )

(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package org-ql)

;; evaulation overlay for elisp
(use-package eros
  :config
  (eros-mode 1))

;; live web dev
(use-package skewer-mode)

;; (use-package ialign)
;; (use-package 'org-protocol-capture-html)

;; emacs application framework
;; (use-package eaf
;;   :quelpa (eaf
;;              :type git
;;              :host github
;;              :repo "emacs-eaf/emacs-application-framework"
;;              :files ("*.el" "*.py" "core" "app" "*.json")
;;              :includes (eaf-pdf-viewer eaf-browser) ; quelpa won't try to search for these packages when we make further use-package invocations for them
;;              :pre-build (("python" "install-eaf.py" "--install" "pdf-viewer" "browser" "--ignore-sys-deps"))
;;              )
;;   :init (evil-set-initial-state 'eaf-mode 'emacs) ; Evil mode doesn't work well with eaf keybindings.
;;   :config
;;   (load-library "eaf-pdf-viewer")
;;   (load-library "eaf-browser"))

;; (use-package tree-sitter
;;   :config
;;   (global-tree-sitter-mode 1))
;; (global-tree-sitter-mode 1)
;; (use-package tree-sitter-langs
;;   :config
;;   (tree-sitter-langs-install-grammars t))

                                        ;(use-package json-to-org-table :quelpa (:host github :repo "noonker/json-to-org-table"))

;; (use-package lsp-java)

(use-package emmet-mode
  :config
  (add-hook 'mhtml-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook 'emmet-mode))

;; transclusions (including text from other documents) for org mode
(use-package org-transclusion
  :config
  (add-hook 'org-mode-hook #'org-transclusion-mode))
(with-eval-after-load 'org-transclusion
  (defun org-transclusion-add-blk (link plist)
    "Return a list for Org-ID LINK object and PLIST.
Return nil if not found."
    (when (string= "blk" (org-element-property :type link))
      (let* ((path (org-element-property :path link))
             (marker (ignore-errors (org-blk-marker path t)))
             (payload '(:tc-type "org-link")))
        (message "marker %s" marker)
        (if marker
            (append payload (org-transclusion-content-org-marker marker plist))
          (message
           (format "No transclusion done for this blk. Ensure it works at point %d, line %d"
                   (point) (org-current-line)))
          nil))))
  (add-to-list 'org-transclusion-add-functions 'org-transclusion-add-blk)
  )

(use-package eat)

;; (use-package org-bullets
;;   :config
;;   (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; better than org-bullets
;; (use-package org-superstar
;;   :config
;;   (add-hook 'org-mode-hook 'org-superstar-mode))

;; relative line numbers
(use-package linum-relative
  :config
  (add-hook 'prog-mode-hook 'linum-relative-mode)
  ;; show the real line number at current line
  (setq linum-relative-current-symbol ""))

;; krita-supported manual drawing with org mode
;; (quelpa '(org-krita :fetcher github :repo "lepisma/org-krita" :files ("*.el" "resources")))
;; (add-hook 'org-mode-hook 'org-krita-mode)

;; like org-krita, crashes, unusable...
;; (quelpa '(org-xournalpp :fetcher gitlab :repo "vherrmann/org-xournalpp" :files ("*.el" "resources")))
;; (add-hook 'org-mode-hook 'org-xournalpp-mode)
;; (use-package org-xournalpp
;;   :ensure t
;;   :quelpa (org-xournalpp :fetcher gitlab :repo "vherrmann/org-xournalpp" :files ("*.el" "resources"))
;;   :config
;;   (add-hook 'org-mode-hook 'org-xournalpp-mode))

;; perfectly aligned org mode tables
(use-package valign
  :hook
  (org-mode . valign-mode)
  :config
  (setq valign-fancy-bar t))

;; (use-package hydra
;;   :config
;;   (defhydra hydra-agenda (global-map "C-c a")
;;     "hydra-agenda"
;;     ("a" org-agenda-list "list")
;;     ("n" today-entry "entry for today")
;;     ("o" open-todays-file "open today's file")
;;     ("d" org-deadline "deadline")
;;     ("s" org-schedule "schedule"))
;;   (defhydra hydra-roam (global-map "C-c r")
;;     "hydra-roam"
;;     ("f" org-roam-node-find "find roam node")
;;     ("n" (lambda () (interactive) (org-roam-capture nil "n")) "create roam node")
;;     ))

(use-package vimrc-mode)

(use-package sly
  :elpaca (:host github :repo "joaotavora/sly")
  :config
  (setq inferior-lisp-program "")
  (setq sly-lisp-implementations
        '((sbcl ("sbcl" "--dynamic-space-size" "12GB"))
          (clisp ("clisp"))
          (ecl ("ecl"))
          (cmucl ("cmucl"))
          (ccl ("ccl"))
          (maxima ("rmaxima" "-r" "to_lisp();"))))
  ;; make org babel use sly instead of slime
  (setq org-babel-lisp-eval-fn #'sly-eval)
  (setq sly-mrepl-history-file-name (from-brain "sly_history"))
  ;; i think this increases history file size
  (setq comint-input-ring-size 1000000))
;; (use-package slime
;;   :config
;;   (setq inferior-lisp-program "")
;;   (slime-setup '(slime-fancy
;;                  slime-sbcl-exts
;;                  slime-scheme
;;                  slime-sprof
;;                  slime-asdf
;;                  slime-indentation
;;                  slime-cl-indent
;;                  slime-trace-dialog
;;                  slime-repl
;;                  slime-scratch))
;;   (setq slime-lisp-implementations
;;         '((sbcl ("sbcl" "--dynamic-space-size" "10GB"))
;;           (clisp ("clisp"))
;;           (ecl ("ecl"))
;;           (cmucl ("cmucl"))
;;           (ccl ("ccl"))
;;           (maxima ("rmaxima" "-r" "to_lisp();"))))
;;   ;; disable evil-mode
;;   (setq slime-repl-history-file (from-brain "slime_history"))
;;   (setq slime-repl-history-size 1000000))

;; flash cursor when jumping around
;; (use-package beacon
;;   :config
;;   (beacon-mode 1))

;; better alternative to counsel-ag, best i found for grepping
(use-package deadgrep)
(use-package wgrep)

;; some packages that i use are to replicate evil functionality
;; ;; highlight text inside common delimiters
(use-package expand-region
  :config
  (global-set-key (kbd "C-;") 'er/expand-region))

;; center buffer
(use-package olivetti)

;; depth-dependent coloring of code
(use-package prism
  :elpaca (prism :fetcher github :repo "alphapapa/prism.el"))

;; its great but it uses alot of cpu especially when the gif has a fast timer
(use-package org-inline-anim
  :config
  (add-hook 'org-mode-hook #'org-inline-anim-mode)
  ;; (add-hook 'org-mode-hook #'org-inline-anim-animate-all)
  (setq org-inline-anim-loop t)
  (add-hook 'org-babel-after-execute-hook 'org-inline-anim-animate))

(use-package julia-snail
  :ensure ( :fetcher github
            :repo "gcv/julia-snail")
            ;; :files ("*.el" "extensions" "*.jl" "*.toml" "extensions/*"))
  :config
  (setq julia-snail-terminal-type :eat)
  (setq julia-snail-extensions '(repl-history formatter ob-julia))
  (setq julia-snail/ob-julia-mirror-output-in-repl t)
  (setq julia-snail/ob-julia-capture-io nil))
(setq julia-snail-extensions '(repl-history formatter ob-julia)) ;; why is this not getting set on startup?
;; ;; (quelpa '(julia-snail :fetcher github
;; ;;                       :repo "gcv/julia-snail"
;; ;;                       :files ("*.el" "extensions" "*.jl" "*.toml" "extensions/*")))
;; ;; (add-hook 'julia-mode-hook 'julia-snail-mode)

;; for python, it doesnt work with corfu so i disabled it
;; (use-package elpy
;;   :init
;;   (elpy-enable))

;; mastodon fediverse
;; (use-package mastodon
;;   :ensure t)
;; (use-package ement)

;; vertico config
(use-package vertico
  :init
  (vertico-mode)
  ;; display vertico in different buffer
  (require 'vertico-buffer)
  (vertico-buffer-mode))
(defun crm-indicator (args)
  (cons (format "[CRM%s] %s"
                (replace-regexp-in-string
                 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                 crm-separator)
                (car args))
        (cdr args)))
(with-eval-after-load 'vertico
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  ;; do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  ;; use selected window
  (setq vertico-buffer-display-action '(display-buffer-same-window))
  )
;; enable recursive minibuffers
(setq enable-recursive-minibuffers t)
(use-package orderless
  :config
  (setq orderless-component-separator #'orderless-escapable-split-on-space
        completion-styles '(orderless partial-completion basic)))
(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t)
;; commands for ido-like directory navigation.
(use-package vertico-directory
  :elpaca nil
  :after vertico
  :ensure nil
  ;; more convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))
;; (use-package vertico-reverse)
;; (use-package vertico-grid
;;   :after vertico
;;   :ensure nil
;;   :config
;;   (vertico-grid-mode))

(use-package consult)

(use-package marginalia
  :config
  (marginalia-mode))

(use-package all-the-icons-completion
  :after (all-the-icons marginalia)
  :config
  ;; (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup))
  ;; (all-the-icons-completion-mode)
  (all-the-icons-completion-marginalia-setup))

;; prescient history location
;; (use-package prescient
;;   :config
;;   (prescient-persist-mode 1)
;;   (setq prescient-history-length 100000)
;;   (setq prescient-save-file (file-truename (from-brain "emacs_prescient")))) ;; save history to filesystem
;; (use-package vertico-prescient
;;   :config
;;   (vertico-prescient-mode))

;; virtual env integration for python
(use-package pyvenv)

(use-package combobulate
  :elpaca
  (combobulate :type git :host github :repo "mickeynp/combobulate")
  :hook
  ((python-ts-mode . combobulate-mode)
   (js-ts-mode . combobulate-mode)
   (css-ts-mode . combobulate-mode)
   (yaml-ts-mode . combobulate-mode)
   (json-ts-mode . combobulate-mode)
   (typescript-ts-mode . combobulate-mode)
   (tsx-ts-mode . combobulate-mode))
  :config
  (setq combobulate-key-prefix "C-c o"))

                                        ;(use-package embrace
                                        ;:config
                                        ;(global-set-key (kbd "C-,") #'embrace-commander)
  ;;; org-mode has a default binding for C-, override it
                                        ;(define-key org-mode-map (kbd "C-,") #'embrace-commander)
                                        ;(add-hook 'org-mode-hook #'embrace-org-mode-hook))

(use-package easy-kill)

;; emacs "workspaces"
(use-package perspective
  ;; :after consult
  :init
  (persp-mode)
  :config
  ;; make consult buffer switcher see only current perspective's buffers
  (consult-customize consult--source-buffer :hidden t :default nil)
  (add-to-list 'consult-buffer-sources persp-consult-source)
  ;; (add-hook 'kill-emacs-hook #'persp-state-save)
  (setq persp-state-default-file (from-brain "emacs_persp"))
  (add-hook 'kill-emacs-hook #'persp-state-save))

;; very buggy
;; (use-package org-src-context
;;   :elpaca (org-src-context :type git :host github :repo "karthink/org-src-context")
;;   :config
;;   (add-hook 'org-mode-hook #'org-src-context-mode))

;; (use-package wolfram)
;; (use-package wolfram-mode)
(use-package wolfram-mode
  :elpaca (wolfram-mode :fetcher github :repo "dalanicolai/wolfram-mode" :files ("*.el"))
  :config
  (load-library "ob-wolfram")
  (setq org-babel-wolfram-command "wolfram -script")
  (setq wolfram-program "wolfram"))

;; emacs-ipython-notebook
(use-package ein)

;; zeal docs
(use-package zeal-at-point)

;(use-package ob-julia-vterm
;:config
;(setq vterm-always-compile-module t)
;(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
;(add-to-list 'org-babel-load-languages '(julia-vterm . t))
;(defalias 'org-babel-execute:julia 'org-babel-execute:julia-vterm)
;(defalias 'org-babel-execute:julia 'org-babel-execute:julia-vterm)
;(defalias 'org-babel-variable-assignments:julia 'org-babel-variable-assignments:julia-vterm))

;;(use-package el-easydraw
;;  :elpaca
;;  (el-easydraw :type git :host github :repo "misohena/el-easydraw"))

;; (use-package org-timeblock)

;; (use-package org-modern-indent
;;   :elpaca (org-modern-indent :type git :host github :repo "jdtsmith/org-modern-indent")
;;   :config ; add late to hook
;;   (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

;; (use-package copilot
;;   :quelpa (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
;;   :config
;;   (global-copilot-mode))

;; requires external installation, but seems interesting
;; (use-package penel
;;   :quelpa (:host github :repo "semiosis/pen.el"))

;; (use-package math-symbol-lists)
;; (use-package latex-math-preview)

;; (use-package lastfm)
;; (use-package vuiet
;;   :config
;;   (setq browse-url-browser-function 'browse-url-chrome))

;; (use-package delve
;;   :quelpa (:repo "publicimageltd/delve" :host github))
;; (use-package svg-tag-mode)

;; (use-package alert)
;; (use-package ox-json)

;;(use-package org-tree-slide)
;;(use-package aio)
;;(use-package org-web-tools)
;;(use-package system-packages)
;;(use-package ox-pandoc)
;;(use-package org-ioslide)
;;(use-package google-this)
;;(use-package google-maps)

(use-package google-translate)
(use-package biblio)

;; i think its buggy..
;; (use-package jinx
;;   :init (global-jinx-mode)
;;   ;; :custom
;;   ;; (jinx-languages "en_US")
;;   ;; (global-jinx-modes '(text-mode prog-mode))
;;   ;; (jinx-include-faces
;;   ;;  '((prog-mode font-lock-doc-face)
;;   ;;    (conf-mode font-lock-comment-face)))
;;   ;; (jinx-exclude-regexps
;;   ;;  '((t "[A-Z]+\\>"
;;   ;;       "\\<[[:upper:]][[:lower:]]+\\>"
;;   ;;       "\\w*?[0-9\.'\"-]\\w*"
;;   ;;       "[a-z]+://\\S-+"
;;   ;;       "<?[-+_.~a-zA-Z][-+_.~:a-zA-Z0-9]*@[-.a-zA-Z0-9]+>?")))
;;   :bind
;;   (("M-$" . jinx-correct)))

;; links to specific pdf pages from org mode
(use-package org-pdftools)

(use-package hyperbole
  :elpaca (hyperbole :fetcher github :repo "rswgnu/hyperbole")
  :config
  (hyperbole-mode 1)
  ;; it overrides the M-return key for vertico :/
  (unbind-key "M-<return>" hyperbole-mode-map)
  (unbind-key "M-RET" hyperbole-mode-map))

(use-package denote
  :ensure ( :fetcher github :repo "protesilaos/denote" :ref "55dcf23")
  :config
  (setq denote-directory *notes-dir*
        denote-date-prompt-use-org-read-date t
        denote-file-type 'org)
  (add-hook 'dired-mode-hook #'denote-dired-mode)
  (add-to-list 'denote-file-types
               '(xopp :extension ".xopp" :date-function denote-date-org-timestamp :title-value-function identity :title-value-reverse-function denote-trim-whitespace))
  (setq org-agenda-files (denote-files-with-keyword "todo")))
(defun denote-files-with-keyword (keyword)
  (cl-remove-if-not (lambda (filepath) (member keyword (denote-extract-keywords-from-path filepath)))
                    (denote-directory-files)))
;; (setq org-agenda-files (denote-directory-files ".*todo.*"))

(use-package denote-menu
  :elpaca (denote-menu :fetcher github :repo "namilus/denote-menu"))
;; (use-package consult-notes)
;; (use-package citar-denote)
;; (use-package consult-explore)
;; (use-package denote-explore)
;; :elpaca (consult-notes :fetcher github :repo "mclear-tools/consult-notes"))
;; (use-package deft)
;; (use-package xeft)
;; (use-package citar)
;; (use-package reorg)
;; (use-package iscroll)

(use-package rg)

(use-package flyspell-correct
  :bind ("C-;" . flyspell-correct-wrapper))

;; (use-package hl-block-mode)

;; (use-package djvu2
;;   :ensure ( :fetcher github :repo "dalanicolai/djvu2.el"))

(setq enable-god nil)
(when enable-god
  (use-package god-mode
    :config
    (god-mode)
    (global-set-key (kbd "<escape>") #'god-mode-all)
    (setq god-exempt-major-modes nil)
    (setq god-exempt-predicates nil))
  (defun my-god-mode-update-cursor-type ()
    (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar)))
  (add-hook 'post-command-hook #'my-god-mode-update-cursor-type)
  )

;; latex auto activating snippets
;; it requries auctex, so disable it on android
(when (not (is-android-system))
  (use-package laas
    :hook (LaTeX-mode . laas-mode)
    :config ; do whatever here
    (aas-set-snippets 'laas-mode
                      ;; set condition!
                      :cond #'texmathp ; expand only while in math
                      "supp" "\\supp"
                      "On" "O(n)"
                      "O1" "O(1)"
                      "Olog" "O(\\log n)"
                      "Olon" "O(n \\log n)"
                      ;; bind to functions!
                      ;; "Sum" (lambda () (interactive)
                      ;;         (yas-expand-snippet "\\sum_{$1}^{$2} $0"))
                      ;; "Span" (lambda () (interactive)
                      ;;          (yas-expand-snippet "\\Span($1)$0"))
                      ;; add accent snippets
                      ;; :cond #'laas-object-on-left-condition
                      ;; "qq" (lambda () (interactive) (laas-wrap-previous-object "sqrt"))
                      )))

(use-package orglink)

;; hmmmm??
(use-package pulsar)
(use-package ace-link)
(use-package literate-calc-mode)
;; (use-package bm) ;; visual bookmarks, is this useful for evil?
;; (use-package lispy) ? is this needed for lispyville?
;; (use-package lispyville
;;   :after (evil general)
;;   :init
;;   (general-add-hook '(emacs-lisp-mode-hook lisp-mode-hook) #'lispyville-mode)
;;   :config
;;   (lispyville-set-key-theme '(operators c-w additional)))
;; (with-eval-after-load 'lispyville
;;   (lispyville-set-key-theme
;;    '(operators
;;      c-w
;;      (escape insert)
;;      (additional-movement normal visual motion))))
(use-package lentic)
(use-package org-download)
;; (use-package visual-regexp-steroids
;;   :ensure ( :host github :repo "benma/visual-regexp-steroids"))
(use-package browse-kill-ring)
(use-package vundo)
;; (use-package polymode)
(use-package tldr)
;; (use-package emacs-fancy-compilation)
;; (use-package dired-k)
(use-package racket-mode)
(use-package clojure-mode)
;; (use-package cider)
;; (use-package quack) ;; for racket and scheme
;; (use-package geiser) ;; for racket and scheme
;; (use-package org-ai)
;; (use-package gptel)
;; (use-package ellama)
(use-package org-super-agenda)
(use-package fasd)
(use-package aweshell
  :ensure ( :host github :repo "manateelazycat/aweshell"))
(use-package full-ack)
;; (use-package ack-el)
(use-package ansible)
(use-package emms)
(use-package fireplace)
(use-package ledger-mode)
(use-package wttrin)
(use-package ace-link)

;;(use-package emacs-webkit
;;  :ensure ( :fetcher github :repo "akirakyle/emacs-webkit"))

(use-package git-undo
  :ensure ( :host github :repo "jwiegley/git-undo-el"))

;; for mail, not used yet
;; (use-package lieer)

;; for offline docs
(use-package devdocs
  :config
  (add-hook 'python-mode-hook
            (lambda () (setq-local devdocs-current-docs '("python~3.9"))))
  (setq devdocs-data-dir (file-truename "~/data/devdocs")))
(use-package consult-dash)

(use-package apheleia)
  ;; :config
  ;; (apheleia-global-mode +1)) ;; the minor mode isnt needed, its for auto formatting on save
;; (add-to-list 'apheleia-mode-alist '(python-mode . ruff))
;; (add-to-list 'apheleia-mode-alist '(python-ts-mode . ruff)))

;; (use-package stripe-buffer
;;   :config
;;   (add-hook 'dired-mode-hook 'turn-on-stripe-buffer-mode)
;;   (add-hook 'org-mode-hook 'turn-on-stripe-table-mode))

(use-package litable)

(provide 'setup-packages)