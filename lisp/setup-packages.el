(use-package nix-mode
  :mode "\\.nix\\'")

(use-package general)

;; epub reader
(use-package nov)

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

;; projectile, for project browsing/management
;; (use-package projectile
;;   :straight (:host github :repo "bbatsov/projectile")
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
        :straight (:host github :repo "company-mode/company-mode")
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
      ;; :quelpa (:files (:defaults "extensions/*"))
      :init
      (global-corfu-mode)
      ;; :hook ((prog-mode . corfu-mode)
      ;;        (latex-mode . corfu-mode)
      ;;        (shell-mode . corfu-mode)
      ;;        (comint-mode . corfu-mode)
      ;;        (eshell-mode . corfu-mode))
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
      (unbind-key "RET" corfu-map)
      (unbind-key "TAB" corfu-map)
      (define-key corfu-map [tab] nil)
      (define-key corfu-map "\t" nil)
      (bind-key "M-TAB" #'corfu-complete corfu-map)
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

    (use-package cape)

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

    ;; (defun orderless-fast-dispatch (word index total)
    ;;   (and (= index 0) (= total 1) (length< word 4)
    ;;        `(orderless-regexp . ,(concat "^" (regexp-quote word)))))

    ;; (orderless-define-completion-style orderless-fast
    ;;                                    (orderless-style-dispatchers '(orderless-fast-dispatch))
    ;;                                    (orderless-matching-styles '(orderless-literal orderless-regexp)))

    ;; corfu completion in the minibuffer
    (defun corfu-enable-in-minibuffer ()
      "enable corfu in the minibuffer if `completion-at-point' is bound."
      (when (where-is-internal #'completion-at-point (list (current-local-map)))
        ;; (setq-local corfu-auto nil) enable/disable auto completion
        (corfu-mode 1)))
    (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)

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

;; save undos/redos even when buffer is killed or emacs restarts, this package is really buggy so i disabled it
;; (use-package undo-fu-session
;;   :config
;;   (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
;;   (global-undo-fu-session-mode))

;; executing sage in org babel
(use-package ob-sagemath
  :config
  ;; ob-sagemath supports only evaluating with a session.
  (setq org-babel-default-header-args:sage '((:session . t)
                                             (:results . "drawer")))
  (setq sage-shell:input-history-cache-file (brain-file "/sage_history"))
  (add-hook 'sage-shell-after-prompt-hook #'sage-shell-view-mode))

;; better built-in help/documentation
(use-package helpful
  :config
  (define-key help-map (kbd "f") #'helpful-callable)
  (define-key help-map (kbd "v") #'helpful-variable)
  (define-key help-map (kbd "a") #'helpful-symbol)
  (define-key help-map (kbd "k") #'helpful-key))

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
  (add-to-list 'warning-suppress-types '(yasnippet backquote-change)))
  ;; disable the default tab binding
  ;; (define-key yas-minor-mode-map [(tab)] nil)
  ;; (define-key yas-minor-mode-map (kbd "TAB") nil)
  ;; bind C-h to snippet expand
  ;; (define-key yas-minor-mode-map (kbd "C-h") #'yas-expand))
;; Bind `SPC' to `yas-expand' when snippet expansion available (it will still call `self-insert-command' otherwise).
;; (define-key yas-minor-mode-map (kbd "SPC") yas-maybe-expand))

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

(use-package literate-calc-mode)

;; generating linear ranges quickly
(use-package tiny
  :config
  (global-set-key (kbd "C-c t") 'tiny-expand))

;; modern API for working with files/dirs
(use-package f)

;; language server protocol support
;; getting errors about (void-variable lsp-ada-project-file)
(use-package lsp-mode
  :config
  (add-hook 'prog-mode-hook 'lsp-deferred)
  ;; gets rid of some annoying prompts to add project root when visiting definition of symbol
  ;; (setq lsp-auto-guess-root t)
  ;; another annoying warning
  (setq lsp-warn-no-matched-clients nil)
  )
(if (not enable-company)
    (use-package lsp-mode
      :custom
      (lsp-completion-provider :none) ;; we use corfu!
      :init
      ;; (defun my/orderless-dispatch-flex-first (_pattern index _total)
      ;;   (and (eq index 0) 'orderless-flex))
      ;; (defun my/lsp-mode-setup-completion ()
      ;;   (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
      ;;         '(orderless)))
      ;; Optionally configure the first word as flex filtered.
      ;; (add-hook 'orderless-style-dispatchers #'my/orderless-dispatch-flex-first nil 'local)
      ;; Optionally configure the cape-capf-buster.
      (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point)))
      ;; :hook
      ;; (lsp-completion-mode . my/lsp-mode-setup-completion)
      ))

;; show simple info on the right
(use-package lsp-ui
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (setq lsp-ui-doc-delay 0
        lsp-ui-sideline-delay 0
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-symbol t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-doc-enable t
        lsp-ui-sideline-enable t
        lsp-lens-enable t
        lsp-completion-show-detail t)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

;; lsp support for treemacs
(use-package lsp-treemacs
  :config
  (lsp-treemacs-sync-mode 1))

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
(use-package lsp-dart)

;; best pdf viewer
;; main repo for pdf-tools, below another repo is used for continuous scroll
(use-package pdf-tools
  :config
  (pdf-tools-install t)
  (add-hook 'pdf-view-mode-hook 'pdf-view-themed-minor-mode))

;; history for ivy completion, it sometimes makes ivy really slow, so maybe remove the cache file every once in a while
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

;; best latex preview functionality
;; actually i dont use this anymore since org-mode devs implemented a better method
;; (use-package xenops
;;   :config
;;   (setq xenops-reveal-on-entry t
;;         xenops-math-latex-max-tasks-in-flight 3
;;         xenops-math-latex-process 'dvisvgm)
;;   ;; (add-hook 'LaTeX-mode-hook #'xenops-mode)
;;   ;; (add-hook 'org-mode-hook #'xenops-mode)
;;   (add-hook 'xenops-mode-hook 'xenops-render)
;;   ;; (add-hook 'xenops-mode-hook 'xenops-xen-mode)
;;   (add-hook 'org-babel-after-execute-hook (lambda ()
;;                                             (interactive)
;;                                             (ignore-errors (xenops-render))))
;;   ;; (setq xenops-math-image-scale-factor 1.1)
;;   ;; (setq xenops-math-image-current-scale-factor 1.1)
;;   (setcar (cdr (car xenops-elements))
;;           '(:delimiters
;;             ("^[ 	]*\\\\begin{\\(align\\|equation\\|gather\\)\\*?}" "^[ 	]*\\\\end{\\(align\\|equation\\|gather\\)\\*?}")
;;             ("^[ 	]*\\\\\\[" "^[ 	]*\\\\\\]")))
;;   ;; for inline previews
;;   (advice-add 'xenops-math-latex-get-colors :filter-return
;;               (lambda (col)
;;                 (interactive)
;;                 (list (org-latex-color :foreground) (org-latex-color :background))))
;;                 ;; (list (org-latex-color :foreground) (org-latex-color :background))))
;;   ;; for code blocks i think, i make backgrounds transparent so dont gotta set a color
;;   (plist-put org-format-latex-options :foreground "black")
;;   ;; (add-to-list 'xenops-math-latex-process-alist
;;   ;;              '(mymath :programs ("latex" "dvisvgm")
;;   ;;                       :description "pdf > svg"
;;   ;;                       :message "you need to install the programs: latex and dvisvgm (they come together in texlive distro)."
;;   ;;                       :image-input-type "dvi"
;;   ;;                       :image-output-type "svg"
;;   ;;                       :image-size-adjust (1.7 . 1.5)
;;   ;;                       :latex-compiler ("latex -output-directory %o %f")
;;   ;;                       :image-converter ("dvisvgm %f -o %O")))
;;   ;; this fixes issues with dvisvgm, phhhewww, took me a long time to figure out.
;;   (defun preamble-advice (document)
;;     (concat "\\def\\pgfsysdriver{pgfsys-tex4ht.def}" document))
;;   (advice-add 'xenops-math-latex-make-latex-document :filter-return 'preamble-advice)
;;   )

;; (use-package mixed-pitch
;;   :hook
;;   (text-mode . mixed-pitch-mode))
;; (add-hook 'text-mode-hook #'variable-pitch-mode)

;; add edition/creation timestamps to headers and files, this is absurd, git would be a bbetter option
;; (use-package org-roam-timestamps
;;   :config
;;   (org-roam-timestamps-mode)
;;   (setq org-roam-timestamps-remember-timestamps t))

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
;; (use-package org-appear
;;   :config
;;   (setq org-appear-autoemphasis t
;;         org-appear-autoentities t
;;         org-appear-autokeywords t
;;         org-appear-autolinks t
;;         org-appear-autosubmarkers t)
;;   (add-hook 'org-mode-hook 'org-appear-mode))

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
;; (use-package ob-async)
(use-package format-all)
;; (use-package org-roam-ui)
;; (use-package jupyter)
;; (use-package plantuml-mode)
;; for latex references, i think it can do those (not sure)
(use-package org-ref
  :config
  (setq bibtex-completion-bibliography (list org-cite-global-bibliography))
  (define-key org-mode-map (kbd "C-c ]") 'org-ref-insert-link-hydra/body))
;; (use-package code-compass)

;; (use-package vterm)

;; check which keys i press most
(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  (setq keyfreq-file (concat *brain-dir* "emacs_keyfreq")))

(use-package magit)

;; need the "global" package for gtags binary
(use-package ggtags
  :config
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
                (ggtags-mode 1)))))
;; (use-package counsel-gtags)

(use-package git-auto-commit-mode)
(use-package avy)
;; (use-package auto-yasnippet)

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ;;("C-;" . embark-dwim)        ;; good alternative: M-.
   ;; ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
   )
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
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

;; (use-package ialign)
;; (use-package 'org-protocol-capture-html)
;; (use-package org-download)

;; evaulation overlay for elisp
(use-package eros
  :config
  (eros-mode 1))

;; live web dev
(use-package skewer-mode)

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

;; (quelpa '(eat :fetcher git
;;               :url "https://codeberg.org/akib/emacs-eat"
;;               :files ("*.el" ("term" "term/*.el") "*.texi"
;;                       "*.ti" ("terminfo/e" "terminfo/e/*")
;;                       ("terminfo/65" "terminfo/65/*")
;;                       ("integration" "integration/*")
;;                       (:exclude ".dir-locals.el" "*-tests.el"))))

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

(use-package org-super-agenda)

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
;; (use-package valign
;;   :hook
;;   (org-mode . valign-mode)
;;   :config
;;   (setq valign-fancy-bar t))

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
  :straight (:host github :repo "joaotavora/sly")
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
  (setq sly-mrepl-history-file-name (concat *brain-dir* "/sly_history"))
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
;;   (setq slime-repl-history-file (concat *brain-dir* "/slime_history"))
;;   (setq slime-repl-history-size 1000000))

;; flash cursor when jumping around
;; (use-package beacon
;;   :config
;;   (beacon-mode 1))

;; better alternative to counsel-ag, best i found for grepping
(use-package deadgrep)

;; some packages that i use are to replicate evil functionality
;; ;; highlight text inside common delimiters
(use-package expand-region
  :config
  (global-set-key (kbd "C-;") 'er/expand-region))

;; ;; change text inside delimiters
;; (use-package change-inner)

;; center buffer
(use-package olivetti)

;; depth-dependent coloring of code
(use-package prism
  :straight (prism :fetcher github :repo "alphapapa/prism.el"))

;; its great but it uses alot of cpu especially when the gif has a fast timer
(use-package org-inline-anim
  :config
  (add-hook 'org-mode-hook #'org-inline-anim-mode)
  ;; (add-hook 'org-mode-hook #'org-inline-anim-animate-all)
  (setq org-inline-anim-loop t)
  (add-hook 'org-babel-after-execute-hook 'org-inline-anim-animate))

;; (quelpa '(julia-snail :fetcher github
;;                       :repo "gcv/julia-snail"
;;                       :files ("*.el" "extensions" "*.jl" "*.toml" "extensions/*")))
;; (add-hook 'julia-mode-hook 'julia-snail-mode)
;; (setq julia-snail-terminal-type :eat)
;; (setq julia-snail-extensions '(repl-history formatter ob-julia))
;; ;;(setq julia-snail-extensions '(repl-history formatter))
;; (setq julia-snail/ob-julia-mirror-output-in-repl t)
;; (setq julia-snail/ob-julia-capture-io nil)

;; from https://github.com/karthink/.emacs.d/blob/master/lisp/setup-org.el
;; (use-package ob-julia
;;   :straight (ob-julia :host github :repo "nico202/ob-julia"
;;                       :files ("*.el" "julia")
;;                       :fork (:host github
;;                              :repo "karthink/ob-julia"
;;                              :branch "main"))
;;   :after ob
;;   :hook (org-babel-julia-after-async-execute . my/org-redisplay-babel-result)
;;   :init (setq ob-julia-insert-latex-environment-advice nil)
;;   :config
;;   (add-to-list 'org-structure-template-alist
;;                '("j" . "src julia"))
;;   (setq org-babel-default-header-args:julia
;;         '((:session . nil)
;;           (:async   . "yes")))
;;   (setq org-babel-julia-backend 'julia-snail)
;;   (when (featurep 'ess)
;;     (setq ess-eval-visibly 'nowait)
;;     (defun org-babel-julia-initiate-session (&optional session params)
;;       "Create or switch to an ESS Julia session.
;;
;; Return the initialized session, if any."
;;       (unless (string= session "none")
;;         (let ((session (or session "*julia*")))
;;           (if (org-babel-comint-buffer-livep session)
;;               session
;;             (save-window-excursion
;;               (org-babel-prep-session:julia session params))))))))


;; for python, it doesnt work with corfu so i disabled it
;; (use-package elpy
;;   :init
;;   (elpy-enable))

;; mastodon fediverse
(use-package mastodon
  :ensure t)
(use-package ement)

;; vertico config
(use-package vertico
  :init
  (vertico-mode))
(defun crm-indicator (args)
  (cons (format "[CRM%s] %s"
                (replace-regexp-in-string
                 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                 crm-separator)
                (car args))
        (cdr args)))
(advice-add #'completing-read-multiple :filter-args #'crm-indicator)
;; do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
;; emacs 28: Hide commands in M-x which do not work in the current mode.
;; vertico commands are hidden in normal buffers.
;; (setq read-extended-command-predicate
;;       #'command-completion-default-include-p)
;; enable recursive minibuffers
(setq enable-recursive-minibuffers t)
;; optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
        orderless-component-separator "\s+"
        corfu-separator orderless-component-separator)
  (setq completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))
(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t)
;; display vertico in different buffer
(require 'vertico-buffer)
(vertico-buffer-mode)
;; use selected window
(setq vertico-buffer-display-action '(display-buffer-same-window))
;; Commands for Ido-like directory navigation.
;; Configure directory extension.
(use-package vertico-directory
  :straight nil
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))
;; (use-package vertico-reverse)
;; (use-package vertico-grid
;;   :after vertico
;;   :ensure nil
;;   :config
;;   (vertico-grid-mode))
;; is similar to ivy-rich
(use-package consult)
;; corfu with orderless
(defun orderless-fast-dispatch (word index total)
  (and (= index 0) (= total 1) (length< word 4)
       `(orderless-regexp . ,(concat "^" (regexp-quote word)))))
(orderless-define-completion-style orderless-fast
  (orderless-style-dispatchers '(orderless-fast-dispatch))
  (orderless-matching-styles '(orderless-literal orderless-regexp orderless-flex)))
(setq completion-styles '(substring orderless-fast basic))

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
;;   (setq prescient-save-file (file-truename (concat *brain-dir* "emacs_prescient")))) ;; save history to filesystem
;; (use-package vertico-prescient
;;   :config
;;   (vertico-prescient-mode))

;; virtual env integration for python
(use-package pyvenv)

;; (use-package multiple-cursors)
;;  :config
;;  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;;  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;;  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;;  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines))

(use-package combobulate
  :straight
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

(use-package embrace
  :config
  (global-set-key (kbd "C-,") #'embrace-commander)
  ;; org-mode has a default binding for C-, override it
  (define-key org-mode-map (kbd "C-,") #'embrace-commander)
  (add-hook 'org-mode-hook #'embrace-org-mode-hook))

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
  (setq persp-state-default-file (concat *brain-dir* "/emacs_persp"))
  (add-hook 'kill-emacs-hook #'persp-state-save))

;; very buggy
;; (use-package org-src-context
;;   :straight (org-src-context :type git :host github :repo "karthink/org-src-context")
;;   :config
;;   (add-hook 'org-mode-hook #'org-src-context-mode))

;; (use-package wolfram)
;; (use-package wolfram-mode)
(use-package wolfram-mode
  :straight (wolfram-mode :fetcher github :repo "dalanicolai/wolfram-mode" :files ("*.el"))
  :config
  (load-library "ob-wolfram")
  (setq org-babel-wolfram-command "wolfram -script")
  (setq wolfram-program "wolfram"))
;; from https://github.com/tririver/ob-mathematica/
;; (when (file-exists-p "~/.emacs.d/ob-mathematica.el")
;;  (load-file "~/.emacs.d/ob-mathematica.el"))

;; emacs-ipython-notebook
(use-package ein)

;; zeal docs
(use-package zeal-at-point)

;; devdocs seems to have more docs than zeal?
(use-package devdocs)
  ;; :config
  ;; (add-hook 'python-mode-hook
  ;;         (lambda () (setq-local devdocs-current-docs '("python~3.9")))))

;; (use-package ob-julia-vterm
;;   :config
;;   (add-to-list 'org-babel-load-languages '(julia-vterm . t))
;;   (defalias 'org-babel-execute:julia 'org-babel-execute:julia-vterm)
;;   (defalias 'org-babel-variable-assignments:julia 'org-babel-variable-assignments:julia-vterm))

;;(use-package el-easydraw
;;  :straight
;;  (el-easydraw :type git :host github :repo "misohena/el-easydraw"))

;; (use-package org-timeblock)

;; (use-package org-modern-indent
;;   :straight (org-modern-indent :type git :host github :repo "jdtsmith/org-modern-indent")
;;   :config ; add late to hook
;;   (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

;; this just doesnt work...
;; (use-package roam-block
;;   :quelpa (roam-block :fetcher github :repo "Kinneyzhang/roam-block")
;;   :config
;;   (setq roam-block-home (list *brain-dir*)
;;         roam-block-ref-highlight t
;;         roam-block-embed-highlight t))

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

;; (use-package org-ml)
;; (use-package lispy)
;; (use-package ein)

;; (use-package delve
;;   :quelpa (:repo "publicimageltd/delve" :host github))
;; (use-package svg-tag-mode)

;; (use-package alert)
;; (use-package ox-json)

;;(use-package org-tree-slide)
;;(use-package orgajs) installed externally i think
;;(use-package roam-block)
;;(use-package aio)
;;(use-package org-web-tools)
;;(use-package system-packages)
;;(use-package ox-pandoc)
;;(use-package org-html-themes)
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
  :straight (hyperbole :fetcher github :repo "rswgnu/hyperbole")
  :config
  (hyperbole-mode 1)
  ;; it overrides the M-return key for vertico :/
  (unbind-key "M-<return>" hyperbole-mode-map)
  (unbind-key "M-RET" hyperbole-mode-map))

(use-package denote
  :straight (denote :fetcher github :repo "protesilaos/denote"))
(use-package denote-menu
  :straight (denote-menu :fetcher github :repo "namilus/denote-menu"))
;; (use-package consult-notes)
;; (use-package citar-denote)
;; (use-package consult-explore)
  ;; :straight (consult-notes :fetcher github :repo "mclear-tools/consult-notes"))
;; (use-package deft)

(provide 'setup-packages)