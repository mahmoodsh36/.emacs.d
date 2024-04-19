;; fails to compile on android, also default recipe fails when used with elpaca
(when (not (is-android-system))
  (use-package auctex
    :ensure '(auctex
              :pre-build (("./autogen.sh")
                          ("./configure"
                           "--without-texmf-dir"
                           "--with-packagelispdir=./"
                           "--with-packagedatadir=./")
                          ("make"))
              :build (:not elpaca--compile-info) ;; Make will take care of this step
              :files ("*.el" "doc/*.info*" "etc" "images" "latex" "style")
              :version (lambda (_) (require 'tex-site) AUCTeX-version))
    ;;:hook
    ;; (LaTeX-mode . turn-on-prettify-symbols-mode)
    ;; (LaTeX-mode . reftex-mode)
    ;; (LaTeX-mode . (lambda () (corfu-mode -1)))
    ;; (LaTeX-mode . outline-minor-mode)
    ;; (LaTeX-mode . olivetti-mode))
    :config
    (add-hook 'plain-TeX-mode-hook 'LaTeX-mode) ;; why is this not the default?
    ;; (setq auto-mode-alist (cons '("\\.tex$" . latex-mode) auto-mode-alist))
  ))

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

;; auto completion
;; im sticking with company for now as corfu keeps crashing with org mode, plus slime doesnt work with corfu (for now)
(setq completion-ignore-case t) ;; case-insensitivity
(use-package corfu
  :init
  (global-corfu-mode)
  :custom
  (corfu-cycle t)
  (corfu-auto t) ;; i feel like this gets in the way so i wanna disable it
  (corfu-quit-no-match t)
  (corfu-auto-delay 0.1) ;; never set it to 0, makes emacs very laggy and hogs cpu
  ;; (corfu-separator ?_) ;; set to orderless separator, if not using space
  ;; (corfu-separator " ")
  (corfu-count 10)
  (corfu-indexed-mode t)
  (corfu-echo-mode t) ;; display brief documentation in echo area
  (corfu-popupinfo-mode t) ;; display documentation in popup
  (corfu-quit-at-boundary nil)
  (corfu-on-exact-match nil) ;; dont auto insert when there is an exact match
  (corfu-popupinfo-delay (cons 0 0)) ;; dont auto insert when there is an exact match
  ;; (corfu-auto-prefix 2)
  :config
  (unbind-key "RET" corfu-map)
  (unbind-key "TAB" corfu-map)
  (define-key corfu-map [tab] nil)
  (define-key corfu-map "\t" nil)
  (bind-key "C-TAB" #'corfu-complete corfu-map) ;; why does this not work
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

;; https://github.com/minad/cape
(use-package cape
  :init
  ;; somehow the value isnt really set but the completions work according to this list lol
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  ;; (add-to-list 'completion-at-point-functions #'cape-line) ;; too intrusive, dont enable
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;; (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-to-list 'completion-at-point-functions #'cape-history) ;; too intrusive
  (add-to-list 'completion-at-point-functions #'cape-sgml)
  (add-to-list 'completion-at-point-functions #'cape-rfc1345)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  ;; (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  )

;; for some reason ispell completion is enabled in org mode
(defun remove-ispell-cap ()
  (interactive)
  (setq-local completion-at-point-functions (delete 'ispell-completion-at-point completion-at-point-functions)))
(add-hook 'org-mode-hook #'remove-ispell-cap)

;; corfu completion in the minibuffer
(with-eval-after-load 'corfu
  (defun corfu-enable-in-minibuffer ()
    "enable corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      ;; (setq-local corfu-auto nil) enable/disable auto completion
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)
  )

;; for shell completion i think
(use-package pcmpl-args)

;; vertical completion interface
(use-package counsel
  :config
  ;;(ivy-mode)
  (setq ivy-height 20)
  (setq ivy-use-selectable-prompt t)
  (setq ivy-calling t)
  ;; (global-set-key (kbd "M-x") 'counsel-M-x)
  ;; (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  )

;; ;; more featureful ivy menus, it may cause some error when switching buffers
;; (use-package ivy-rich
;;   :config
;;   (ivy-rich-mode 1)
;;   (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))
;; ;; icons for ivy
;; (use-package all-the-icons-ivy-rich
;;   :config (all-the-icons-ivy-rich-mode 1))
;; (use-package ivy-bibtex)

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
(use-package dart-mode)

;; pdf viewer
(when (not (is-android-system))
  (use-package pdf-tools
    :ensure (:host github :repo "vedang/pdf-tools")
    :config
    (pdf-tools-install t)
    (add-hook 'pdf-view-mode-hook 'pdf-view-themed-minor-mode))

  (with-eval-after-load 'pdf-tools
    ;; workaround for pdf-tools not reopening to last-viewed page of the pdf:
    ;; https://github.com/politza/pdf-tools/issues/18#issuecomment-269515117
    (defun brds/pdf-set-last-viewed-bookmark ()
      (interactive)
      (when (eq major-mode 'pdf-view-mode)
        (bookmark-set (brds/pdf-generate-bookmark-name))))
    (defun brds/pdf-jump-last-viewed-bookmark ()
      (bookmark-set "fake") ; this is new
      (when
          (brds/pdf-has-last-viewed-bookmark)
        (bookmark-jump (brds/pdf-generate-bookmark-name))))
    (defun brds/pdf-has-last-viewed-bookmark ()
      (assoc
       (brds/pdf-generate-bookmark-name) bookmark-alist))
    (defun brds/pdf-generate-bookmark-name ()
      (concat "PDF-LAST-VIEWED: " (buffer-file-name)))
    (defun brds/pdf-set-all-last-viewed-bookmarks ()
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (brds/pdf-set-last-viewed-bookmark))))
    (add-hook 'kill-buffer-hook 'brds/pdf-set-last-viewed-bookmark)
    (add-hook 'pdf-view-mode-hook 'brds/pdf-jump-last-viewed-bookmark)
    (unless noninteractive  ; as `save-place-mode' does
      (add-hook 'kill-emacs-hook #'brds/pdf-set-all-last-viewed-bookmarks))
    )
  )

;; provides syntax highlighting when exporting from org mode to html
(use-package htmlize)

;; static website generation for org mode
(use-package ox-hugo
  :config
  (setq org-hugo-base-dir (file-truename "~/work/blog/"))
  ;; (setq org-more-dir (expand-file-name "~/work/blog/static/more/"))
  (setq *org-static-dir* (format "%s/static" org-hugo-base-dir))
  (add-to-list 'org-hugo-external-file-extensions-allowed-for-copying "webp")
  (add-to-list 'org-hugo-external-file-extensions-allowed-for-copying "html")
  ;;(plist-put org-html-latex-image-options :image-dir (file-truename (concat *org-static-dir* "/ltximg/"))))
  )

(use-package dap-mode)

(use-package mixed-pitch
  :hook
  (text-mode . mixed-pitch-mode))
(add-hook 'text-mode-hook
          (lambda ()
            (variable-pitch-mode 1)))

;; ;; give org mode a better look
(use-package org-modern
  ;; :config
  ;; (setq
  ;;  org-modern-hide-stars nil
  ;;  org-auto-align-tags nil
  ;;  org-tags-column 0
  ;;  org-catch-invisible-edits 'show-and-error
  ;;  org-special-ctrl-a/e t
  ;;  org-insert-heading-respect-content t
  ;;  ;; Agenda styling
  ;;  ;; org-agenda-tags-column 0
  ;;  ;; org-agenda-block-separator ?─
  ;;  ;; org-agenda-time-grid
  ;;  ;; '((daily today require-timed)
  ;;  ;;   (800 1000 1200 1400 1600 1800 2000)
  ;;  ;;   " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
  ;;  ;; org-agenda-current-time-string
  ;;  ;; "⭠ now ─────────────────────────────────────────────────"
  ;;  )
  ;; (global-org-modern-mode)
  )

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
  (add-hook 'org-mode-hook 'org-appear-mode t))

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
;; (use-package format-all)
;; (use-package plantuml-mode)

;; for latex references
;; (use-package org-ref
;;   :config
;;   (setq bibtex-completion-bibliography (list org-cite-global-bibliography))
;;   (define-key org-mode-map (kbd "C-c ]") 'org-ref-insert-link-hydra/body))

;; (use-package code-compass)

;; (use-package vterm
;; :custom
;; (vterm-always-compile-module t))

;; check which keys i press most
(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  (setq keyfreq-file (from-brain "emacs_keyfreq")))

;; has issues with transient versions/elpaca
(use-package magit)
  ;; :ensure ( :host github :repo "magit/magit"))

;; need the "global" package for gtags binary
;; (use-package ggtags
;;   :config
;;   (add-hook 'c-mode-common-hook
;;             (lambda ()
;;               (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
;;                 (ggtags-mode 1)))))
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

;; (use-package json-to-org-table :quelpa (:host github :repo "noonker/json-to-org-table"))

(use-package emmet-mode
  :config
  (add-hook 'mhtml-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook 'emmet-mode))

;; transclusions (including text from other documents) for org mode
(use-package org-transclusion
  :config
  ;; (add-hook 'org-mode-hook #'org-transclusion-mode)
  )

(use-package eat)

;; relative line numbers, really slows buffer redisplay down, can cause cpu spikes even when simply navigating code buffers
;; (use-package linum-relative
;;   :config
;;   (add-hook 'prog-mode-hook 'linum-relative-mode)
;;   ;; show the real line number at current line
;;   (setq linum-relative-current-symbol ""))

;; perfectly aligned org mode tables
;; painfully slows down org-mode, on block evaluation for example
(use-package valign
  ;; :hook
  ;; (org-mode . valign-mode)
  ;; :config
  ;; (setq valign-fancy-bar t)
  )

(use-package vimrc-mode)

(use-package sly
  :ensure (:host github :repo "joaotavora/sly")
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
  :ensure (prism :fetcher github :repo "alphapapa/prism.el"))

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
  (setq julia-snail/ob-julia-capture-io nil)
  (add-hook 'julia-mode-hook 'julia-snail-mode))
(with-eval-after-load 'julia-snail
  (setq julia-snail-extensions '(repl-history formatter ob-julia))) ;; why is this not getting set on startup?

;; vertico config
(use-package vertico
  :config
  (vertico-mode)
  ;; display vertico in different buffer
  (require 'vertico-buffer)
  (require 'vertico-grid)
  (require 'vertico-mouse)
  (require 'vertico-indexed)
  (require 'vertico-reverse)
  (vertico-buffer-mode)
  (vertico-indexed-mode)
  (vertico-mouse-mode)
  ;; (vertico-grid-mode)
  ;; (vertico-reverse-mode)
  ;; (setq vertico-grid-annotate 1)
  (setq vertico-buffer-display-action '(display-buffer-same-window))
  )
;; enable recursive minibuffers
(setq enable-recursive-minibuffers t)
(use-package orderless
  :config
  (setq read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t)
  (setq completion-styles '(orderless basic flex)
        orderless-component-separator #'orderless-escapable-split-on-space
        completion-category-overrides '((file (styles basic partial-completion)))
        )
  )
;; commands for ido-like directory navigation.
(use-package vertico-directory
  :ensure nil
  :after vertico
  :ensure nil
  ;; more convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package consult)

(use-package marginalia
  :config
  (marginalia-mode))

(use-package all-the-icons-completion
  :after (all-the-icons marginalia)
  :config
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)
  (all-the-icons-completion-mode)
  (all-the-icons-completion-marginalia-setup))

;; virtual env integration for python
(use-package pyvenv)

(use-package combobulate
  :ensure
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

;; emacs-ipython-notebook
(use-package ein)

;; zeal docs
(use-package zeal-at-point)

(use-package google-translate)

;; links to specific pdf pages from org mode
(when (not (is-android-system))
  (use-package org-pdftools))

(use-package hyperbole
  :ensure (hyperbole :fetcher github :repo "rswgnu/hyperbole")
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
  )
(with-eval-after-load 'denote
  ;; overwrite the export function, for some slight modifications
  (defun denote-link-ol-export (link description format)
    (let* ((path-id (denote-link--ol-resolve-link-to-target link :full-data))
           (path (file-relative-name (nth 0 path-id)))
           (id (nth 1 path-id))
           (search (nth 2 path-id))
           (anchor (file-name-sans-extension path))
           (path-no-ext (file-name-sans-extension (file-name-base path)))
           (desc (cond
                  (description)
                  (search (format "denote:%s::%s" id search))
                  (t (concat "denote:" id)))))
      (cond
       ((eq format 'html)
        (if search
            (format "<a href=\"%s.html%s\">%s</a>" anchor search desc)
          (format "<a href=\"%s.html\">%s</a>" anchor desc)))
       ((eq format 'latex) (format "\\href{%s}{%s}" (replace-regexp-in-string "[\\{}$%&_#~^]" "\\\\\\&" path) desc))
       ((eq format 'texinfo) (format "@uref{%s,%s}" path desc))
       ((eq format 'ascii) (format "[%s] <denote:%s>" desc path))
       ((eq format 'md) (format "[%s](../%s.md)" desc path-no-ext))
       (t path))))
  )

(use-package denote-menu
  :ensure (denote-menu :fetcher github :repo "namilus/denote-menu"))
;; (use-package consult-notes)
;; (use-package citar-denote)
;; (use-package consult-explore)
;; (use-package denote-explore)
;; :ensure (consult-notes :fetcher github :repo "mclear-tools/consult-notes"))
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
;; without this emacs chokes on djvu files
(use-package djvu)

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

;; (use-package orglink
;;   :config
;;   (global-orglink-mode))

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

(use-package litable)

;; allows for defining expressions based on regexp, unlike the builtin prettify-symbols-mode
(use-package pretty-mode)

(use-package ligature)

(use-package font-lock-studio)

;; (use-package consult-eglot)
(use-package sideline)

(use-package origami)

(use-package org-super-agenda
  ;; :config
  ;; (org-super-agenda-mode)
  )

(use-package biblio)
(use-package ebib)
(use-package zotra)

(use-package proof-general)

(use-package yasnippet-capf
  :after cape
  :ensure ( :host github :repo "elken/yasnippet-capf")
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

(use-package async-completing-read
  :ensure ( :host github :repo "oantolin/async-completing-read"))

;;(use-package consult-eglot)
;; (use-package consult-lsp)
(use-package consult-tex)
(use-package consult-compile-multi)
(use-package consult-flycheck)

(use-package consult-bibtex
  :ensure ( :host github :repo "mohkale/consult-bibtex"))

(use-package notmuch
  :config
  ;; setup the mail address and use name
  (setq mail-user-agent 'notmuch-user-agent)
  (setq user-mail-address "mahmod.m2015@gmail.com"
        user-full-name "mahmood sheikh")
  ;; smtp config
  (setq smtpmail-smtp-server "smtp.gmail.com"
        message-send-mail-function 'message-smtpmail-send-it)
  ;; report problems with the smtp server
  (setq smtpmail-debug-info t)
  ;; add Cc and Bcc headers to the message buffer
  (setq message-default-mail-headers "Cc: \nBcc: \n")
  ;; postponed message is put in the following draft directory
  (setq message-auto-save-directory (from-brain "mail/draft"))
  (setq message-kill-buffer-on-exit t)
  ;; change the directory to store the sent mail
  (setq message-directory (from-brain "mail/"))
  (setq notmuch-search-oldest-first nil)
  )

;; convert emacs regexes to other engine regexes and backwards
(use-package pcre2el)

;; for non-contiguous region selection
(use-package zones)

;; automatic tangling and detangling for org babel
(use-package org-tanglesync)

;; make using julia's language server easier with eglot.
(use-package eglot-jl)

(use-package command-log-mode)

(use-package consult-web
  :ensure ( :host github :repo "armindarvish/consult-web"))

(provide 'setup-packages)