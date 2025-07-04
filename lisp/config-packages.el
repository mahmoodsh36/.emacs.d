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
    :hook
    (LaTeX-mode . turn-on-prettify-symbols-mode)
    (LaTeX-mode . reftex-mode)
    (LaTeX-mode . outline-minor-mode)
    ;; (LaTeX-mode . olivetti-mode)
    :config
    (add-hook 'plain-TeX-mode-hook 'LaTeX-mode) ;; why is this not the default?
    ;; (add-to-list 'auto-mode-alist '("\\.tex$" . LaTeX-mode))
  ))

;; has issues with transient versions/elpaca, build seems to fail on android
(when (not (is-android-system))
  (use-package transient) ;; to avoid issues with magit we have to grab transient too
  (use-package magit))
  ;; :ensure ( :host github :repo "magit/magit"))

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
;; (defun setup-tide-mode ()
;;   (interactive)
;;   (tide-setup)
;;   (flycheck-mode +1)
;;   (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;   (eldoc-mode +1)
;;   (tide-hl-identifier-mode +1)
;;   ;; company is an optional dependency. You have to
;;   ;; install it separately via package-install
;;   ;; `M-x package-install [ret] company`
;;   (company-mode +1))
;; formats the buffer before saving
;; (add-hook 'before-save-hook 'tide-format-before-save)
;; if you use typescript-mode
;; (add-hook 'typescript-mode-hook #'setup-tide-mode)
;; if you use treesitter based typescript-ts-mode (emacs 29+)
;; (add-hook 'typescript-ts-mode-hook #'setup-tide-mode)

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
  ;; (unbind-key "TAB" corfu-map)
  ;; to unbind tab completiuon
  ;; (define-key corfu-map [tab] nil)
  ;; (define-key corfu-map "\t" nil)
  ;; (bind-key "C-TAB" #'corfu-complete corfu-map) ;; why does this not work
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
  ;; (add-to-list 'completion-at-point-functions #'cape-dict)
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
  (setq ivy-height 40)
  (setq ivy-use-selectable-prompt t)
  (setq ivy-calling t)
  ;; (global-set-key (kbd "M-x") 'counsel-M-x)
  ;; (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (setq ivy-re-builders-alist '((t . orderless-ivy-re-builder)))
  (add-to-list 'ivy-highlight-functions-alist '(orderless-ivy-re-builder . orderless-ivy-highlight))
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

;; package to help making http requests
(use-package request)

;; highlights color names with the corresponding color
;; replaced this with colorful-mode
;; (use-package rainbow-mode
;;   :config
;;   (add-hook 'text-mode-hook 'rainbow-mode))

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

  ;; unbind yasnippet from tab
  (define-key yas-minor-mode-map [(tab)] nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)

  ;; some snippets i dont wanna create files for
  (yas-define-snippets
   'latex-mode
   '(("pt" "\\ptvct{$1}$0" "point (coordinate vector or whatever one'd call it)")
     ))
  (yas-define-snippets
   'text-mode
   '(("tm" "`(current-time-string)`" "current Time")
     ))
  )

;; auto expand snippets, https://github.com/joaotavora/yasnippet/issues/998
(setq my-yas-auto-expand nil)
(defun toggle-yas-auto-expand ()
  (interactive)
  (setq my-yas-auto-expand (not my-yas-auto-expand))
  (if my-yas-auto-expand
      (add-to-list 'yas-key-syntaxes 'yas-longest-key-from-whitespace)
    (setq yas-key-syntaxes (cl-delete 'yas-longest-key-from-whitespace yas-key-syntaxes :test 'equal)))
  (message "my-yas-auto-expand set to %s" my-yas-auto-expand))
(defun my-yas-try-expanding-auto-snippets ()
  (when (and (boundp 'yas-minor-mode)
             yas-minor-mode
             my-yas-auto-expand
             (cl-member this-command '(self-insert-command org-self-insert-command)))
    ;; (let ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
    (yas-expand)))
(add-hook 'post-command-hook #'my-yas-try-expanding-auto-snippets)

;; highlight errors in code
;; (use-package flycheck
;;   :config
;;   (global-flycheck-mode))

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

;; display available keybindings, i think its builtin now
;; (use-package which-key
;;   :config
;;   (which-key-mode 1)
;;   (setq which-key-allow-evil-operators t))

;; small flash when evaluating a sexp
;; (use-package eval-sexp-fu)

;; flutter setup
;; (use-package flutter)
;; (use-package dart-mode)

;; pdf viewer
(when (not (is-android-system))
  (use-package pdf-tools
    :ensure (:host github :repo "vedang/pdf-tools")
    :after (org org-latex-preview)
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
;; (use-package ox-hugo
;;   :config
;;   (setq org-hugo-base-dir (file-truename "~/work/blog/"))
;;   ;; (setq org-more-dir (expand-file-name "~/work/blog/static/more/"))
;;   (setq *org-static-dir* (format "%s/static" org-hugo-base-dir))
;;   (add-to-list 'org-hugo-external-file-extensions-allowed-for-copying "webp")
;;   (add-to-list 'org-hugo-external-file-extensions-allowed-for-copying "html")
;;   ;;(plist-put org-html-latex-image-options :image-dir (file-truename (concat *org-static-dir* "/ltximg/"))))
;;   )

(use-package dap-mode)

;; (use-package mixed-pitch
;;   :hook
;;   (text-mode . mixed-pitch-mode))
;; (add-hook 'text-mode-hook
;;           (lambda ()
;;             (variable-pitch-mode 1)))

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
;; (use-package org-appear
;;   :after org
;;   :config
;;   (setq org-appear-autoemphasis t
;;         org-appear-autoentities t
;;         org-appear-autokeywords t
;;         org-appear-autolinks t
;;         org-appear-autosubmarkers t
;;         ;; org-hide-emphasis-markers t
;;         )
;;   (add-hook 'org-mode-hook 'org-appear-mode t))

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

(use-package eat
  :config
  (setq eat-shell *shell-program*))

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
        `((sbcl ("sbcl"
                 "--dynamic-space-size" "12GB"
                 "--load"
                 ,(join-path (expand-file-name "~/work/cl-tools/myloader.lisp"))))
          (clisp ("clisp"))
          (ecl ("ecl"))
          (cmucl ("cmucl"))
          (ccl ("ccl"))
          (maxima ("rmaxima" "-r" "to_lisp();"))))
  ;; dont truncate my outputs!
  (setq sly-truncate-lines nil)
  (setq-default sly-truncate-lines nil)
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
(use-package deadgrep
  :config
  (setq deadgrep--search-type 'regexp))
(use-package wgrep)

;; some packages that i use are to replicate evil functionality
;; ;; highlight text inside common delimiters
;; (use-package expand-region
;;   :config
;;   (global-set-key (kbd "C-;") 'er/expand-region))

;; center buffer
(use-package olivetti)

;; depth-dependent coloring of code
(use-package prism
  :ensure (prism :fetcher github :repo "alphapapa/prism.el"))

;; its great but it uses alot of cpu especially when the gif has a fast timer
;; (use-package org-inline-anim
;;   :config
;;   (add-hook 'org-mode-hook #'org-inline-anim-mode)
;;   ;; (add-hook 'org-mode-hook #'org-inline-anim-animate-all)
;;   (setq org-inline-anim-loop t)
;;   (add-hook 'org-babel-after-execute-hook 'org-inline-anim-animate))

(use-package julia-snail
  :ensure ( :fetcher github
            :repo "gcv/julia-snail")
            ;; :files ("*.el" "extensions" "*.jl" "*.toml" "extensions/*"))
  :config
  (setq julia-snail-terminal-type :eat)
  (setq julia-snail-extensions '(repl-history formatter ob-julia)) ;; why is this not getting set on startup?
  (setq julia-snail/ob-julia-mirror-output-in-repl t)
  (setq julia-snail/ob-julia-capture-io nil)
  (add-hook 'julia-mode-hook 'julia-snail-mode)
  ;; (setq julia-snail-executable "LD_LIBRARY_PATH=/run/opengl-driver/lib/ julia")
  )

;; has a julia repl that uses comint, ~inferior-julia~
;; (use-package julia-mode)

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
  ;; (vertico-buffer-mode)
  (vertico-indexed-mode)
  (vertico-mouse-mode)
  ;; (vertico-grid-mode)
  ;; (vertico-reverse-mode)
  (setq vertico-grid-annotate 1)
  (setq vertico-buffer-display-action '(display-buffer-same-window))
  (setq vertico-count 25)
  )
;; enable recursive minibuffers
(setq enable-recursive-minibuffers t)
(use-package orderless
  :config
  (setq read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t)
  (setq completion-styles '(orderless basic) ;; flex is really slow so i removed it
        orderless-component-separator #'orderless-escapable-split-on-space
        completion-category-overrides '((file (styles basic partial-completion)))
        )
  )
;; commands for ido-like directory navigation.
(use-package vertico-directory
  :ensure nil
  :after vertico
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
  :ensure (combobulate :type git :host github :repo "mickeynp/combobulate")
  :hook
  ((python-ts-mode . combobulate-mode)
   (js-ts-mode . combobulate-mode)
   (css-ts-mode . combobulate-mode)
   (yaml-ts-mode . combobulate-mode)
   (json-ts-mode . combobulate-mode)
   (typescript-ts-mode . combobulate-mode)
   (tsx-ts-mode . combobulate-mode))
  :config
  (setq combobulate-key-prefix "C-c o")

  ;; here’s some example code that navigates to the next dictionary, list or set, see https://www.masteringemacs.org/article/combobulate-structured-movement-editing-treesitter
  (defun move-to-next-container ()
    (interactive)
    (with-navigation-nodes (:nodes '("dictionary" "set" "list"))
      (combobulate-visual-move-to-node
       (combobulate-nav-logical-next) t)))

  ;; (defvar combobulate-edit-map
  ;;   (let ((map (make-sparse-keymap)))
  ;;     (pcase-dolist (`(,k . ,f)
  ;;                    '(("u" . combobulate-navigate-up-list-maybe)
  ;;                      ("f" . combobulate-navigate-forward)
  ;;                      ("b" . combobulate-navigate-backward)
  ;;                      ("d" . combobulate-navigate-down-list-maybe)
  ;;                      ("k" . combobulate-kill-node-dwim)
  ;;                      ("n" . combobulate-navigate-next)
  ;;                      ("p" . combobulate-navigate-previous)
  ;;                      ("J" . combobulate-splice)
  ;;                      ("a" . combobulate-navigate-beginning-of-defun)
  ;;                      ("e" . combobulate-navigate-end-of-defun)
  ;;                      ("\\" . indent-region)
  ;;                      ("/" . undo)
  ;;                      ("t" . combobulate-transpose-sexps)
  ;;                      ("x" . eval-defun)))
  ;;       (define-key map (kbd k) f))
  ;;     map))
  ;; (map-keymap
  ;;  (lambda (_ cmd)
  ;;    (put cmd 'repeat-map 'combobulate-edit-map))
  ;;  combobulate-edit-map)
  )

(use-package easy-kill)

;; emacs "workspaces"
(use-package perspective
  ;; :after consult
  :init
  (persp-mode)
  :config
  (consult-customize consult--source-buffer :hidden t :default nil)
  (add-to-list 'consult-buffer-sources persp-consult-source)
  (setq persp-state-default-file (from-brain "emacs_persp"))
  (add-hook 'kill-emacs-hook #'persp-state-save))

;; emacs-ipython-notebook
(use-package ein)

;; zeal docs
(use-package zeal-at-point)

(use-package google-translate)

;; links to specific pdf pages from org mode
(when (not (is-android-system))
  (use-package org-pdftools
    :after 'org))

(use-package rg)

;; (use-package flyspell-correct
;;   :bind ("C-;" . flyspell-correct-wrapper))

;; (use-package hl-block-mode)

;; (use-package djvu2
;;   :ensure ( :fetcher github :repo "dalanicolai/djvu2.el"))
;; without this emacs chokes on djvu files
(use-package djvu)

;; latex auto activating snippets
;; it requries auctex, so disable it on android
;; (when (not (is-android-system))
;;   (use-package laas
;;     :hook (LaTeX-mode . laas-mode)
;;     :config ; do whatever here
;;     (aas-set-snippets 'laas-mode
;;                       ;; set condition!
;;                       :cond #'texmathp ; expand only while in math
;;                       "supp" "\\supp"
;;                       "On" "O(n)"
;;                       "O1" "O(1)"
;;                       "Olog" "O(\\log n)"
;;                       "Olon" "O(n \\log n)"
;;                       ;; bind to functions!
;;                       ;; "Sum" (lambda () (interactive)
;;                       ;;         (yas-expand-snippet "\\sum_{$1}^{$2} $0"))
;;                       ;; "Span" (lambda () (interactive)
;;                       ;;          (yas-expand-snippet "\\Span($1)$0"))
;;                       ;; add accent snippets
;;                       ;; :cond #'laas-object-on-left-condition
;;                       ;; "qq" (lambda () (interactive) (laas-wrap-previous-object "sqrt"))
;;                       )))

;; (use-package orglink
;;   :config
;;   (global-orglink-mode))

;; hmmmm??
;; (use-package pulsar)
(use-package literate-calc-mode)
;; (use-package visual-regexp)
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
;; (use-package lentic)
(use-package org-download)
;; (use-package visual-regexp-steroids
;;   :ensure ( :host github :repo "benma/visual-regexp-steroids"))
(use-package browse-kill-ring)
;; (use-package vundo)
;; (use-package polymode)
;; (use-package tldr)
;; (use-package emacs-fancy-compilation)
;; (use-package dired-k)
;; (use-package racket-mode)
;; (use-package clojure-mode)
;; (use-package cider)
;; (use-package quack) ;; for racket and scheme
;; (use-package geiser) ;; for racket and scheme
;; (use-package org-ai)
;; (use-package ellama)
;; (use-package fasd)
;; (use-package aweshell
;;   :ensure ( :host github :repo "manateelazycat/aweshell"))
;; (use-package full-ack)
;; (use-package ack-el)
;; (use-package ansible)
;; (use-package emms)
;; (use-package fireplace)
;; (use-package ledger-mode)
;; (use-package wttrin)
;; (use-package ace-link)

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

;; the overlays it places are all over the place..
;; (use-package litable
;;   :config
;;   (add-hook 'emacs-lisp-mode-hook #'litable-mode))

;; allows for defining expressions based on regexp, unlike the builtin prettify-symbols-mode
(use-package pretty-mode)

(use-package ligature)

(use-package font-lock-studio)

;; (use-package consult-eglot)
(use-package sideline)

;; errors out in batch mode?
;; (use-package origami)

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
;; (use-package consult-flycheck)

(use-package consult-bibtex
  :ensure ( :host github :repo "mohkale/consult-bibtex"))

(use-package notmuch
  :config
  ;; setup the mail address and use name
  (setq mail-user-agent 'message-user-agent)
  (setq user-mail-address "mahmod.m2015@gmail.com"
        user-full-name "mahmood sheikh")
  ;; smtp config
  (setq smtpmail-smtp-server "smtp.gmail.com"
        message-send-mail-function 'smtpmail-send-it
        smtpmail-stream-type 'ssl
        smtpmail-smtp-service 465)
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
  ;; (setq notmuch-search-result-format
  ;;       '(("date" . "%12s ") ("count" . "%-6s ") ("authors" . "%-20s ")
  ;;         ("subject" . "%s ") ("tags" . "(%s)")))
  )

;; convert emacs regexes to other engine regexes and backwards
(use-package pcre2el)

;; for non-contiguous region selection
;; (use-package zones)

;; automatic tangling and detangling for org babel
(use-package org-tanglesync)

;; make using julia's language server easier with eglot.
;; (use-package eglot-jl)

(use-package command-log-mode)

(use-package consult-web
  :ensure ( :host github :repo "armindarvish/consult-web"))

(use-package engrave-faces
  :ensure ( :host github :repo "tecosaur/engrave-faces"))

;; imenu side buffer
(use-package imenu-list)
(use-package side-hustle)

(use-package envrc)

;; hmmm, could it be useful?
(use-package citar)

;; adds stuff from node_modules to path
(use-package add-node-modules-path
  :config
  (eval-after-load 'js-mode
    '(add-hook 'js-mode-hook #'add-node-modules-path)))

;; auto treesitter grammar and mode setup
(use-package treesit-auto
  :custom
  (treesit-auto-install t)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  ;; doesnt help things with auctex?
  ;; (add-to-list
  ;;  'treesit-auto-recipe-list
  ;;  (make-treesit-auto-recipe
  ;;   :lang 'latex
  ;;   :ts-mode 'latex-ts-mode
  ;;   :remap 'LaTeX-mode
  ;;   :url "https://github.com/latex-lsp/tree-sitter-latex"
  ;;   :ext "\\.tex\\'"))
  (global-treesit-auto-mode))

;; library for working with time stamps easily
(use-package ts)

;; from github readme: isend-mode is an Emacs extension allowing interaction with code interpreters in ansi-term/term or vterm buffers. Some language-specific modes (e.g. python.el) already provide similar features; isend-mode does the same in a language-agnostic way.
(use-package isend-mode)

;; for popups
(use-package popper)

;; for nushell scripts
;; (use-package nushell-mode)

(use-package el-easydraw
  :ensure ( :host github :repo "misohena/el-easydraw" :main nil)
  :after (org)
  :config
  (require 'edraw-org)
  (edraw-org-setup-default)
  ;; When using the org-export-in-background option (when using the
  ;; asynchronous export function), the following settings are
  ;; required. This is because Emacs started in a separate process does
  ;; not load org.el but only ox.el.
  ;; (with-eval-after-load "ox"
  ;;   (require 'edraw-org)
  ;;   (edraw-org-setup-exporter))
  )

;; (use-package fancy-dabbrev
;;   :config
;;   (global-fancy-dabbrev-mode)
;;   (setq dabbrev-case-distinction nil)
;;   (setq dabbrev-case-fold-search t)
;;   (setq dabbrev-case-replace nil))

;; rg.el
(use-package rg)

;; (use-package phscroll
;;   :ensure ( :host github :repo "misohena/phscroll")
;;   :config
;;   (setq org-startup-truncated nil)
;;   (with-eval-after-load 'org
;;     (require 'org-phscroll)))

;; (use-package ligature)

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)))
(use-package meow)
;;   :config
;;   (meow-setup)
;;   (meow-global-mode 1))

(use-package ob-mongo
  :config
  (setq ob-mongo:default-mongo-executable "mongosh"))
(use-package inf-mongo
  :config
  (setq inf-mongo-command "mongosh"))

;; (use-package macrursors
;;   :config
;;   (dolist (mode '(corfu-mode goggles-mode beacon-mode))
;;     (add-hook 'macrursors-pre-finish-hook mode)
;;     (add-hook 'macrursors-post-finish-hook mode))
;;   (define-prefix-command 'macrursors-mark-map)
;;   (global-set-key (kbd "C-c SPC") #'macrursors-select)
;;   (global-set-key (kbd "C->") #'macrursors-mark-next-instance-of)
;;   (global-set-key (kbd "C-<") #'macrursors-mark-previous-instance-of)
;;   (global-set-key (kbd "C-;") 'macrursors-mark-map)
;;   (define-key macrursors-mark-map (kbd "C-;") #'macrursors-mark-all-lines-or-instances)
;;   (define-key macrursors-mark-map (kbd ";") #'macrursors-mark-all-lines-or-instances)
;;   (define-key macrursors-mark-map (kbd "l") #'macrursors-mark-all-lists)
;;   (define-key macrursors-mark-map (kbd "s") #'macrursors-mark-all-symbols)
;;   (define-key macrursors-mark-map (kbd "e") #'macrursors-mark-all-sexps)
;;   (define-key macrursors-mark-map (kbd "f") #'macrursors-mark-all-defuns)
;;   (define-key macrursors-mark-map (kbd "n") #'macrursors-mark-all-numbers)
;;   (define-key macrursors-mark-map (kbd ".") #'macrursors-mark-all-sentences)
;;   (define-key macrursors-mark-map (kbd "r") #'macrursors-mark-all-lines))

(use-package org-web-tools)

(use-package git-timemachine)

(use-package org-timeblock
  :after 'org)

(unless (is-android-system)
  (use-package gptel
    :config
    (setq gptel-default-mode 'org-mode)))

;; convert other formats to org using pandoc
(use-package org-pandoc-import
  :elpaca (:host github
                 :repo "tecosaur/org-pandoc-import"
                 :files ("*.el" "filters" "preprocessors")))

;; (use-package elsa)

(use-package puni)

(use-package punpun-themes)

;; need this for hebrew to be readable and other unicode characters
(use-package unicode-fonts
  :config
  (unicode-fonts-setup))

;; filtering/sorting/history for ivy
(use-package ivy-prescient
  :config
  (ivy-prescient-mode)
  (setq prescient-save-file (from-brain "emacs_prescient"))
  (prescient-persist-mode 1)
  (setq prescient-history-length 10000))

;; this doesnt work
;; (use-package org-xournalpp
;;   :elpaca ( :fetcher gitlab :repo "vherrmann/org-xournalpp")
;;   :config
;;   ;; (add-hook 'org-mode-hook 'org-xournalpp-mode)
;;   )

(use-package org-krita
  :ensure ( :fetcher github :repo "lepisma/org-krita" :files ("*.el" "resources"))
  :config
  (add-hook 'org-mode-hook 'org-krita-mode))

(use-package envrc
  :hook (after-init . envrc-global-mode))

(use-package p-search
  :elpaca (:host github :repo "https://github.com/zkry/p-search.git"))

;; (use-package org-xopp
;;   :ensure nil)

;; (if (file-exists-p "/home/mahmooz/work/org-xopp/")
    (use-package org-xopp
      :after (org)
      :load-path "/home/mahmooz/work/org-xopp/"
      :config
      (org-xopp-setup))
  ;; (use-package org-xopp
  ;;   :after (org)
  ;;   :ensure ( :host github :repo "mahmoodsh36/org-xopp" :files (:defaults "*.sh"))
  ;;   :config
  ;;   (org-xopp-setup)))

(use-package ox-ipynb
  :after (org)
  :ensure ( :host github :repo "jkitchin/ox-ipynb"))

(use-package colorful-mode
  :ensure ( :host github :repo "DevelopmentCool2449/colorful-mode")
  :hook (prog-mode text-mode))

;; version issues with transient
;; (use-package casual
;;   :config
;;   (require 'casual-agenda)
;;   (keymap-set org-agenda-mode-map "C-o" #'casual-agenda-tmenu)
;;   (require 'casual-calc) ; optional if using autoloaded menu
;;   (keymap-set calc-mode-map "C-o" #'casual-calc-tmenu)
;;   (keymap-set calc-alg-map "C-o" #'casual-calc-tmenu))

;; (use-package evil-terminal-cursor-changer
;;   )

;; actual smooth scrolling?
(use-package ultra-scroll
  :ensure ( :host github :repo "jdtsmith/ultra-scroll")
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

;; does this work?
(use-package org-sliced-images
  :config (org-sliced-images-mode))

;; from https://github.com/karthink/.emacs.d/blob/ff61c62c955eb941b8111fa9356d5f80b8dc9cbc/init.el#L200
(condition-case-unless-debug nil
    (use-package gcmh
      :defer 2
      :ensure t
      ;; :hook (after-init . gcmh-mode)
      :config
      (defun gcmh-register-idle-gc ()
        "Register a timer to run `gcmh-idle-garbage-collect'.
Cancel the previous one if present."
        (unless (eq this-command 'self-insert-command)
          (let ((idle-t (if (eq gcmh-idle-delay 'auto)
                            (* gcmh-auto-idle-delay-factor gcmh-last-gc-time)
                          gcmh-idle-delay)))
            (if (timerp gcmh-idle-timer)
                (timer-set-time gcmh-idle-timer idle-t)
              (setf gcmh-idle-timer
                    (run-with-timer idle-t nil #'gcmh-idle-garbage-collect))))))
      (setq gcmh-idle-delay 'auto  ; default is 15s
            gcmh-high-cons-threshold (* 32 1024 1024)
            gcmh-verbose nil
            gc-cons-percentage 0.2)
      (gcmh-mode 1))
  (error (setq gc-cons-threshold (* 16 1024 1024)
               gc-cons-percentage 0.2)))

;; (use-package pdfgrep)

;; svg popups?
;; (use-package nova
;;   :ensure ( :host github :repo "thisisran/nova")
;;   :config
;;   (nova-vertico-mode 1)
;;   (nova-corfu-mode 1)
;;   (nova-corfu-popupinfo-mode 1)
;;   (nova-eldoc-mode 1))

;; dont resize windows on vertico popups
(use-package mini-ontop
  :ensure ( :host github :repo "hkjels/mini-ontop.el")
  :config (mini-ontop-mode 1))

;; indentation guide
(use-package indent-bars
  :ensure ( :host github :repo "jdtsmith/indent-bars")
  :custom
  (indent-bars-no-descend-lists t) ; no extra bars in continued func arg lists
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  ;; add other languages as needed
  (indent-bars-treesit-scope '((python function_definition class_definition for_statement
                                       if_statement with_statement while_statement)))
  ;; note: wrap may not be needed if no-descend-list is enough
  ;;(indent-bars-treesit-wrap '((python argument_list parameters ; for python, as an example
  ;;                             list list_comprehension
  ;;                             dictionary dictionary_comprehension
  ;;                             parenthesized_expression subscript)))
  :hook ((python-base-mode yaml-mode) . indent-bars-mode))

(unless (is-android-system)
  (use-package aidermacs
    :straight (:host github :repo "MatthewZMD/aidermacs" :files ("*.el"))
    :config
    (setq aidermacs-args
          '("--model"
            "ollama_chat/qwq:32b"
            "--no-auto-commits"
            "--no-git"))
    ;; (setq aidermacs-auto-accept-architect t)
    (setenv "OLLAMA_API_BASE" "http://mahmooz2:11434")
    (setq aidermacs-extra-args '("--thinking-tokens" "16k"))
    ;; set the `aidermacs-config-file` variable in your emacs config:
    ;; (setq aidermacs-config-file "/path/to/your/project/.aider.conf.yml")
    ;; *or*, include the `--config` or `-c` flag in `aidermacs-extra-args`:
    ;; (setq aidermacs-extra-args '("--config" "/path/to/your/project/.aider.conf.yml"))
    (global-set-key (kbd "C-c a") 'aidermacs-transient-menu)))

;; (use-package elysium
;;   :custom
;;   (elysium-window-size 0.33)
;;   (elysium-window-style 'vertical))

;; (use-package evedel
;;   :defer t
;;   :config
;;   (customize-set-variable 'evedel-empty-tag-query-matches-all nil)
;;   :bind (("C-c e r" . evedel-create-reference)
;;          ("C-c e d" . evedel-create-directive)
;;          ("C-c e s" . evedel-save-instructions)
;;          ("C-c e l" . evedel-load-instructions)
;;          ("C-c e p" . evedel-process-directives)
;;          ("C-c e m" . evedel-modify-directive)
;;          ("C-c e C" . evedel-modify-reference-commentary)
;;          ("C-c e k" . evedel-delete-instructions)
;;          ("C-c e c" . evedel-convert-instructions)
;;          ("C->"     . evedel-next-instruction)
;;          ("C-<"     . evedel-previous-instruction)
;;          ("C-."     . evedel-cycle-instructions-at-point)
;;          ("C-c e t" . evedel-add-tags)
;;          ("C-c e T" . evedel-remove-tags)
;;          ("C-c e D" . evedel-modify-directive-tag-query)
;;          ("C-c e P" . evedel-preview-directive-prompt)
;;          ("C-c e /" . evedel-directive-undo)
;;          ("C-c e ?" . (lambda ()
;;                         (interactive)
;;                         (evedel-directive-undo t)))))

;; havent tried this yet
;; (use-package ellama
;;   :ensure t
;;   :bind ("C-c e" . ellama-transient-main-menu)
;;   ;; send last message in chat buffer with C-c C-c
;;   :hook (org-ctrl-c-ctrl-c-final . ellama-chat-send-last-message)
;;   :init (setopt ellama-auto-scroll t)
;;   :config
;;   ;; show ellama context in header line in all buffers
;;   (ellama-context-header-line-global-mode +1))

;; havent tried this yet
;; (use-package minuet
;;     :bind
;;     (("M-y" . #'minuet-complete-with-minibuffer) ;; use minibuffer for completion
;;      ("M-i" . #'minuet-show-suggestion) ;; use overlay for completion
;;      ("C-c m" . #'minuet-configure-provider)
;;      :map minuet-active-mode-map
;;      ;; These keymaps activate only when a minuet suggestion is displayed in the current buffer
;;      ("M-p" . #'minuet-previous-suggestion) ;; invoke completion or cycle to next completion
;;      ("M-n" . #'minuet-next-suggestion) ;; invoke completion or cycle to previous completion
;;      ("M-A" . #'minuet-accept-suggestion) ;; accept whole completion
;;      ;; Accept the first line of completion, or N lines with a numeric-prefix:
;;      ;; e.g. C-u 2 M-a will accepts 2 lines of completion.
;;      ("M-a" . #'minuet-accept-suggestion-line)
;;      ("M-e" . #'minuet-dismiss-suggestion))

;;     :init
;;     ;; if you want to enable auto suggestion.
;;     ;; Note that you can manually invoke completions without enable minuet-auto-suggestion-mode
;;     (add-hook 'prog-mode-hook #'minuet-auto-suggestion-mode)

;;     :config
;;     ;; You can use M-x minuet-configure-provider to interactively configure provider and model
;;     (setq minuet-provider 'openai-fim-compatible)

;;     ;; Required when defining minuet-ative-mode-map in insert/normal states.
;;     ;; Not required when defining minuet-active-mode-map without evil state.
;;     (add-hook 'minuet-active-mode-hook #'evil-normalize-keymaps)

;;     (minuet-set-optional-options minuet-openai-fim-compatible-options :max_tokens 256))

(use-package mcp
  :elpaca (:host github :repo "lizqwerscott/mcp.el")
  :config
  (setq mcp-hub-servers
        `(
          ("filesystem" . (:command "mcp-server-filesystem" :args ,(file-truename "~/.mcp")))
          ;; ("everything" . (:command "mcp-server-everything"))
          ("fetch" . (:command "mcp-server-fetch" :args ("mcp-server-fetch")))
          ("mcp-tavily-search" . (:command "npx" :args ("-y" "mcp-tavily-search")))
          ;; ("mcp-wolfram-alpha" . (:command "uvx" :args ("mcp-wolfram-alpha")))
          ;; ("mcp-server-searxng" . (:command "uvx" :args ("mcp-searxng")
          ;;                                   :env (:SEARXNG_URL "https://searx.mahmoodsh.com/")))
          ("mcp-server-time" . (:command "mcp-server-time"
                                         :args ("--local-timezone" "Asia/Jerusalem")))
          ("mcp-server-sqlite" . (:command "mcp-server-sqlite" ("--db-path" ,(file-truename "~/.mcp/db"))))
          ("mcp-server-sequential-thinking" . (:command "mcp-server-sequential-thinking"))
          ;; ("mcp-server-playwright" . (:command "mcp-server-playwright"))
          ;; ("mcp-server-git" . (:command "mcp-server-git"))

          ;; ("qdrant" . (:url "http://localhost:8000/sse"))
          ;; ("graphlit" . (:command "npx"
          ;;                :args ("-y" "graphlit-mcp-server")
          ;;                :env (
          ;;                      :GRAPHLIT_ORGANIZATION_ID "your-organization-id"
          ;;                      :GRAPHLIT_ENVIRONMENT_ID "your-environment-id"
          ;;                      :GRAPHLIT_JWT_SECRET "your-jwt-secret")))
          ))
  ;; (add-hook 'after-init-hook #'mcp-hub-start-all-server)
  )

;; from https://github.com/lizqwerscott/mcp.el
(defun gptel-mcp-register-tool ()
  (interactive)
  (let ((tools (mcp-hub-get-all-tool :asyncp t :categoryp t)))
    (mapcar #'(lambda (tool)
                (apply #'gptel-make-tool
                       tool))
            tools)))
(defun gptel-mcp-use-tool ()
  (interactive)
  (let ((tools (mcp-hub-get-all-tool :asyncp t :categoryp t)))
    (mapcar #'(lambda (tool)
                (let ((path (list (plist-get tool :category)
                                  (plist-get tool :name))))
                  (push (gptel-get-tool path)
                        gptel-tools)))
            tools)))
(defun gptel-mcp-close-use-tool ()
  (interactive)
  (let ((tools (mcp-hub-get-all-tool :asyncp t :categoryp t)))
    (mapcar #'(lambda (tool)
                (let ((path (list (plist-get tool :category)
                                  (plist-get tool :name))))
                  (setq gptel-tools
                        (cl-remove-if #'(lambda (tool)
                                          (equal path
                                                 (list (gptel-tool-category tool)
                                                       (gptel-tool-name tool))))
                                      gptel-tools))))
            tools)))

(use-package lua-mode
  :init
  (autoload 'lua-mode "lua-mode" "Lua editing mode." t)
  (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
  :config
  (setf lua-indent-level 2)
  (when (executable-find "lua-language-server")
    (require 'eglot)
    (add-to-list 'eglot-server-programs
                 `(lua-mode . ("lua-language-server")))
    (add-hook 'lua-mode-hook 'eglot-ensure)))

(use-package macher
  :ensure (:host github :repo "kmontag/macher")
  :hook
  ;; add the current file to the gptel context when making macher requests.
  (macher-before-send
   .
   (lambda ()
     (when-let* ((filename (buffer-file-name))
                 ((not (file-directory-p filename))))
       (gptel-add-file filename))))
  :config
  ;; adjust buffer positioning to taste.
  (add-to-list
   'display-buffer-alist
   '("\\*macher:.*\\*"
     (display-buffer-in-side-window)
     (side . bottom)))
  (add-to-list
   'display-buffer-alist
   '("\\*macher-patch:.*\\*"
     (display-buffer-in-side-window)
     (side . right)))
  ;; customize patch display action. The 'macher-context' struct
  ;; contains data from the current request, including the contents of
  ;; any files that were edited.
  (setopt macher-patch-ready-function
          (lambda (macher-context)
            (ediff-patch-file nil (current-buffer))))
  )

(provide 'config-packages)