;; fails to compile on android, also default recipe fails when used with elpaca
(when (not (is-android-system))
  (use-package auctex
    :ensure '(auctex
              ;; upstream dropped autotools, GNUmakefile's elpa target generates everything now
              :pre-build (("make" "elpa"))
              :files ("*.el" "doc/*.info*" "etc" "images" "latex" "style"))
    :hook
    (LaTeX-mode . turn-on-prettify-symbols-mode)
    (LaTeX-mode . reftex-mode)
    (LaTeX-mode . outline-minor-mode)
    ;; (LaTeX-mode . olivetti-mode)
    :config
    (add-hook 'plain-TeX-mode-hook 'LaTeX-mode) ;; why is this not the default?
    ;; (add-to-list 'auto-mode-alist '("\\.tex$" . LaTeX-mode))
  ))

;; makes binding keys less painful
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

;; typescript setup
(use-package typescript-mode)

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
  :ensure (corfu :ref "856171ac98c3aaa629caa011be7cd3a9405e6e0f") ;; pin to before "Require Compat 31"
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
  :ensure (cape :ref "2e15e1909754752f66096dde1b8d639d6eb25f35") ;; pin to before "Require Compat 31"
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

;; highlight surrounding parentheses
(use-package highlight-parentheses
  :config
  (add-hook 'prog-mode-hook #'highlight-parentheses-mode))

;; modern API for working with files/dirs
(use-package f)

;; small flash when evaluating a sexp
;; (use-package eval-sexp-fu)

;; provides syntax highlighting when exporting from org mode to html
(use-package htmlize)

;; check which keys i press most
(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  (setq keyfreq-file (from-brain "emacs_keyfreq")))

(use-package avy)

;; evaulation overlay for elisp
(use-package eros
  :config
  (eros-mode 1))

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

(use-package vimrc-mode)

;; whether to use slime/sly
(if *use-sly*
    (use-package sly
      :ensure (:host github :repo "joaotavora/sly")
      :config
      (setq inferior-lisp-program "")
      (setq sly-lisp-implementations
            `((sbcl ("sbcl"
                     "--dynamic-space-size" "12GB"
                     "--load"
                     ,(join-path
                       (my-getenv "WORK_DIR")
                       "/cl-tools/myloader.lisp")))
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
  (use-package slime
    :config
    (setq inferior-lisp-program "")
    (setq slime-lisp-implementations
           `((sbcl ("sbcl"
                    "--dynamic-space-size" "12GB"
                    "--load"
                    ,(join-path
                      (my-getenv "WORK_DIR")
                      "/cl-tools/myloader.lisp")))
            (clisp ("clisp"))
            (ecl ("ecl"))
            (cmucl ("cmucl"))
            (ccl ("ccl"))
            (maxima ("rmaxima" "-r" "to_lisp();"))))
    ;; i think this increases history file size
    (setq comint-input-ring-size 1000000)))
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

;; depth-dependent coloring of code
(use-package prism
  :ensure (prism :fetcher github :repo "alphapapa/prism.el"))

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
  :ensure (vertico :ref "e4338c5bae2c725be2940726be170bc034af3b6c") ;; pin to before "Require Compat 31"
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
  :ensure (orderless :ref "3a2a32181f7a5bd7b633e40d89de771a5dd88cc7") ;; pin to before "Require Compat 31"
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

(use-package consult
  :ensure (consult :ref "45fdad7b234141ea572267024c8f4b08dd2e1022")) ;; pin to before "Require Compat 31"

(use-package marginalia
  :ensure (marginalia :ref "4a0628dfdf944a5d307d31d2a514825cc5386986") ;; pin to before "Require Compat 31"
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

(use-package yasnippet-capf
  :after cape
  :ensure ( :host github :repo "elken/yasnippet-capf")
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

(use-package async-completing-read
  :ensure ( :host github :repo "oantolin/async-completing-read"))

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

(use-package engrave-faces
  :ensure ( :host github :repo "tecosaur/engrave-faces"))

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
  ;; global-treesit-auto-mode rebuilds the full remap alist on every find-file (97% of open time)
  ;; treesit-auto-add-to-auto-mode-alist already handles mode remapping at startup
  ;; (global-treesit-auto-mode)
  )

(use-package ob-mongo
  :config
  (setq ob-mongo:default-mongo-executable "mongosh"))
;; (use-package inf-mongo
;;   :config
;;   (setq inf-mongo-command "mongosh"))

(use-package org-web-tools)

(use-package git-timemachine)

(defun get-env-var-from-script (var)
  "return the value of environment variable VAR defined in ~/brain/moredots/env.sh."
  (let ((script (from-brain "moredots/env.sh")))
    (string-trim
     (shell-command-to-string
      (format "bash -c 'source %s >/dev/null 2>&1; echo -n \"$%s\"'"
              script
              var)))))

;; convert other formats to org using pandoc
(use-package org-pandoc-import
  :elpaca (:host github
                 :repo "tecosaur/org-pandoc-import"
                 :files ("*.el" "filters" "preprocessors")))

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

(use-package envrc
  :hook (after-init . envrc-global-mode))

(if (file-exists-p "/home/mahmooz/work/org-xopp/")
    (use-package org-xopp
      :after (org)
      :load-path "/home/mahmooz/work/org-xopp/"
      :config
      (org-xopp-setup))
    (use-package org-xopp
      :after (org)
      :ensure ( :host github :repo "mahmoodsh36/org-xopp" :files (:defaults "*.sh"))
      :config
      (org-xopp-setup)))

;; (use-package colorful-mode
;;   :ensure ( :host github :repo "DevelopmentCool2449/colorful-mode")
;;   :hook (prog-mode text-mode))

;; actual smooth scrolling?
(use-package ultra-scroll
  :ensure ( :host github :repo "jdtsmith/ultra-scroll")
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

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

;; (use-package git-gutter
;;   :ensure t)
(use-package git-gutter-fringe
  :ensure t
  :config
  (global-git-gutter-mode))

(provide 'config-packages)
