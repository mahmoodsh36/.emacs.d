;; setup straight.el package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq package-enable-at-startup nil
      straight-use-package-by-default t
      native-comp-async-report-warnings-errors nil)

;; disable customization using the interactive interface
(setq custom-file "/dev/null")
(setq inhibit-startup-screen t)
(straight-use-package 'use-package)

;; set tab size to 2 spaces except 4 for python
(setq-default tab-width 2)
(setq-default js-indent-level 2)
(setq-default c-basic-offset 2)
(setq-default indent-tabs-mode nil)
(setq evil-shift-width 2)
(setq-default python-indent-offset 4)
;; overwrite highlighted text
(delete-selection-mode 1)
;; show matching parenthases
(show-paren-mode 1)
;; disable upper bars and scrollbar
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
;; always follow symlinks
(setq vc-follow-symlinks t)
;; y-or-n instead of yes-or-no
(defalias 'yes-or-no-p 'y-or-n-p)
;; all backups to one folder
(setq backup-directory-alist
      `((".*" . ,"~/.emacs.d/backup/")))
(setq auto-save-file-name-transforms
      `((".*" ,"~/.emacs.d/backup/" t)))
;; disable cursor blink
(blink-cursor-mode 0)
;; treat underscore as part of word
(defun underscore-part-of-word-hook ()
  (modify-syntax-entry ?_ "w"))
;;(add-hook 'text-mode-hook 'underscore-part-of-word-hook)
;; highlight current line
(global-hl-line-mode)
;; reload file automatically
(global-auto-revert-mode t)
;; enable all disabled commands
(setq disabled-command-function nil)
;; initial frame size
;;(when window-system (set-frame-size (selected-frame) 115 58))
;; no damn fringes dude!
(set-fringe-style 0)
;; display only buffer name in modeline
(setq-default mode-line-format (list " " mode-line-modified "%e %b"))
;; restore default status line for pdf mode
(add-hook 'pdf-view-mode-hook
          (lambda ()
            (interactive)
            (setq-local mode-line-format (eval (car (get 'mode-line-format 'standard-value))))))
;; kill buffer without confirmation when its tied to a process
(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))
;; make tab actually insert tab..
(global-set-key "\t" 'tab-to-tab-stop)
;; save open buffers on exit
(desktop-save-mode 1)
;; save minibuffer history
(savehist-mode 1)
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(setq savehist-file "~/data/emacs_savehist")

;; smooth scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don"t accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

(use-package org)

;; needed for evil mode
(use-package undo-fu)

;; evil-mode
(setq evil-want-keybinding nil)
(use-package evil
  :config
  (evil-mode 1)
  (evil-set-initial-state 'image-dired-thumbnail-mode 'emacs)
  ;; undo/redo keys
  (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
  (define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo)
  ;; make ESC cancel all
  (define-key key-translation-map (kbd "ESC") (kbd "C-g")))
  ;;dont copy the overwritten text when overwriting text by pasting
  ;;(setq-default evil-kill-on-visual-paste nil))

;; evil-surround for evil mode
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;; exchange portions of text more easily with evil
(use-package evil-exchange
  :config
  (evil-exchange-install))

;; search for the current visual selection with */#
(use-package evil-visualstar
  :config
  (global-evil-visualstar-mode))

;; support to make evil more compatible with the whole of emacs
(use-package evil-collection
  :after (evil)
  :config
  (evil-collection-init))

;; display marks visually
(use-package evil-visual-mark-mode
  :config
  (add-hook 'evil-mode-hook 'evil-visual-mark-mode))

;; display visual hints for evil actions
(use-package evil-goggles
  :config
  (evil-goggles-mode))

;; make line a text object - yil dil cil, etc..
(use-package evil-textobj-line)

;; quick commenting
(use-package evil-nerd-commenter
  :config
  (global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines))

;; makes binding keys less painful, is used later on in the config
(use-package general
  :config
  (general-evil-setup))

(use-package counsel
  :config
  (ivy-mode)
  (setq ivy-height 25)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file))

;; projectile
(use-package projectile
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-mode +1))

;; auto completion
(use-package company
  :config
  (add-hook 'prog-mode-hook 'company-mode)
  (global-set-key (kbd "M-/") 'company-complete-common-or-cycle)
  (setq company-idle-delay 0
        company-require-match nil
        company-tooltip-limit 30
        company-tooltip-align-annotations t)
  (eval-after-load 'company
    '(progn
      (define-key company-active-map (kbd "TAB") 'company-complete-selection)
      (define-key company-active-map [tab] 'company-complete-selection)
      (unbind-key "RET" company-active-map)
      (unbind-key "<return>" company-active-map))))

;; popup documentation for quick help for company
(use-package company-quickhelp
  :config
  (company-quickhelp-mode))

;; company completion with icons
(use-package company-box
  :hook (company-mode . company-box-mode))

;; anaconda for python
(use-package company-anaconda
  :config
  (eval-after-load "company"
    '(add-to-list 'company-backends 'company-anaconda))
  (add-hook 'python-mode-hook 'anaconda-mode))

;; company for web mode
(use-package company-web)

;; company for shell scripting
(use-package company-shell
  :config
  (add-to-list 'company-backends '(company-shell company-shell-env)))

;; colorful delimiters
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; evil mode support for org
(use-package evil-org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  ;; the package annoyingly binds O to another function so here im just restoring it
  (general-define-key :states 'normal :keymaps 'override "O"
                      (lambda ()
                        (interactive)
                        (evil-open-above 1)))
  (general-define-key :states '(normal visual) :keymaps 'override "0" 'evil-beginning-of-line)
  (general-define-key :states '(normal visual) :keymaps 'override "$" 'evil-end-of-line)
  (general-define-key :states '(normal visual) :keymaps 'override "^" 'evil-first-non-blank))

;; (use-package poet-theme
;;   :config
;;   (set-face-attribute 'default nil :family "Inconsolata" :height 130)
;;   (set-face-attribute 'fixed-pitch nil :family "Inconsolata")
;;   (set-face-attribute 'variable-pitch nil :family "Noto Sans Mono")
;;   (load-theme 'poet t)
;;   (add-hook 'text-mode-hook
;;             (lambda ()
;;               (variable-pitch-mode 1))))

(use-package web-mode
  :config
  (setq web-mode-enable-auto-quoting nil)
  (setq web-mode-enable-auto-closing nil)
  (setq web-mode-enable-auto-expanding nil)
  (setq web-mode-enable-auto-pairing nil)
  (setq web-mode-enable-auto-indentation nil)
  (setq web-mode-enable-auto-opening nil)
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))
  (setq web-mode-content-types-alist
        '(("jsx" . "\\.js[x]?\\'")))
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(use-package lua-mode
  :config
  (setq lua-indent-level 2))

;; package to help making http requests
(use-package request)

;; highlights color names with the corresponding color
(use-package rainbow-mode
  :config
  (add-hook 'prog-mode-hook 'rainbow-mode))

;; helps figure out which key runs which function
(use-package command-log-mode
  :config
  (global-command-log-mode))

;; save undos/redos even when buffer is killed or emacs restarts
(use-package undo-fu-session
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (global-undo-fu-session-mode))

;; executing sage in org babel
(use-package ob-sagemath
  :config
  ;; Ob-sagemath supports only evaluating with a session.
  (setq org-babel-default-header-args:sage '((:session . t)
                                             (:results . "drawer")))
  (setq sage-shell:input-history-cache-file "~/data/sage_history")
  (add-hook 'sage-shell-after-prompt-hook #'sage-shell-view-mode))

;; better built-in help/documentation
(use-package helpful
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h a") #'helpful-symbol)
  (global-set-key (kbd "C-h k") #'helpful-key))

;; yasnippet
(use-package yasnippet-snippets)
(use-package yasnippet
  :config
  ;;(setq yas-snippet-dirs
  ;;      `(,(concat user-emacs-directory "snippets")))
  (yas-global-mode 1)
  ;; prevent warnings about snippets using elisp
  (require 'warnings)
  (add-to-list 'warning-suppress-types '(yasnippet backquote-change)))

;; highlight errors in code
(use-package flycheck
  :config
  (global-flycheck-mode))

;; edit multiple instances of a word simulataneously
(use-package iedit)

;; highlight surrounding parentheses
(use-package highlight-parentheses
  :config
  (add-hook 'prog-mode-hook #'highlight-parentheses-mode))

(use-package literate-calc-mode)

;; generating linear ranges quickly
(use-package tiny
  :config
  (global-set-key (kbd "C-c t") 'tiny-expand))

;; icons for dired
(use-package all-the-icons)
(use-package all-the-icons-dired
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;; modern API for working with files/dirs
(use-package f)

;; language server protocol support
(use-package lsp-mode
  :config
  (add-hook 'prog-mode-hook 'lsp-mode))

;; show simple info on the right
(use-package lsp-ui
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;; lsp support for treemacs
(use-package lsp-treemacs
  :config
  (lsp-treemacs-sync-mode 1)
  (treemacs-resize-icons 15)
  (setq treemacs-width 30))

;; for evil mode compatibility
(use-package treemacs-evil
  :config
  (general-define-key :states '(normal motion emacs treemacs) :keymaps 'override "SPC t" 'treemacs))

;; indentation-based text objects for evil
(use-package evil-indent-plus
  :config
  (define-key evil-inner-text-objects-map "i" 'evil-indent-plus-i-indent)
  (define-key evil-outer-text-objects-map "i" 'evil-indent-plus-a-indent)
  (define-key evil-inner-text-objects-map "I" 'evil-indent-plus-i-indent-up-down)
  (define-key evil-outer-text-objects-map "I" 'evil-indent-plus-a-indent-up-down))

;; ensure the PATH variable is set according to the users shell, solves some issues on macos
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; display available keybindings
(use-package which-key
  :config
  (which-key-mode 1))

;; small flash when evaluating a sexp
(use-package eval-sexp-fu)

;; flutter setup
(use-package highlight-indent-guides
  :config
  (setq highlight-indent-guides-method 'character)
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))
(use-package dart-mode)
(use-package flutter)
(use-package lsp-dart)

;; best pdf viewer
(use-package pdf-tools
  :config
  (pdf-tools-install t)
  (add-hook 'pdf-view-mode-hook 'pdf-view-themed-minor-mode))

;; latex company backend
(use-package company-auctex
  :config
  (company-auctex-init))

;; csharp setup
(use-package csharp-mode)

;; history for ivy completion
(use-package ivy-prescient
  :config
  (ivy-prescient-mode)
  (prescient-persist-mode 1)
  (setq prescient-save-file (expand-file-name "~/data/emacs_prescient"))) ;; save history to filesystem
(use-package company-prescient
  :config
  (company-prescient-mode))

;; auto indentation
;; (use-package aggressive-indent
;;   :config
;;   (aggressive-indent-global-mode))

;; auto pairs insertion
;; (use-package smartparens
;;   :config
;;   ;; dont insert pair when cursor is before text
;;   (sp-pair "(" nil :unless '(sp-point-before-word-p))
;;   (sp-pair "[" nil :unless '(sp-point-before-word-p))
;;   (sp-pair "{" nil :unless '(sp-point-before-word-p))
;;   (sp-pair "\"" nil :unless '(sp-point-before-word-p))
;;   (sp-local-pair '(latex-mode org-mode) "$" "$" :unless '(sp-point-before-word-p))
;;   (smartparens-global-mode))

;; multiple cursors for evil mode
(use-package evil-mc
  :config
  (global-evil-mc-mode))

;; provides syntax highlighting when exporting from org mode to html
(use-package htmlize)

;; static website generation for org mode
(use-package ox-hugo
  :config
  (setq org-hugo-base-dir "/home/mahmooz/workspace/blog/")
  (setq org-hugo-section "math")
  (setq org-more-dir (expand-file-name "~/workspace/blog/static/more/"))
  (ignore-errors (make-directory org-more-dir)))
;;(add-hook 'org-mode-hook 'org-hugo-auto-export-mode))

;; best latex preview functionality
(use-package xenops
  :config
  (setq xenops-reveal-on-entry t)
  (setq xenops-math-latex-max-tasks-in-flight 6)
  (add-hook 'LaTeX-mode-hook #'xenops-mode)
  (add-hook 'org-mode-hook #'xenops-mode)
  (add-hook 'xenops-mode-hook 'xenops-render)
  (add-hook 'org-babel-after-execute-hook (lambda ()
                                            (interactive)
                                            (ignore-errors (xenops-render))))
  (setq xenops-math-image-scale-factor 1.5)
  (setcar (cdr (car xenops-elements))
          '(:delimiters
            ("^[ 	]*\\\\begin{\\(align\\|equation\\|gather\\)\\*?}" "^[ 	]*\\\\end{\\(align\\|equation\\|gather\\)\\*?}")
            ("^[ 	]*\\\\\\[" "^[ 	]*\\\\\\]"))))

(use-package mixed-pitch
  :hook
  (text-mode . mixed-pitch-mode))

;; show hidden elements when cursor is over them like links/markers etc
(use-package org-appear
  :config
  (setq org-appear-autoemphasis t
        org-appear-autoentities t
        org-appear-autokeywords t
        org-appear-autolinks t
        org-appear-autosubmarkers t)
  (add-hook 'org-mode-hook 'org-appear-mode))

;; jump to matching tags
(use-package evil-matchit
  :config
  (global-evil-matchit-mode 1))

;; the key to building a second brain in org mode
(use-package org-roam
  :custom
  (org-roam-directory (file-truename "~/workspace/college/"))
  :config
  (general-define-key :states 'normal :keymaps 'override "SPC r t" 'org-roam-buffer-toggle)
  (general-define-key :states 'normal :keymaps 'override "SPC r f" 'org-roam-node-find)
  (general-define-key :states 'normal :keymaps 'override "SPC r g" 'org-roam-graph)
  (general-define-key :states 'normal :keymaps 'override "SPC r i" 'org-roam-node-insert)
  (general-define-key :states 'normal :keymaps 'override "SPC r c" 'org-roam-capture)
  (general-define-key :states 'normal :keymaps 'override "SPC r d" 'org-roam-dailies-capture-today)
  (general-define-key :states 'normal :keymaps 'override "SPC r c" 'org-id-get-create)
  (general-define-key :states 'normal :keymaps 'override "SPC r o" 'org-open-at-point)
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  ;;(setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

;; add edition/creation timestamps to headers and files
(use-package org-roam-timestamps
  :config
  (org-roam-timestamps-mode)
  (setq org-roam-timestamps-remember-timestamps t))

;; text evil objects for latex
(use-package evil-tex
  :config
  (add-hook 'LaTeX-mode-hook #'evil-tex-mode)
  (add-hook 'org-mode-hook #'evil-tex-mode))

;; give org mode a better look
(use-package org-modern
  :config
  (global-org-modern-mode))

(use-package org-roam-ui)
;;(use-package org-transclusion)
;;(use-package roam-block)

(use-package magit)

(use-package slime
  :config
  (setq inferior-lisp-program "sbcl"))

(use-package aio)

;; text aligning
(use-package evil-lion)

;; preview registers and marks before actually using them
(use-package evil-owl
  :config
  (evil-owl-mode))

(use-package org-super-agenda)
(use-package org-web-tools)
(use-package evil-textobj-tree-sitter)
(use-package evil-embrace)

;; start server
(server-start)

;; c-x c-l to complete line like vim
(defun my-expand-lines ()
  (interactive)
  (let ((hippie-expand-try-functions-list
         '(try-expand-line)))
    (call-interactively 'hippie-expand)))
(define-key evil-insert-state-map (kbd "C-x C-l") 'my-expand-lines)

;; more text objects
(defmacro define-and-bind-text-object (key start-regex end-regex)
  (let ((inner-name (make-symbol "inner-name"))
        (outer-name (make-symbol "outer-name")))
    `(progn
       (evil-define-text-object ,inner-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count nil))
       (evil-define-text-object ,outer-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count t))
       (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
       (define-key evil-outer-text-objects-map ,key (quote ,outer-name)))))
(define-and-bind-text-object "$" "\\$" "\\$")
(define-and-bind-text-object "|" "|" "|")
(define-and-bind-text-object "/" "/" "/")
(define-and-bind-text-object "*" "*" "*")

;; org mode config
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(setq org-log-done 'time)
;; Show images when opening a file.
(setq org-startup-with-inline-images t)
;; Show images after evaluating code blocks.
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
;; render latex preview after evaluating code blocks
;; (add-hook 'org-babel-after-execute-hook 'org-latex-preview)
;; disable prompt when executing code block in org mode
(setq org-confirm-babel-evaluate nil)
;; enable more code block languages for org mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (js . t)
   (lisp . t)
   (java . t)
   (latex . t)
   (C . t)
   (shell . t)
   (lua . t)))
;; require org-tempo to enable <s expansion
(require 'org-tempo)
;; make org babel default to python3
(setq org-babel-python-command "python3")
;; increase org table max lines
(setq org-table-convert-region-max-lines 10000)
;; to increase depth of the imenu in treemacs
(setq org-imenu-depth 4)
;; who cares about annoying broken links errors..
(setq org-export-with-broken-links t)

(defun run-command-show-output (cmd)
  "run shell command and show continuous output in new buffer"
  (interactive)
  (progn
    (start-process-shell-command cmd cmd cmd)
    (display-buffer cmd)
    (end-of-buffer-other-window nil)))

;; hide unnecessary stuff
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
(setq dired-listing-switches "-l")
(setq dired-dwim-target t) ;; moving files in a smart way when window is split into 2
(add-hook 'dired-mode-hook 'auto-revert-mode) ;; hook to make dired auto refresh files when they get edited/changed/created/whatever

;; function to get size of files in dired
(defun dired-get-size ()
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "du" nil t nil "-sch" files)
      (message "Size of all marked files: %s"
               (progn 
                 (re-search-backward "\\(^[0-9.,]+[A-Za-z]+\\).*total$")
                 (match-string 1))))))
(define-key dired-mode-map (kbd "?") 'dired-get-size)

;; vim like keys for dired image viewer
(setq image-dired-show-all-from-dir-max-files 100000000)
(defun image-dired-up ()
  (interactive)
  (previous-line)
  (setq current-char (- (point) (point-at-bol)))
  (if (eq (% current-char 2) 1)
      (left-char))
  (image-dired-display-thumbnail-original-image))
(defun image-dired-down ()
  (interactive)
  (next-line)
  (setq current-char (- (point) (point-at-bol)))
  (if (eq (% current-char 2) 1)
      (left-char))
  (image-dired-display-thumbnail-original-image))
(defun image-dired-bol ()
  (interactive)
  (beginning-of-line)
  (image-dired-display-thumbnail-original-image))
(defun image-dired-eol ()
  (interactive)
  (end-of-line)
  (left-char)
  (image-dired-display-thumbnail-original-image))
(defun define-dired-thumbnail-mode-keys ()
  (define-key image-dired-thumbnail-mode-map (kbd "l") 'image-dired-display-next-thumbnail-original)
  (define-key image-dired-thumbnail-mode-map (kbd "h") 'image-dired-display-previous-thumbnail-original)
  (define-key image-dired-thumbnail-mode-map (kbd "k") 'image-dired-up)
  (define-key image-dired-thumbnail-mode-map (kbd "j") 'image-dired-down)
  (define-key image-dired-thumbnail-mode-map (kbd "0") 'image-dired-bol)
  (define-key image-dired-thumbnail-mode-map (kbd "$") 'image-dired-eol))
(add-hook 'image-dired-thumbnail-mode-hook 'define-dired-thumbnail-mode-keys)

(defun current-filename ()
  (file-name-sans-extension
   (file-name-nondirectory (buffer-file-name))))

(defun get-latex-cache-dir-path ()
  "return the path for the directory that contains the compiled pdf latex documents"
  (interactive)
  (setq dir-path (concat (expand-file-name user-emacs-directory) "latex/"))
  (ignore-errors (make-directory dir-path))
  dir-path)

(defun compile-latex-file (path)
  (start-process-shell-command "latex" "latex" (format "pdflatex -shell-escape -output-directory=%s %s" (get-latex-cache-dir-path) path)))
  ;; the minted library annoyingly creates directories named _minted-something, get rid of those
  ;;(run-at-time "4 sec" nil #'call-process-shell-command "rmdir _minted-*"))

(defun compile-current-document ()
  "compile the current latex document being edited"
  (interactive)
  (compile-latex-file (buffer-file-name)))

(defun open-current-document ()
  "open the pdf of the current latex document that was generated"
  (interactive)
  (find-file-other-window (concat (get-latex-cache-dir-path) (concat (current-filename) ".pdf"))))
(defun open-current-document-this-window ()
  (interactive)
  (find-file (concat (get-latex-cache-dir-path) (concat (current-filename) ".pdf"))))

(evil-define-key 'normal 'TeX-mode-map (kbd "SPC v") 'open-current-document)
(evil-define-key 'normal 'TeX-mode-map (kbd "SPC V") 'open-current-document-this-window)

;; tex hook to auto compile on save
;; (add-hook
;;  'TeX-mode-hook
;;  (lambda ()
;;    (compile-current-document)
;;    (add-hook 'after-save-hook 'compile-current-document 0 t)))

;; the next 2 functions need to be rewritten
(defun compile-sagetex-command ()
  "return the command needed to compile sagetex"
  (interactive)
  (setq first-pdflatex-command (concat "(" (concat (concat (concat "pdflatex --synctex=1 -output-directory=" (concat (get-latex-cache-dir-path) " ")) (buffer-file-name)) ";")))
  (setq last-pdflatex-command (concat (concat (concat "pdflatex --synctex=1 -output-directory=" (concat (get-latex-cache-dir-path) " ")) (buffer-file-name)) ")"))
  (concat first-pdflatex-command (concat (concat "(cd " (concat (get-latex-cache-dir-path) (concat "; sage " (concat (current-filename) ".sagetex.sage);")))) last-pdflatex-command)))
(defun compile-sagetex ()
  "compile the current latex document with support for sagetex"
  (interactive)
  (start-process-shell-command "latex" "latex" (compile-sagetex-command)))

;; dmenu like functions
(defun search-open-file (directory-path regex)
  "search for a file recursively in a directory and open it"
  "search for file and open it similar to dmenu"
  (interactive)
  (let ((my-file (ivy-completing-read "select file: " (directory-files-recursively directory-path regex))))
    (browse-url (expand-file-name my-file))))

(defun search-open-file-in-emacs (directory-path regex)
  "search for a file recursively in a directory and open it in emacs"
  (let ((my-file (ivy-completing-read "select file: " (directory-files-recursively directory-path regex))))
    (find-file (expand-file-name my-file) "'")))

;; keys to search for files
(define-key evil-normal-state-map (kbd "SPC f c")
            (lambda () (interactive) (search-open-file "~/workspace/college" ".*\\(pdf\\|tex\\|doc\\|mp4\\|png\\)")))
(define-key evil-normal-state-map (kbd "SPC F c")
            (lambda () (interactive)
              (search-open-file-in-emacs "~/workspace/college" ".*\\(pdf\\|tex\\|doc\\|org\\)")))
(define-key evil-normal-state-map (kbd "SPC f p")
            (lambda () (interactive) (search-open-file "~/data/p" "")))
(define-key evil-normal-state-map (kbd "SPC f b")
            (lambda () (interactive) (search-open-file "~/data/books" "")))
(define-key evil-normal-state-map (kbd "SPC f d")
            (lambda () (interactive) (search-open-file "~/data" "")))
(define-key evil-normal-state-map (kbd "SPC F d")
            (lambda () (interactive)
              (search-open-file-in-emacs "~/data" "")))

;; keybindings
(global-set-key (kbd "C-M-S-x") 'eval-region)
(global-set-key (kbd "C-x D") 'image-dired)
(general-define-key :states '(normal motion emacs) :keymaps 'override "SPC d w" (lambda () (interactive) (dired "~/dl/")))
(general-define-key :states '(normal motion emacs) :keymaps 'override "SPC d a" (lambda () (interactive) (dired "~/data/")))
(general-define-key :states '(normal motion emacs) :keymaps 'override "SPC d c" (lambda () (interactive) (dired "~/workspace/college/")))
(general-define-key :states '(normal motion emacs) :keymaps 'override "SPC d l" (lambda () (interactive) (dired (get-latex-cache-dir-path))))
(general-define-key :states '(normal motion emacs) :keymaps 'override "SPC d r" (lambda () (interactive) (dired "~/data/resources/")))
(general-define-key :states '(normal motion emacs) :keymaps 'override "SPC d b" (lambda () (interactive) (dired "~/workspace/blog/")))
(general-define-key :states '(normal motion emacs) :keymaps 'override "SPC d h" (lambda () (interactive) (dired "~/")))
(general-define-key :states '(normal motion emacs) :keymaps 'override "SPC d d" 'dired)
(general-define-key :states '(normal motion emacs) :keymaps 'override "SPC f f" 'counsel-find-file)
(general-define-key :states '(normal motion emacs) :keymaps 'override "SPC SPC" 'counsel-M-x)
(general-define-key :states '(normal motion emacs) :keymaps 'override "SPC b k" 'kill-this-buffer)
(general-define-key :states '(normal motion emacs) :keymaps 'override "SPC b K" 'kill-buffer-and-window)
(general-define-key :states '(normal motion emacs) :keymaps 'override "SPC b s" 'counsel-switch-buffer)
(general-define-key :states '(normal motion emacs) :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map) "SPC x" 'eval-defun)
(general-define-key :states '(normal motion emacs) :keymaps 'override "SPC g" 'counsel-ag)
(general-define-key :states '(normal motion emacs) :keymaps 'org-mode-map "SPC x" 'org-ctrl-c-ctrl-c)
(general-define-key :states '(normal motion emacs) :keymaps 'override "SPC e" (lambda () (interactive) (find-file user-init-file)))
(general-define-key :states '(normal motion emacs) :keymaps 'override "SPC s" 'eshell)
(general-define-key :states '(normal motion emacs) :keymaps 'override "SPC p" 'projectile-command-map)
(general-define-key :states 'normal :keymaps 'TeX-mode-map "SPC c" 'compile-sagetex)
(general-define-key :states 'normal :keymaps 'pdf-view-mode-map "d" 'pdf-view-scroll-up-or-next-page)
(general-define-key :states 'normal :keymaps 'pdf-view-mode-map "u" 'pdf-view-scroll-down-or-previous-page)
(general-define-key :states 'normal :keymaps 'pdf-view-mode-map "K" 'pdf-view-enlarge)
(general-define-key :states 'normal :keymaps 'pdf-view-mode-map "J" 'pdf-view-shrink)
(general-define-key :states 'normal :keymaps 'dired-mode-map "l" 'dired-find-file)
(general-define-key :states 'normal :keymaps 'dired-mode-map "h" 'dired-up-directory)
(general-define-key :states 'normal :keymaps 'org-mode-map "SPC l s" 'org-store-link)
(general-define-key :states 'normal :keymaps 'org-mode-map "SPC l i" 'org-insert-link)
(general-define-key :states 'normal :keymaps 'org-mode-map "SPC l l" 'org-insert-last-stored-link)
(general-define-key :states 'normal :keymaps 'org-mode-map "SPC z" 'xenops-render)
(general-define-key :states 'normal :keymaps 'org-mode-map ")" 'org-next-block)
(general-define-key :states 'normal :keymaps 'org-mode-map "(" 'org-previous-block)
(general-define-key :states '(normal motion emacs) :keymaps 'override "SPC w" 'evil-window-map)
(general-define-key :states '(normal motion emacs) :keymaps 'override "SPC h" (general-simulate-key "C-h"))
(general-define-key :states '(normal motion emacs) :keymaps 'override "SPC u" 'save-buffer)
(general-define-key :states '(normal motion emacs) :keymaps 'override "SPC i"
                    (lambda ()
                      (interactive)
                        (org-insert-time-stamp (current-time) t)))
(general-define-key :states '(normal motion emacs) :keymaps 'override "SPC a" (lambda () (interactive) (find-file "/home/mahmooz/workspace/college/activity.org")))

;; keybinding to evaluate math expressions
(general-define-key :states '(normal motion emacs) :keymaps 'override "SPC m"
                    (lambda ()
                      (interactive)
                      (setq result (calc-eval (buffer-substring (region-beginning) (region-end))))
                      (end-of-line)
                      (insert " ")
                      (insert result)))
(general-define-key :states '(normal motion emacs) :keymaps 'override "SPC '" (general-simulate-key "C-c '"))

;; automatically run script being edited, demonstrates how we can auto compile files on save
(defun run-script ()
  "run the current bash script being edited"
  (interactive)
  (run-command-show-output (buffer-file-name)))
(add-hook 'sh-mode-hook
          (lambda ()
            (add-hook 'after-save-hook 'run-script 0 t)))

;; eshell configs
;; key to clear the screen
(defun run-this-in-eshell (cmd)
  "Runs the command 'cmd' in eshell."
  (with-current-buffer "*eshell*"
    (end-of-buffer)
    (eshell-kill-input)
    (message (concat "Running in Eshell: " cmd))
    (insert cmd)
    (eshell-send-input)
    (end-of-buffer)
    (eshell-bol)
    (yank)))
(add-hook 'eshell-mode-hook
          (lambda ()
            (general-define-key :states '(normal) :keymaps 'local "SPC c" (lambda () (interactive) (run-this-in-eshell "clear 1")))))
;; make the cursor stay at the prompt when scrolling
(setq eshell-scroll-to-bottom-on-input t)

;; compile org docs to pdfs and put them in ~/.emacs.d/latex/
(defun org-to-pdf ()
  (interactive)
  (let ((outfile (concat (get-latex-cache-dir-path) (concat (current-filename) ".tex"))))
    (call-process-shell-command (format "rm %s*%s*" (get-latex-cache-dir-path) (current-filename)))
    (org-export-to-file 'latex outfile
      nil nil nil nil nil nil)
    (compile-latex-file outfile)))
(general-define-key :states '(normal motion emacs) :keymaps 'org-mode-map "SPC c"
                    (lambda ()
                      (interactive)
                      ;; (setq previous-theme (car custom-enabled-themes))
                      ;; (if (not (eq previous-theme 'poet))
                      ;;     (switch-to-light-theme))
                      (org-to-pdf)
                      (org-hugo-export-to-md)))
                      ;; (if (not (eq previous-theme 'poet))
                      ;;     (switch-to-dark-theme))))
;; function to execute buffer with light theme
(defun org-execute-buffer-with-light-theme ()
  (interactive)
  (switch-to-light-theme)
  (org-babel-execute-buffer)
  (switch-to-dark-theme))
;; change latex images cache location
(setq org-preview-latex-image-directory (get-latex-cache-dir-path))
;; make latex preview bigger
;;(plist-put org-format-latex-options :scale 1.2)
;; allow usage of #+BIND in latex exports
(setq org-export-allow-bind-keywords t)
;; make images default to their original size in latex exports
(setq org-latex-image-default-scale "0.6")
;; enable latex snippets in org mode
(defun my-org-latex-yas ()
  "Activate org and LaTeX yas expansion in org-mode buffers."
  (yas-minor-mode)
  (yas-activate-extra-mode 'latex-mode))
(add-hook 'org-mode-hook #'my-org-latex-yas)
;; preserve all line breaks when exporting
(setq org-export-preserve-breaks t)
;; indent headings properly
(add-hook 'org-mode-hook 'org-indent-mode)

(setq org-latex-listings t ;; use listings package for latex code blocks
      org-time-stamp-formats '("<%Y-%m-%d %a>" . "<%Y-%m-%d %a %H:%M:%S>")) ;; timestamp with seconds
;; some extra libraries to use with latex
(add-to-list 'org-latex-default-packages-alist '("" "tkz-euclide" t))
(add-to-list 'org-latex-default-packages-alist '("" "tikz" t))
(add-to-list 'org-latex-default-packages-alist '("" "pgfplots" t))
(add-to-list 'org-latex-default-packages-alist '("" "cancel" t))
(add-to-list 'org-latex-default-packages-alist '("" "mathtools" t))
;;(setq org-format-latex-header (concat org-format-latex-header "\\usetikzlibrary{tikzmark,calc,fit,matrix}"))
;; give svg's a proper width when exporting with dvisvgm
(with-eval-after-load 'ox-html
  (setq org-html-head
        (replace-regexp-in-string
         ".org-svg { width: 90%; }"
         ".org-svg { width: auto; }"
         org-html-style-default)))
;; better than the default, works for tikzpicture
(setq org-preview-latex-default-process 'imagemagick)
;; syntax highlighting for latex fragments in org mode
(setq org-highlight-latex-and-related '(native latex script entities))
(require 'org-src)
(add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t)))
;; make org-babel java act like other langs
(setq org-babel-default-header-args:java
      '((:dir . nil)
        (:results . "value")))
;; use unique id's to identify headers, better than using names cuz names could change
(setq org-id-link-to-org-use-id t)
;; org agenda
(setq org-agenda-files '("/home/mahmooz/workspace/college/agenda.org"))
;; load some files into org babel library
(org-babel-lob-ingest "~/workspace/college/data_structures/data_structures.org")
(org-babel-lob-ingest "~/workspace/college/code/sage.org")
(org-babel-lob-ingest "~/workspace/college/code/tikz.org")
;; creation dates for TODOs
(defun my/log-todo-creation-date (&rest ignore)
  "Log TODO creation time in the property drawer under the key 'CREATED'."
  (when (and (org-get-todo-state)
             (not (org-entry-get nil "CREATED")))
    (org-entry-put nil "CREATED" (format-time-string (cdr org-time-stamp-formats)))))
(add-hook 'org-after-todo-state-change-hook #'my/log-todo-creation-date)
;; src block indentation / editing / syntax highlighting
(setq org-src-window-setup 'current-window
      org-src-strip-leading-and-trailing-blank-lines t)

(defun insert-random-string (NUM)
  "Insert a random alphanumerics string of length NUM."
  (interactive "P")
  (let* (($charset "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
         ($baseCount (length $charset)))
    (dotimes (_ (if (numberp NUM) (abs NUM) NUM))
      (insert (elt $charset (random $baseCount))))))
(global-set-key (kbd "C-c r") (lambda () (interactive) (insert-random-string 6)))

(defun switch-to-dark-theme ()
  "switch to dark theme"
  (interactive)
  (disable-theme 'poet)
  (load-theme 'poet-dark t)
  ;;(add-hook 'pdf-view-mode-hook 'pdf-view-themed-minor-mode)
  (set-themed-pdf 1))

(defun switch-to-light-theme ()
  "switch to light theme"
  (interactive)
  (disable-theme 'poet-dark)
  (load-theme 'poet t)
  ;;(remove-hook 'pdf-view-mode-hook 'pdf-view-themed-minor-mode)
  (set-face-background hl-line-face "PeachPuff3")
  (set-themed-pdf 1))

(defun set-themed-pdf (should-be-themed)
  "if 1 is passed the buffers with pdf files open will be themed using pdf-tools, unthemed if 0"
  (dolist (buffer (buffer-list))
    (if (buffer-name buffer)
        (if (string-match ".*.pdf$" (buffer-name buffer))
            (with-current-buffer (buffer-name buffer)
              (pdf-view-themed-minor-mode should-be-themed)
              (pdf-view-refresh-themed-buffer t))))))

(defun kill-all-dired-buffers ()
  "Kill all dired buffers."
  (interactive)
  (save-excursion
    (let ((count 0))
      (dolist (buffer (buffer-list))
        (set-buffer buffer)
        (when (equal major-mode 'dired-mode)
          (setq count (1+ count))
          (kill-buffer buffer)))
      (message "Killed %i dired buffer(s)." count))))

;; make org roam insert link after cursor in evil mode
(defadvice org-roam-node-insert (around append-if-in-evil-normal-mode activate compile)
  "If in evil normal mode and cursor is on a whitespace character, then go into
append mode first before inserting the link. This is to put the link after the
space rather than before."
  (let ((is-in-evil-normal-mode (and (bound-and-true-p evil-mode)
                                     (not (bound-and-true-p evil-insert-state-minor-mode))
                                     (looking-at "[[:blank:]]"))))
    (if (not is-in-evil-normal-mode)
        ad-do-it
      (evil-append 0)
      ad-do-it
      (evil-normal-state))))

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

;; gotta keep this here at the end so nothing overrides it.
(set-face-attribute 'default nil :family "Inconsolata" :height 130)
(set-face-attribute 'fixed-pitch nil :family "Noto Sans Mono")
(set-face-attribute 'variable-pitch nil :family "Noto Sans Mono")
(load-theme 'modus-operandi t)
