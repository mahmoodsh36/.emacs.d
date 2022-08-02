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
(setq-default tab-width 2
              js-indent-level 2
              c-basic-offset 2
              indent-tabs-mode nil
              python-indent-offset 4)
(setq evil-shift-width 2)
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
(when window-system (set-frame-size (selected-frame) 100 45))
;; no damn fringes dude!
(set-fringe-style 0)
;; display only buffer name in modeline
(setq-default mode-line-format (list " " mode-line-modified "%e %b" mode-line-position-line-format))
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
;; (desktop-save-mode 1)
;; save minibuffer history
(setq savehist-file (expand-file-name "~/brain/emacs_savehist"))
(savehist-mode 1)
(add-to-list 'savehist-additional-variables 'search-ring)
(add-to-list 'savehist-additional-variables 'regexp-search-ring)
(add-to-list 'savehist-additional-variables 'kill-ring)
;; break long lines into multiple
(global-visual-line-mode)
;; stop the annoying warnings from org mode cache
(setq warning-minimum-level :emergency)

;; smooth scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don"t accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

;;(setq evil-disable-insert-state-bindings t)
(setq enable-evil t)

;; the all-powerful org mode
(use-package org)
(use-package org-contrib)

;; the key to building a second brain in org mode
(use-package org-roam
  :custom
  (org-roam-directory (file-truename "~/brain/"))
  (org-roam-completion-everywhere t)
  :config
  (setq org-roam-node-display-template "${title:*} ${tags:*}")
  (org-roam-db-autosync-mode)
  (require 'org-roam-export)
  (require 'org-roam-protocol)
  (setq org-roam-capture-templates
        '(("n" "note" plain "%?"
           :if-new (file+head "notes/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}")
           :kill-buffer :unnarrowed t)
          ("q" "quick note" plain "%?"
           :if-new (file+head "quick/%<%Y%m%d%H%M%S>.org" "#+filetags: :quick-note:")
           :kill-buffer :unnarrowed t)
          ("t" "todo" entry "* TODO %?"
           :if-new (file "agenda.org")
           :kill-buffer :unnarrowed t))))

;; side tree
(use-package treemacs
  :config
  (treemacs-follow-mode -1))

;; makes binding keys less painful
(use-package general)

;; needed for evil mode
(use-package undo-fu)

(use-package counsel
  :config
  (ivy-mode)
  (setq ivy-height 25)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file))

;; evil-mode
(if enable-evil
    (progn
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

      ;; evil mode support for org
      (use-package evil-org
        :config
        (add-hook 'org-mode-hook 'evil-org-mode)
        (evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading return calendar))
        (require 'evil-org-agenda)
        (evil-org-agenda-set-keys)
        ;; the package annoyingly binds O to another function so here im just restoring it
        (general-define-key :states 'normal :keymaps 'override "O" 'evil-open-above)

        ;; for some reason these keys act weirdly in org mode with xenops so im gonna rebind them..
        ;; (general-define-key :states 'normal :keymaps 'org-mode-map "p" 'evil-paste-after)
        ;; (general-define-key :states 'normal :keymaps 'override "o"
        ;;                     (lambda ()
        ;;                       (interactive)
        ;;                       (evil-org-append-line 1)
        ;;                       (evil-ret 1)
        ;;                       (indent-according-to-mode))))
        (general-define-key :states '(normal visual motion operator) :keymaps 'override "0" 'evil-beginning-of-line)
        (general-define-key :states '(normal visual motion operator) :keymaps 'override "$" 'evil-end-of-line)
        (general-define-key :states '(normal visual motion operator) :keymaps 'override "^" 'evil-first-non-blank))

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

      ;; jump to matching tags
      (use-package evil-matchit
        :config
        (global-evil-matchit-mode 1))

      ;; text evil objects for latex
      (use-package evil-tex
        :config
        (add-hook 'LaTeX-mode-hook #'evil-tex-mode)
        (add-hook 'org-mode-hook #'evil-tex-mode))

      ;; preview registers and marks before actually using them
      (use-package evil-owl
        :config
        (evil-owl-mode))

      ;; interpret words of columns as a text object
      (use-package evil-textobj-column
        :config
        (define-key evil-inner-text-objects-map "c" 'evil-textobj-column-word)
        (define-key evil-inner-text-objects-map "C" 'evil-textobj-column-WORD))

      ;; extend evil-surround functionality
      (use-package evil-embrace
        :config
        (evil-embrace-enable-evil-surround-integration)
        (add-hook 'org-mode-hook 'embrace-org-mode-hook))

      ;; this macro was copied from here: https://stackoverflow.com/a/22418983/4921402
      (defmacro define-and-bind-quoted-text-object (name key start-regex end-regex)
        (let ((inner-name (make-symbol (concat "evil-inner-" name)))
              (outer-name (make-symbol (concat "evil-a-" name))))
          `(progn
             (evil-define-text-object ,inner-name (count &optional beg end type)
               (evil-select-paren ,start-regex ,end-regex beg end type count nil))
             (evil-define-text-object ,outer-name (count &optional beg end type)
               (evil-select-paren ,start-regex ,end-regex beg end type count t))
             (define-key evil-inner-text-objects-map ,key #',inner-name)
             (define-key evil-outer-text-objects-map ,key #',outer-name))))
      (define-and-bind-quoted-text-object "dollar" "$" "\\$" "\\$")
      (define-and-bind-quoted-text-object "pipe" "|" "|" "|")
      (define-and-bind-quoted-text-object "slash" "/" "/" "/")
      (define-and-bind-quoted-text-object "space" " " " " " ")
      (define-and-bind-quoted-text-object "tilda" "~" "~" "~")
      (define-and-bind-quoted-text-object "asterisk" "*" "*" "*")

      ;; create "il"/"al" (inside/around) line text objects:
      ;; (define-and-bind-text-object "l" "^\\s-*" "\\s-*$")
      ;; create "ia"/"aa" (inside/around) entire buffer text objects:
      (define-and-bind-quoted-text-object "buffer" "a" "\\`\\s-*" "\\s-*\\'")

      (general-evil-setup)

      (evil-define-key 'normal 'TeX-mode-map (kbd "SPC v") 'open-current-document-this-window)
      (general-define-key :states 'normal :keymaps 'override "s" 'save-buffer)
      (general-define-key :states '(normal motion emacs) :keymaps 'override "SPC d w" (lambda () (interactive) (dired "~/dl/")))
      (general-define-key :states '(normal motion emacs) :keymaps 'override "SPC d a" (lambda () (interactive) (dired "~/data/")))
      (general-define-key :states '(normal motion emacs) :keymaps 'override "SPC d c" (lambda () (interactive) (dired "~/brain/")))
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
      (general-define-key :states '(normal motion emacs) :keymaps 'override "SPC p" 'projectile-command-map)
      ;; (general-define-key :states 'normal :keymaps 'TeX-mode-map "SPC c" 'compile-sagetex)
      (general-define-key :states 'normal :keymaps 'pdf-view-mode-map "d" 'pdf-view-scroll-up-or-next-page)
      (general-define-key :states 'normal :keymaps 'pdf-view-mode-map "u" 'pdf-view-scroll-down-or-previous-page)
      (general-define-key :states 'normal :keymaps 'pdf-view-mode-map "K" 'pdf-view-enlarge)
      (general-define-key :states 'normal :keymaps 'pdf-view-mode-map "J" 'pdf-view-shrink)
      ;; (general-define-key :states 'normal :keymaps 'dired-mode-map "l" 'dired-find-file)
      ;; (general-define-key :states 'normal :keymaps 'dired-mode-map "h" 'dired-up-directory)
      (general-define-key :states 'normal :keymaps 'org-mode-map "SPC l s" 'org-store-link)
      (general-define-key :states 'normal :keymaps 'org-mode-map "SPC l i" 'org-insert-link)
      (general-define-key :states 'normal :keymaps 'org-mode-map "SPC l l" 'org-insert-last-stored-link)
      (general-define-key :states 'normal :keymaps 'org-mode-map "SPC z"
                          (lambda ()
                            (interactive)
                            (if (not xenops-mode)
                                (xenops-mode)
                              (xenops-render))))
      (general-define-key :states 'normal :keymaps 'org-mode-map ")" 'org-next-block)
      (general-define-key :states 'normal :keymaps 'org-mode-map "(" 'org-previous-block)
      (general-define-key :states '(normal motion emacs) :keymaps 'override "SPC w" 'evil-window-map)
      (general-define-key :states '(normal motion emacs) :keymaps 'override "SPC h" (general-simulate-key "C-h"))
      (general-define-key :states '(normal motion emacs) :keymaps 'override "SPC i"
                          (lambda ()
                            (interactive)
                            (org-insert-time-stamp (current-time) t)))
      (general-define-key :states '(normal motion emacs) :keymaps 'override "SPC a" (lambda () (interactive) (find-file "/home/mahmooz/brain/agenda.org")))
      ;;(define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)
      (general-define-key :states 'normal :keymaps 'override "SPC r t" 'org-roam-buffer-toggle)
      (general-define-key :states 'normal :keymaps 'override "SPC r f" 'org-roam-node-find)
      (general-define-key :states 'normal :keymaps 'override "SPC r g" 'org-roam-graph)
      (general-define-key :states 'normal :keymaps 'override "SPC r i" 'org-roam-node-insert)
      (general-define-key :states 'normal :keymaps 'override "SPC r c" 'org-roam-capture)
      (general-define-key :states 'normal :keymaps 'override "SPC r d" 'org-roam-dailies-capture-today)
      (general-define-key :states 'normal :keymaps 'override "SPC r c" 'org-id-get-create)
      (general-define-key :states 'normal :keymaps 'override "SPC r o" 'org-open-at-point)
      (general-define-key :states 'normal :keymaps 'override "SPC r a" 'org-attach)
      (general-define-key :states 'normal :keymaps 'override "SPC r A" 'org-attach-open)
      (general-define-key :states 'normal :keymaps 'override "SPC r l" 'org-roam-alias-add)
      (general-define-key :states 'normal :keymaps 'override "SPC r n"
                          (lambda ()
                            (interactive)
                            (org-roam-capture nil "n")))
      (general-define-key :states 'normal :keymaps 'override "SPC r w" 'org-roam-tag-add)
      (general-define-key :states 'normal :keymaps 'override "SPC r q"
                          (lambda ()
                            (interactive)
                            (org-roam-capture-no-title-prompt nil "q")))
      (general-define-key :states 'normal :keymaps 'override "SPC r e"
                          (lambda ()
                            (interactive)
                            (org-roam-capture nil "t")))
      (general-define-key :states 'normal :keymaps 'org-mode-map "SPC r x"
                          (lambda ()
                            (interactive)
                            (org-to-pdf)
                            (org-hugo-export-to-md)))
      (general-define-key :states 'normal :keymaps 'org-mode-map "SPC r [" 'org-clock-in)
      (general-define-key :states 'normal :keymaps 'org-mode-map "SPC r ]" 'org-clock-out)
      (general-define-key :states 'normal :keymaps 'org-mode-map "SPC r -" 'org-clock-cancel)
      (general-define-key :states 'normal :keymaps 'org-mode-map "SPC r p" 'org-clock-display)
      (general-define-key :states 'normal :keymaps 'org-mode-map "SPC r b" 'org-babel-tangle)
      (general-define-key :states 'normal :keymaps 'override "SPC c" 'calc)

      ;; keys to search for files
      (define-key evil-normal-state-map (kbd "SPC f c")
                  (lambda () (interactive) (search-open-file "~/workspace/college" ".*\\(pdf\\|tex\\|doc\\|mp4\\|png\\)")))
      (define-key evil-normal-state-map (kbd "SPC F c")
                  (lambda () (interactive)
                    (search-open-file-in-emacs "~/workspace/college" ".*\\(pdf\\|tex\\|doc\\|org\\)")))
      (define-key evil-normal-state-map (kbd "SPC f p")
                  (lambda () (interactive) (search-open-file "~/data/p" "")))
      (define-key evil-normal-state-map (kbd "SPC f b")
                  (lambda () (interactive) (search-open-file "~/brain/" "")))
      (define-key evil-normal-state-map (kbd "SPC f d")
                  (lambda () (interactive) (search-open-file "~/data" "")))
      (define-key evil-normal-state-map (kbd "SPC F d")
                  (lambda () (interactive)
                    (search-open-file-in-emacs "~/data" "")))

      ;; keybinding to evaluate math expressions
      (general-define-key :states '(normal motion emacs) :keymaps 'override "SPC m"
                          (lambda ()
                            (interactive)
                            (setq result (calc-eval (buffer-substring-no-properties (region-beginning) (region-end))))
                            (end-of-line)
                            (insert " ")
                            (insert result)))
      (general-define-key :states '(normal motion emacs) :keymaps 'override "SPC '" (general-simulate-key "C-c '"))
      
      ;; key to clear the screen in eshell
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

      ;; evil mode multiple cursors
      (use-package evil-mc
        :config
        (global-evil-mc-mode))

      (use-package evil-escape
        :config
        (evil-escape-mode))

      ;; interpret function arguments as a text object
      (use-package evil-args)
      (use-package evil-lion)

      (use-package evil-extra-operator)
      ;;(use-package evil-textobj-tree-sitter)

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
      ))

(defun org-roam-capture-no-title-prompt (&optional goto keys &key filter-fn templates info)
  (interactive "P")
  (org-roam-capture- :goto goto
                     :info info
                     :keys keys
                     :templates templates
                     :node (org-roam-node-create :title "")
                     :props '(:immediate-finish nil)))

;; (use-package god-mode
;; :config
;; (god-mode)
;; (global-set-key (kbd "<escape>") #'god-mode-all)
;; (setq god-exempt-major-modes nil)
;; (setq god-exempt-predicates nil))
;; (defun my-god-mode-update-cursor-type ()
;;   (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar)))
;; (add-hook 'post-command-hook #'my-god-mode-update-cursor-type)

;; projectile
(use-package projectile
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-mode +1))

;; auto completion
(setq completion-ignore-case t) ;; case-insensitivity
(use-package company
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
        company-show-quick-access t)
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

;; icons for dired
(use-package all-the-icons
  :custom
  (all-the-icons-dired-monochrome nil))
(use-package all-the-icons-dired
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;; (set-face-attribute 'default nil :family "Comic Sans MS" :height 120)
;; (set-face-attribute 'default nil :family "Cascadia Code" :height 130)
;; (set-face-attribute 'default nil :family "Monaco" :height 120)
(set-face-attribute 'default nil :family "DejaVu Sans Mono" :height 130)
(set-face-attribute 'fixed-pitch nil :family "DejaVu Sans Mono" :height 120)
(set-face-attribute 'variable-pitch nil :family "DejaVu Sans Mono" :height 120)
(use-package darktooth-theme)
(use-package modus-themes)
(use-package ample-theme)
(use-package anti-zenburn-theme)
(use-package zenburn-theme)
(use-package poet-theme)
(use-package gruvbox-theme)
(use-package inkpot-theme)
;; (load-theme 'darktooth t)
;; (modus-themes-load-operandi)

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
  (setq sage-shell:input-history-cache-file "~/brain/sage_history")
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
  ;; disable builtin snippets
  (setq yas-snippet-dirs
       `(,(concat user-emacs-directory "snippets")))
  (setq yas-triggers-in-field t)
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

;; modern API for working with files/dirs
(use-package f)

;; language server protocol support
(use-package lsp-mode
  :config
  (add-hook 'prog-mode-hook 'lsp-mode))

;; show simple info on the right
(use-package lsp-ui
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (setq lsp-ui-doc-delay 0)
  (setq lsp-ui-sideline-delay 0)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

;; lsp support for treemacs
(use-package lsp-treemacs
  :config
  (lsp-treemacs-sync-mode 1)
  (treemacs-resize-icons 15)
  (setq treemacs-width 30))

;; ensure the PATH variable is set according to the users shell, solves some issues on macos
;; (use-package exec-path-from-shell
;;   :config
;;   (exec-path-from-shell-initialize))

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
;;(use-package dart-mode)
(use-package flutter)
;;(use-package lsp-dart)

;; best pdf viewer
(use-package pdf-tools
  :config
  (pdf-tools-install t)
  (add-hook 'pdf-view-mode-hook 'pdf-view-themed-minor-mode))

;; latex company backend
(use-package company-auctex
  :config
  (company-auctex-init))

;; history for ivy completion, it sometimes makes ivy really slow, so maybe remove the cache file every once in a while
(use-package ivy-prescient
  :config
  (ivy-prescient-mode)
  (prescient-persist-mode 1)
  (setq prescient-save-file (expand-file-name "~/brain/emacs_prescient"))) ;; save history to filesystem
(use-package company-prescient
  :config
  (company-prescient-mode))

;; ;; auto indentation
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

;; provides syntax highlighting when exporting from org mode to html
(use-package htmlize)

;; static website generation for org mode
(use-package ox-hugo
  :config
  (setq org-hugo-base-dir "/home/mahmooz/workspace/blog/")
  (setq org-hugo-section "post")
  (setq org-more-dir (expand-file-name "~/workspace/blog/static/more/"))
  (ignore-errors (make-directory org-more-dir))
  (add-to-list 'org-hugo-external-file-extensions-allowed-for-copying "webp"))

;; best latex preview functionality
(use-package xenops
  :config
  (setq xenops-reveal-on-entry t
        xenops-math-latex-max-tasks-in-flight 6
        xenops-math-latex-process 'imagemagick)
  (add-hook 'LaTeX-mode-hook #'xenops-mode)
  ;; (add-hook 'org-mode-hook #'xenops-mode)
  (add-hook 'xenops-mode-hook 'xenops-render)
  (add-hook 'org-babel-after-execute-hook (lambda ()
                                            (interactive)
                                            (ignore-errors (xenops-render))))
  (setq xenops-math-image-scale-factor 1.4)
  (setcar (cdr (car xenops-elements))
          '(:delimiters
            ("^[ 	]*\\\\begin{\\(align\\|equation\\|gather\\)\\*?}" "^[ 	]*\\\\end{\\(align\\|equation\\|gather\\)\\*?}")
            ("^[ 	]*\\\\\\[" "^[ 	]*\\\\\\]"))))

(use-package mixed-pitch
  :hook
  (text-mode . mixed-pitch-mode))

;; show hidden elements when cursor is over them like links/markers etc
;; (use-package org-appear
;;   :config
;;   (setq org-appear-autoemphasis t
;;         org-appear-autoentities t
;;         org-appear-autokeywords t
;;         org-appear-autolinks t
;;         org-appear-autosubmarkers t)
;;   (add-hook 'org-mode-hook 'org-appear-mode))

;; add edition/creation timestamps to headers and files
(use-package org-roam-timestamps
  :config
  (org-roam-timestamps-mode)
  (setq org-roam-timestamps-remember-timestamps t))

;; give org mode a better look
(use-package org-modern
  :config
  (setq
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t
   ;; Agenda styling
   org-agenda-tags-column 0
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────")
  (global-org-modern-mode))

;; more featureful ivy menus
(use-package ivy-rich
  :config
  (ivy-rich-mode 1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package elfeed-tube
  :straight (:host github :repo "karthink/elfeed-tube")
  :after elfeed
  :demand t
  :config
  ;; (setq elfeed-tube-auto-save-p nil) ;; t is auto-save (not default)
  ;; (setq elfeed-tube-auto-fetch-p t) ;;  t is auto-fetch (default)
  (elfeed-tube-setup)
  :bind (:map elfeed-show-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save)
         :map elfeed-search-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save)))

;; progress in mode line
;; (use-package procress
;;   :straight (:host github :repo "haji-ali/procress")
;;   :commands tex-procress-mode
;;   :init
;;   (add-hook 'LaTeX-mode-hook 'tex-procress-mode)
;;   :config
;;   (procress-load-default-svg-images))

(use-package dumb-jump)
(use-package ob-async)
(use-package csharp-mode)
(use-package format-all)
(use-package org-roam-ui)
;; (use-package corfu
;;   :init
;;   (global-corfu-mode))
;; (use-package code-compass)

;; (use-package lastfm)
;; (use-package vuiet
;;   :config
;;   (setq browse-url-browser-function 'browse-url-chrome))

(use-package org-ml)

;; (use-package lispy)
;; (use-package jupyter)
;; (use-package ein)

;; (use-package delve
;;   :straight (:repo "publicimageltd/delve" :host github))
;; (use-package embark)
;; (use-package orderless)
;; (use-package org-transclusion)
;; (use-package svg-tag-mode)

;; (use-package org-ref)
;; (use-package alert)
;; (use-package olivetti)
;; (use-package ox-json)

;;(use-package org-tree-slide)
;;(use-package orgajs) installed externally i think
;;(use-package roam-block)
;;(use-package aio)
;;(use-package slime
;;  :config
;;  (setq inferior-lisp-program "sbcl"))
;;(use-package magit)
;;(use-package org-super-agenda)
;;(use-package org-web-tools)
;;(use-package system-packages)
;;(use-package org-ql)
;;(use-package copilot)
;;(use-package ox-pandoc)
;;(use-package org-download)
;;(use-package org-html-themes)
;;(use-package org-ioslide)
;;(use-package org-protocol-capture-html)
;;(use-package google-this)
;;(use-package google-translate)
;;(use-package google-maps)
;;(use-package plantuml-mode)

;; start server
(server-start)

;; c-x c-l to complete line like vim
(defun my-expand-lines ()
  (interactive)
  (let ((hippie-expand-try-functions-list
         '(try-expand-line)))
    (call-interactively 'hippie-expand)))
(general-define-key "C-x C-l" 'my-expand-lines)

;; org mode config
;; save the clock history across sessions
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
;; make org babel default to python3
(setq org-babel-python-command "python3")
;; increase org table max lines
(setq org-table-convert-region-max-lines 10000)
;; to increase depth of the imenu in treemacs
(setq org-imenu-depth 4)
;; who cares about annoying broken links errors..
;; (setq org-export-with-broken-links t)

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
  (start-process-shell-command "latex" "latex" (format "lualatex -shell-escape -output-directory=%s %s" (get-latex-cache-dir-path) path)))

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

;; tex hook to auto compile on save
(add-hook
 'TeX-mode-hook
 (lambda ()
   (compile-current-document)
   (add-hook 'after-save-hook 'compile-current-document 0 t)))

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

;; keybindings
(global-set-key (kbd "C-M-S-x") 'eval-region)
(global-set-key (kbd "C-x D") 'image-dired)

;; automatically run script being edited, demonstrates how we can auto compile files on save
(defun run-script ()
  "run the current bash script being edited"
  (interactive)
  (run-command-show-output (buffer-file-name)))
(add-hook 'sh-mode-hook
          (lambda ()
            (add-hook 'after-save-hook 'run-script 0 t)))

;; eshell configs
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
;; change latex images cache location
(setq org-preview-latex-image-directory (get-latex-cache-dir-path))
;; make latex preview bigger
(plist-put org-format-latex-options :scale 1.5)
;; allow usage of #+BIND in latex exports
(setq org-export-allow-bind-keywords t)
;; decrease image size in latex exports
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
(setq org-todo-keywords
  '((sequence
     "TODO(t!)" ; Initial creation
     "GO(g@)"; Work in progress
     "WAIT(w@)" ; My choice to pause task
     "REVIEW(r!)" ; Inspect or Share Time
     "|" ; Remaining close task
     "DONE(d@)" ; Normal completion
     "CANCELED(c@)" ; Not going to od it
     )))

(setq org-latex-listings t ;; use listings package for latex code blocks
      org-time-stamp-formats '("<%Y-%m-%d %a>" . "<%Y-%m-%d %a %H:%M:%S>") ;; timestamp with seconds
      org-latex-src-block-backend 'listings)
;; give svg's a proper width when exporting with dvisvgm
(with-eval-after-load 'ox-html
  (setq org-html-head
        (replace-regexp-in-string
         ".org-svg { width: 90%; }"
         ".org-svg { width: auto; }"
         org-html-style-default)))
;; enable <s code block snippet
(require 'org-src)
;; make org-babel java act like other langs
(setq org-babel-default-header-args:java
      '((:dir . nil)
        (:results . "value")))
;; use unique id's to identify headers, better than using names cuz names could change
(setq org-id-link-to-org-use-id t)
;; org agenda
(setq org-agenda-files '("/home/mahmooz/brain/agenda.org"))
(defun lob-reload ()
  "load some files into org babel library"
  (interactive)
  (org-babel-lob-ingest "~/brain/data_structures/data_structures.org")
  (org-babel-lob-ingest "~/brain/code/sage.org")
  (org-babel-lob-ingest "~/brain/code/tikz.org"))
;; creation dates for TODOs
;; (defun my/log-todo-creation-date (&rest ignore)
;;   "Log TODO creation time in the property drawer under the key 'CREATED'."
;;   (when (and (org-get-todo-state)
;;              (not (org-entry-get nil "CREATED")))
;;     (org-entry-put nil "CREATED" (format-time-string (cdr org-time-stamp-formats)))))
;; (add-hook 'org-after-todo-state-change-hook #'my/log-todo-creation-date)
;; src block indentation / editing / syntax highlighting
(setq org-src-window-setup 'current-window
      org-src-strip-leading-and-trailing-blank-lines t)
;; imagemagick is way better than the default especially for tikz
(setq org-preview-latex-default-process 'imagemagick)
;; latex syntax highlighting in org mode
(setq org-highlight-latex-and-related '(latex))
(setq org-html-mathjax-template "")
(setq org-html-mathjax-options '())
;; to enable imagemagick latex generation with :async
(setq org-babel-default-header-args:latex
      '((:results . "latex")
        (:exports . "results")
        (:fit . t)
        (:imagemagick . t)
        (:async . t)
        (:eval . "no-export")
        (:packages . ("\\usepackage{forest}"
                      "\\usepackage{amsmath}"
                      "\\usepackage{tikz}"
                      "\\usepackage{tikz-3dplot}"
                      "\\usepackage{pgfplots}"
                      "\\usetikzlibrary{tikzmark,calc,fit,matrix,arrows,automata,positioning}"
                      ))))
;; (add-to-list 'org-babel-default-header-args '(:eval . "no-export"))
;;(setq org-src-fontify-natively nil)
;; make org export deeply nested headlines as headlines still
(setq org-export-headline-levels 20)
;; workaround to make yasnippet expand after dollar sign in org mode
(add-hook 'org-mode-hook (lambda ()  (modify-syntax-entry ?$ "_" org-mode-syntax-table)))
;; startup with headlines and blocks folded
(setq org-startup-folded 'content)
      ;; org-hide-block-startup t)
;; try to get the width from an #+ATTR.* keyword and fall back on the original width if none is found.
(setq org-image-actual-width nil)
;; get rid of background colors of block lines bleeding all over folded headlines
(setq org-fontify-whole-block-delimiter-line nil)
(setq org-fold-catch-invisible-edits 'smart)

(defun generate-random-string (NUM)
  "Insert a random alphanumerics string of length NUM."
  (interactive "P")
  (setq random-str "")
  (let* (($charset "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
         ($baseCount (length $charset)))
    (dotimes (_ (if (numberp NUM) (abs NUM) NUM))
      (setq random-str (concat random-str (char-to-string (elt $charset (random $baseCount)))))))
  random-str)
(defun temp-file (EXT)
  (setq dir-path (concat (expand-file-name user-emacs-directory) "tmp/"))
  (ignore-errors (make-directory dir-path))
  (format "%s%s.%s" dir-path (generate-random-string 7) EXT))
(global-set-key (kbd "C-c r") (lambda () (interactive) (insert (generate-random-string 7))))

(defun switch-to-dark-theme ()
  "switch to dark theme"
  (interactive)
  (disable-theme 'gruvbox-light-medium)
  (load-theme 'darktooth t))
  ;; (add-hook 'pdf-view-mode-hook 'pdf-view-themed-minor-mode)
  ;; (set-themed-pdf 1))

(defun switch-to-light-theme ()
  "switch to light theme"
  (interactive)
  (disable-theme 'darktooth)
  (load-theme 'gruvbox-light-medium t))
  ;; (set-face-background hl-line-face "PeachPuff3"))
  ;; (remove-hook 'pdf-view-mode-hook 'pdf-view-themed-minor-mode)
  ;; (set-themed-pdf 1))

(switch-to-light-theme)
(lob-reload)

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

(defun yas-delete-if-empty ()
  "function to remove _{} or ^{} fields, used by some of my latex yasnippets"
  (interactive)
  (point-to-register 'my-stored-pos)
  (save-excursion
    (while (re-search-backward "\\(_{}\\)" (line-beginning-position) t)
      (progn
        (replace-match "" t t nil 1)
        (jump-to-register 'my-stored-pos)))
    (while (re-search-backward "\\(\\^{}\\)" (line-beginning-position) t)
      (progn
        (replace-match "" t t nil 1)
        (jump-to-register 'my-stored-pos)))))

;; disable stupid ox-hugo relative path exports
(defun non-relative-path (obj)
  "return non-relative path for hugo"
  (interactive)
  (if (eq (type-of obj) 'string)
      (progn
        ;; (message (format "filename: %s" obj))
        (file-name-nondirectory obj))
    obj))
(advice-add 'org-export-resolve-id-link :filter-return #'non-relative-path)

;; move over text object
;; (evil-define-motion evil-forward-text-object
;;   (count &optional text-object)
;;   "move to the end of following input text-object define 
;; in evil-inner-text-objects-map ."
;;   (unless text-object
;;       (setf text-object
;;             (let ((key (read-key-sequence "text-object:")))
;;               (lookup-key evil-inner-text-objects-map key))))
;;   (let* ((region (funcall text-object count))
;;          (end (nth 1 region)))
;;     (goto-char end)))
;; (define-key evil-motion-state-map (kbd "M-w")
;;   #'evil-forward-text-object)

;; (evil-define-motion evil-backward-text-object
;;   (count &optional text-object)
;;   "move to the begin of following input text-object define 
;; in evil-inner-text-objects-map ."
;;   (unless text-object
;;       (setf text-object
;;             (let ((key (read-key-sequence "text-object:")))
;;               (lookup-key evil-inner-text-objects-map key))))
;;   (let* ((region (funcall text-object count))
;;          (start (nth 0 region)))
;;     (goto-char start)))
;; (define-key evil-motion-state-map (kbd "M-b")
;;   #'evil-backward-text-object)
