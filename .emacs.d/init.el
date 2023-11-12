;; disable annoying warnings
(setq native-comp-async-report-warnings-errors nil)
;; disable customization using the interactive interface and remove startup screen
(setq custom-file "/dev/null")
;; disable stupid startup screen
(setq inhibit-startup-screen t)

;; setup use-package, it provides stable packages unlike straight.el so i use it as the default package manager
(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(unless package-archive-contents
  (package-refresh-contents))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))
(require 'use-package)

;; setup quelpa
(setq quelpa-update-melpa-p nil)
(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))
(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

;; path where all my notes etc go
(setq brain-path (file-truename "~/brain/"))
(defconst *music-dir* (file-truename "~/music/"))

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
(menu-bar-mode -1) ;; enable it so that emacs acts like a normal app on macos
(toggle-scroll-bar -1)
(tool-bar-mode -1)
;; always follow symlinks
(setq vc-follow-symlinks t)
;; y-or-n instead of yes-or-no
(defalias 'yes-or-no-p 'y-or-n-p)
;; all backups to one folder
(setq backup-directory-alist `((".*" . ,"~/.emacs.d/backup/"))
      auto-save-file-name-transforms `((".*" ,"~/.emacs.d/backup/" t)))
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
;; (when window-system (set-frame-size (selected-frame) 120 48))
(when window-system (set-frame-size (selected-frame) 100 50))
;; margin around the windows
;; (set-fringe-style '(12 . 0))
(set-fringe-style '(0 . 0))
;; display only buffer name in modeline
;; the following line enables L<line number> at the end
(setq-default mode-line-format (list " " mode-line-modified "%e %b" mode-line-position-line-format))
;; (setq-default mode-line-format (list " " mode-line-modified "%e %b"))
;; restore default status line for pdf mode
(add-hook 'pdf-view-mode-hook
          (lambda ()
            (interactive)
            (setq-local mode-line-format (eval (car (get 'mode-line-format 'standard-value))))))
;; kill buffer without confirmation when its tied to a process
(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))
;; make tab complete current word
(setq dabbrev-case-replace nil)
(global-set-key "\t" 'dabbrev-completion)
;; save open buffers on exit
;; (desktop-save-mode 1)
;; save minibuffer history
(setq savehist-file (expand-file-name (concat brain-path "/emacs_savehist")))
(savehist-mode 1)
(add-to-list 'savehist-additional-variables 'search-ring)
(add-to-list 'savehist-additional-variables 'regexp-search-ring)
(add-to-list 'savehist-additional-variables 'kill-ring)
(setq history-length t) ;; no limit to history length
;; break long lines into multiple
;;(global-visual-line-mode)
;; stop the annoying warnings from org mode cache
(setq warning-minimum-level :emergency)
;; use imagemagick for formats like webp
(setq image-use-external-converter t)
;; display white spaces and newlines
(setq whitespace-style '(face tabs spaces trailing space-before-tab newline indentation empty space-after-tab space-mark tab-mark newline-mark missing-newline-at-eof))
;; show zero-width characters
(set-face-background 'glyphless-char "red")
;; change newline character
;;(setf (elt (car (cdr (cdr (assoc 'newline-mark whitespace-display-mappings)))) 0) ?⤸)
;; (global-whitespace-mode)
;; relative line numbers
;; (add-hook 'prog-mode-hook
;;           (lambda ()
;;             (setq display-line-numbers 'relative)))
;; disable annoying local variable prompt
(setq enable-local-variables :all)
;; stop always inserting a newline at the end of a file
(setq require-final-newline nil) ;; not sure if this is needed
(setq mode-require-final-newline nil)

;; smooth scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don"t accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

;; bibliography file (i use one global one for everything)
(setq org-cite-global-bibliography '("~/brain/bib.bib"))

(defun kill-all-buffers ()
  "kill all buffers excluding internal buffers (buffers starting with a space)"
  (interactive)
  (setq list (buffer-list))
  (while list
    (let* ((buffer (car list))
           (name (buffer-name buffer)))
      (and name
           (not (string-equal name ""))
           (/= (aref name 0) ?\s)
           (kill-buffer buffer)))
    (setq list (cdr list))))

(setq evil-want-keybinding nil)
;; enable emacs keys in evil insert mode
(setq evil-disable-insert-state-bindings t)
(setq enable-evil nil)
(setq enable-god nil)
;;(defconst *leader-key* "C-z");;"SPC")
;;(global-set-key (kbd "<escape>") (make-sparse-keymap))
(defconst *leader-key* "<escape>")
(defun led (key-str)
  "return the keybinding `key-str', which is taken as a string, prefixed by the leader key defined in '*leader-key*'"
  (concat *leader-key* " " key-str))

;; need straight for tecosaur's org version, for now, so install both
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(use-package org
  :defer
  :straight `(org
              :fork (:host nil
                           :repo "https://git.tecosaur.net/tec/org-mode.git"
                           :branch "dev"
                           :remote "tecosaur")
              :files (:defaults "etc")
              :build t
              :pre-build
              (with-temp-file "org-version.el"
                (require 'lisp-mnt)
                (let ((version
                       (with-temp-buffer
                         (insert-file-contents "lisp/org.el")
                         (lm-header "version")))
                      (git-version
                       (string-trim
                        (with-temp-buffer
                          (call-process "git" nil t nil "rev-parse" "--short" "HEAD")
                          (buffer-string)))))
                  (insert
                   (format "(defun org-release () \"The release version of Org.\" %S)\n" version)
                   (format "(defun org-git-version () \"The truncate git commit hash of Org mode.\" %S)\n" git-version)
                   "(provide 'org-version)\n")))
              :pin nil))

;; the all-powerful org mode
;; (use-package org)
(use-package org-contrib)

;; the key to building a second brain in org mode, requires pre-isntallation of gcc/clang
(use-package org-roam
  :custom
  (org-roam-directory brain-path)
  (org-roam-completion-everywhere t)
  :config
  ;; (setq org-roam-node-display-template "${title:*} ${tags:*}")
  (org-roam-db-autosync-mode 1)
  (require 'org-roam-export)
  (require 'org-roam-protocol)
  (global-set-key (kbd "C-c r f") 'org-roam-node-find)
  (global-set-key (kbd "C-c r g") 'org-roam-graph)
  (setq org-roam-capture-templates
        '(("n" "note" plain "%?"
           :if-new (file+head "notes/%<%Y%m%d%H%M%S>-${slug}.org" "#+setupfile: ~/.emacs.d/setup.org\n#+include: ~/.emacs.d/common.org\n#+title: ${title}")
           :kill-buffer t :unnarrowed t :empty-lines-after 0)
          ("k" "quick note" plain "%?"
           :if-new (file+head "quick/%<%Y%m%d%H%M%S>.org" "#+filetags: :quick-note:")
           :kill-buffer t :unnarrowed t :empty-lines-after 0)
          ("d" "daily" plain "%?" ;;"* %T %?"
           ;; ("d" "daily" plain "* %T %<%Y-%m-%d %H:%M:%S> %?"
           :if-new (file+head "daily/%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n#+filetags: :daily:")
           :kill-buffer t :unnarrowed t :empty-lines-after 0 :immediate-finish t)
          ("t" "todo" plain "* TODO ${title}"
           :if-new (file+head "notes/agenda.org" "#+title: ${title}\n")))))
;; go to insert mode after for org-capture cursor
;; (add-hook 'org-capture-mode-hook 'evil-insert-state)

;; side tree
(use-package treemacs
  :config
  (treemacs-resize-icons 15)
  (setq treemacs-width 30)
  (treemacs-follow-mode -1))

;; makes binding keys less painful
(use-package general)

;; needed for evil mode
(use-package undo-fu
  :config
  ;;(global-unset-key (kbd "C-z"))
  ;;(global-set-key (kbd "C-z") 'undo-fu-only-undo)
  ;;(global-set-key (kbd "C-S-z") 'undo-fu-only-redo)
  )

;; vertical completion interface
;; (use-package counsel
;;   :config
;;   (ivy-mode)
;;   (setq ivy-height 20)
;;   (global-set-key (kbd "M-x") 'counsel-M-x)
;;   (global-set-key (kbd "C-x C-f") 'counsel-find-file))

;; evil-mode
(when enable-evil
  (use-package evil
    :config
    (evil-mode 1)
    (evil-set-initial-state 'image-dired-thumbnail-mode 'emacs)
    ;; undo/redo keys
    (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
    (define-key evil-normal-state-map "r" 'undo-fu-only-redo)
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

  ;; search for the current visual selection with *
  (use-package evil-visualstar
    :config
    (global-evil-visualstar-mode))

  ;; support to make evil more compatible with the whole of emacs
  (use-package evil-collection
    :after (evil)
    :config
    (evil-collection-init))

  ;; ;; display visual hints for evil actions
  ;; (use-package evil-goggles
  ;;   :config
  ;;   (evil-goggles-mode))

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
    (general-define-key :states '(normal motion treemacs) :keymaps 'override "SPC t" 'treemacs))

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

  ;; disable evil mode in deadgrep, they dont work well together
  (evil-set-initial-state 'deadgrep-mode 'emacs)
  (evil-set-initial-state 'calc-mode 'emacs)
  ;; (evil-set-initial-state 'sldb-mode 'emacs) ;; for slime
  (evil-set-initial-state 'sly-db-mode 'emacs)
  ;; (evil-set-initial-state 'sly-inspector-mode 'emacs)
  (evil-set-initial-state 'vterm-mode 'emacs)

  (general-define-key :keymaps 'override "{" 'evil-scroll-line-up)
  (general-define-key :keymaps 'override "}" 'evil-scroll-line-down)

  (general-define-key :keymaps 'prog-mode-map "K" 'evil-jump-to-tag)

  (general-define-key :keymaps 'override ")" 'evil-scroll-page-down)
  (general-define-key :keymaps 'override "(" 'evil-scroll-page-up)

  (general-define-key :keymaps 'override (led "w") 'evil-window-map)

  ;; display number of matches when searching
  ;; (use-package anzu)
  (use-package evil-anzu)

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
  (define-and-bind-quoted-text-object "buffer" "A" "\\`\\s-*" "\\s-*\\'")

  (general-evil-setup)

  (general-define-key :states 'normal "s" 'save-buffer)

  (general-define-key :states 'normal :keymaps 'pdf-view-mode-map "d" 'pdf-view-scroll-up-or-next-page)
  (general-define-key :states 'normal :keymaps 'pdf-view-mode-map "u" 'pdf-view-scroll-down-or-previous-page)
  (general-define-key :states 'normal :keymaps 'pdf-view-mode-map "K" 'pdf-view-enlarge)
  (general-define-key :states 'normal :keymaps 'pdf-view-mode-map "J" 'pdf-view-shrink)
  ;; (general-define-key :states 'normal :keymaps 'dired-mode-map "l" 'dired-find-file)
  ;; (general-define-key :states 'normal :keymaps 'dired-mode-map "h" 'dired-up-directory)

  ;; dired
  (general-define-key :states 'normal :keymaps 'dired-mode-map "y" #'copy-file-path)

  ;; some sly keys
  ;; (general-define-key :states '(normal motion) :keymaps 'sly-repl-mode-map "K" 'sly-describe-symbol)

  (general-define-key :keymaps 'org-mode-map "]k" 'org-babel-next-src-block)
  (general-define-key :keymaps 'org-mode-map "[k" 'org-babel-previous-src-block)
  (general-define-key :keymaps 'org-mode-map "]o" 'org-next-block)
  (general-define-key :keymaps 'org-mode-map "[o" 'org-previous-block)


  ;; general keys for programming
  (general-define-key :states '(normal) :keymaps 'prog-mode-map "] r" 'next-error)
  (general-define-key :states '(normal) :keymaps 'prog-mode-map "[ r" 'previous-error)


  ;; evil mode multiple cursors
  (use-package evil-mc
    :config
    (global-evil-mc-mode)
    (general-define-key :states 'normal :keymaps 'override "g . p" 'evil-mc-pause-cursors)
    (general-define-key :states 'normal :keymaps 'override "g . r" 'evil-mc-resume-cursors))

  ;; (use-package evil-escape)
  ;; (evil-escape-mode)

  ;; interpret function arguments as a text object
  (use-package evil-args)
  (use-package evil-lion)

  (use-package evil-extra-operator)

  ;; (use-package evil-textobj-tree-sitter
  ;;   :config
  ;;   ;; bind `function.outer`(entire function block) to `f` for use in things like `vaf`, `yaf`
  ;;   (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
  ;;   ;; bind `function.inner`(function block without name and args) to `f` for use in things like `vif`, `yif`
  ;;   (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))
  ;;   ;; You can also bind multiple items and we will match the first one we can find
  ;;   (define-key evil-outer-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer")))
  ;;   (define-key evil-inner-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj ("conditional.inner" "loop.inner")))
  ;;   ;; Goto start of next function
  ;;   (define-key evil-normal-state-map (kbd "]f")
  ;;               (lambda ()
  ;;                 (interactive)
  ;;                 (evil-textobj-tree-sitter-goto-textobj "function.outer")))
  ;;   ;; Goto start of previous function
  ;;   (define-key evil-normal-state-map (kbd "[f")
  ;;               (lambda ()
  ;;                 (interactive)
  ;;                 (evil-textobj-tree-sitter-goto-textobj "function.outer" t)))
  ;;   ;; Goto end of next function
  ;;   (define-key evil-normal-state-map (kbd "]F")
  ;;               (lambda ()
  ;;                 (interactive)
  ;;                 (evil-textobj-tree-sitter-goto-textobj "function.outer" nil t)))
  ;;   ;; Goto end of previous function
  ;;   (define-key evil-normal-state-map (kbd "[F")
  ;;               (lambda ()
  ;;                 (interactive)
  ;;                 (evil-textobj-tree-sitter-goto-textobj "function.outer" t t)))
  ;;   )

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

  ;; (use-package evil-snipe
  ;;   :config
  ;;   (evil-snipe-override-mode 1)
  ;;   (general-define-key :states '(normal motion) :keymaps 'override "SPC ;" 'evil-snipe-s))

  ;; so that forward-sexp works at end of line, see https://github.com/Fuco1/smartparens/issues/1037
  ;; (setq evil-move-beyond-eol t)

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
    ;; (use-package ryo-modal
    ;;   :quelpa (ryo-modal :fetcher github :repo "Kungsgeten/ryo-modal")
    ;;   :commands ryo-modal-mode
    ;;   :bind ("C-c SPC" . ryo-modal-mode)
    ;;   :config
    ;;   (ryo-modal-keys
    ;;    ("," ryo-modal-repeat)
    ;;    ("q" ryo-modal-mode)
    ;;    ("h" backward-char)
    ;;    ("j" next-line)
    ;;    ("k" previous-line)
    ;;    ("l" forward-char))

    ;;   (ryo-modal-keys
    ;;    ;; First argument to ryo-modal-keys may be a list of keywords.
    ;;    ;; These keywords will be applied to all keybindings.
    ;;    (:norepeat t)
    ;;    ("0" "M-0")
    ;;    ("1" "M-1")
    ;;    ("2" "M-2")
    ;;    ("3" "M-3")
    ;;    ("4" "M-4")
    ;;    ("5" "M-5")
    ;;    ("6" "M-6")
    ;;    ("7" "M-7")
    ;;    ("8" "M-8")
    ;;    ("9" "M-9"))

    ;;   (defun mark-line ()
    ;;     (interactive)
    ;;     (beginning-of-line)
    ;;     (set-mark-command nil)
    ;;     (end-of-line))

    ;;   (ryo-modal-key
    ;;    "g" '(("s" save-buffer)
    ;;          ("g" magit-status)
    ;;          ("b" ibuffer-list-buffers)))

    ;;   (let ((text-objects
    ;;          '(("w" mark-word :name "Word")
    ;;            ("l" mark-line :name "line")
    ;;            )))
    ;;     (eval `(ryo-modal-keys
    ;;             ("v" ,text-objects)
    ;;             ;;("k" ,text-objects :then '(kill-region))
    ;;             ("c" ,text-objects :then '(kill-region)))))
    ;;   )
    ))

;; keybindings here
(general-define-key :keymaps 'override (led "d w") (lambda () (interactive) (dired "~/dl/")))
(general-define-key :keymaps 'override (led "d a") (lambda () (interactive) (dired "~/data/")))
(general-define-key :keymaps 'override (led "d l") (lambda () (interactive) (dired (get-latex-cache-dir-path))))
(general-define-key :keymaps 'override (led "d b") (lambda () (interactive) (dired brain-path)))
(general-define-key :keymaps 'override (led "d r") (lambda () (interactive) (dired (concat brain-path "/resources"))))
(general-define-key :keymaps 'override (led "d h") (lambda () (interactive) (dired "~/")))
(general-define-key :keymaps 'override (led "d p") (lambda () (interactive) (dired "~/p/")))
(general-define-key :keymaps 'override (led "d d") 'dired)
(general-define-key :keymaps 'override (led "d c") (lambda () (interactive) (dired default-directory)))
(general-define-key :keymaps 'override (led "d o") (lambda () (interactive) (dired "~/brain/out/")))
(general-define-key :keymaps 'override (led "d g") (lambda () (interactive) (dired "~/workspace/blog/")))
(general-define-key :keymaps 'override (led "d m") (lambda () (interactive) (dired *music-dir*)))
(general-define-key :keymaps 'override (led "f f") 'find-file)
(general-define-key :keymaps 'override (led "f s") 'sudo-find-file)
(general-define-key :keymaps 'override (led "ESC") #'execute-extended-command)
(general-define-key :keymaps 'override (led "b k") 'kill-this-buffer)
(general-define-key :keymaps 'eshell-mode-map (led "b k") (lambda () (interactive) (run-this-in-eshell "exit"))) ;; if we manually kill the buffer it doesnt save eshell command history
(general-define-key :keymaps 'sly-repl-mode (led "b k") 'sly-quit-lisp) ;; if we manually kill the buffer it doesnt save eshell command history
(general-define-key :keymaps 'override (led "b K") 'kill-buffer-and-window)
(general-define-key :keymaps 'override (led "b a")
                    (lambda ()
                      (interactive)
                      (kill-all-buffers)
                      (switch-to-buffer "*scratch*")))
(general-define-key :keymaps 'override (led "b s") 'consult-buffer)
(general-define-key :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map) (led "x") 'eval-defun)
(general-define-key :keymaps 'override (led "s g") 'deadgrep)
(general-define-key :keymaps 'org-mode-map (led "x") 'org-ctrl-c-ctrl-c) ;;'space-x-with-latex-header-hack)
(general-define-key :keymaps 'TeX-mode-map (led "x") 'compile-current-document)
(general-define-key :keymaps 'override (led "e") (lambda () (interactive) (find-file user-init-file)))
(general-define-key :keymaps 'override (led "p") 'projectile-command-map)
;; (general-define-key :keymaps 'TeX-mode-map (led "c" 'compile-sagetex)
(general-define-key :keymaps 'org-mode-map (led "r k") 'org-insert-link)
;; (general-define-key :keymaps 'org-mode-map (led "z"
;;                     (lambda ()
;;                       (interactive)
;;                       (if (not xenops-mode)
;;                           (xenops-mode)
;;                         (xenops-render))))
(general-define-key :keymaps 'org-mode-map (led "z") 'org-latex-preview)
(general-define-key :keymaps 'override (led "a i") #'org-timestamp)
(general-define-key :keymaps 'override (led "a I")
                    (lambda ()
                      (interactive)
                      (org-insert-time-stamp (current-time) t))) ;; timestamp with full time
;;(define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)
;; (general-define-key :keymaps 'override (led "r t" 'org-roam-buffer-toggle)
(general-define-key :keymaps 'override (led "r f")
                    (lambda ()
                      (interactive)
                      (org-roam-node-find
                       nil
                       nil
                       (lambda (my-roam-node)
                         (and my-roam-node
                              (not (equal (org-roam-node-title my-roam-node) "")))))))
(general-define-key :keymaps 'override (led "r i") 'org-roam-node-insert)
(general-define-key :keymaps 'override (led "r c") 'org-id-get-create)
(general-define-key :keymaps 'org-mode-map (led "r o") 'org-open-at-point)
(general-define-key :keymaps 'org-mode-map (led "r a") 'org-attach)
(general-define-key :keymaps 'org-mode-map (led "r A") 'org-attach-open)
(general-define-key :keymaps 'org-mode-map (led "r l") 'org-roam-alias-add)
(general-define-key :keymaps 'override (led "r n") (lambda () (interactive) (org-roam-capture nil "n")))
(general-define-key :keymaps 'org-mode-map (led "r w") 'org-roam-tag-add)
(general-define-key :keymaps 'org-mode-map (led "r W") 'org-roam-tag-remove)
(general-define-key :keymaps 'override (led "r q")
                    (lambda ()
                      (interactive)
                      (org-roam-capture-no-title-prompt nil "k"))) ;; spc r q - quick note
;; (general-define-key :keymaps 'override "SPC r h"
;;                     (lambda ()
;;                       (interactive)
;;                       (org-roam-capture nil "t")))
(general-define-key :keymaps 'org-mode-map (led "r x") #'export-current-buffer)
(general-define-key :keymaps 'org-mode-map (led "r X") #'export-all-public)
(general-define-key :keymaps 'org-mode-map (led "r u") #'org-latex-preview-clear-cache)
(general-define-key :keymaps 'org-mode-map (led "r e") 'org-babel-tangle)
(general-define-key :keymaps 'org-mode-map (led "r E") 'org-babel-tangle-file)
;; (general-define-key :keymaps 'org-mode-map "SPC r d" 'org-deadline)
;; (general-define-key :keymaps 'org-mode-map "SPC r s" 'org-schedule)
(general-define-key :keymaps 'override (led "r g")
                    (lambda ()
                      (interactive)
                      (find-file (concat brain-path "/bib.bib"))))
(general-define-key :keymaps 'org-mode-map (led "r v") 'org-babel-execute-buffer)
(general-define-key :keymaps 'org-mode-map (led "r r") 'org-redisplay-inline-images)
(general-define-key :keymaps 'org-mode-map (led "r P") 'org-set-property)
(general-define-key :keymaps 'org-mode-map (led "r z") 'org-add-note)
(general-define-key :keymaps 'org-mode-map (led "r p") 'org-latex-preview-auto-mode)
;; (general-define-key :keymaps 'override "/" 'swiper)
(general-define-key :keymaps 'org-mode-map (led "c") "C-c C-c")
(general-define-key :keymaps 'override (led "a N")
                    (lambda ()
                      (interactive)
                      (org-roam-capture-no-title-prompt nil "d")))

;; keys to search for files
(general-define-key :keymaps 'override (led "f b")
                    (lambda () (interactive) (search-open-file brain-path ".*\\(pdf\\|tex\\|doc\\|mp4\\|png\\|org\\)")))
(general-define-key :keymaps 'override (led "F b")
                    (lambda () (interactive) (search-open-file-in-emacs brain-path ".*\\(pdf\\|tex\\|doc\\|org\\)")))
(general-define-key :keymaps 'override (led "f h") (lambda () (interactive) (search-open-file "./" ".*")))
(general-define-key :keymaps 'override (led "f m")
                    (lambda () (interactive)
                      (let ((my-file (completing-read "select file: " (cl-remove-if (lambda (filepath)
                                                                                      (string-match "\\.\\(spotdl\\|lrc\\|jpg\\|json\\)$" filepath))
                                                                                    (directory-files-recursively *music-dir* "")))))
                        (browse-url (expand-file-name my-file)))))

;; (define-key evil-normal-state-map (kbd "SPC f d")
;;             (lambda () (interactive) (search-open-file "~/data" "")))
;; (define-key evil-normal-state-map (kbd "SPC F d")
;;             (lambda () (interactive)
;;               (search-open-file-in-emacs "~/data" "")))

;; music keys
;; play artist
(general-define-key :keymaps 'override (led "m a")
                    (lambda ()
                      (interactive)
                      (let ((artist-names (mapcar #'file-name-nondirectory (cl-remove-if-not #'file-directory-p (directory-files *music-dir* t)))))
                        (let ((chosen-artist (completing-read "pick artist: " artist-names)))
                          (dired (format "%s/%s" *music-dir* chosen-artist))))))
;; play album by artist name + album name
(general-define-key :keymaps 'override (led "m b")
                    (lambda ()
                      (interactive)
                      (let ((album-titles
                             (apply
                              #'cl-concatenate
                              (list*
                               'list
                               (mapcar
                                (lambda (dir)
                                  (mapcar (lambda (album-title)
                                            (concat (file-name-nondirectory dir) "/" album-title))
                                          (directory-files dir nil "^[^.].*$")))
                                (cl-remove-if-not
                                 #'file-directory-p
                                 (directory-files *music-dir* t "^[^.].*$")))))))
                        (let ((chosen-album (completing-read "pick album: " album-titles)))
                          (message (concat *music-dir* chosen-album))
                          (call-process "play_dir_as_album.sh" nil 0 nil
                                        (concat *music-dir* chosen-album))
                          (message "playing album %s" chosen-album)))))
;; play album
(general-define-key :keymaps 'override (led "m B")
                    (lambda ()
                      (interactive)
                      (let ((album-titles
                             (apply
                              #'cl-concatenate
                              (list*
                               'list
                               (mapcar
                                (lambda (dir)
                                  (directory-files dir nil "^[^.].*$"))
                                (cl-remove-if-not
                                 #'file-directory-p
                                 (directory-files *music-dir* t "^[^.].*$")))))))
                        (let ((chosen-album (completing-read "pick album: " album-titles)))
                          (call-process "play_dir_as_album.sh" nil 0 nil
                                        (cl-find-if (lambda (filepath) (string-match (format ".*/%s$" chosen-album) filepath)) (directory-files-recursively *music-dir* "" t)))
                          (message "playing album %s" chosen-album)))))
;; open music table file
(general-define-key :keymaps 'override (led "m f")
                    (lambda ()
                      (interactive)
                      (find-file "/home/mahmooz/brain/notes/20231010211129-music_table.org")))
;; open artist's last.fm page
(general-define-key :keymaps 'override (led "m l")
                    (lambda ()
                      (interactive)
                      (let ((artist-names (mapcar #'file-name-nondirectory (cl-remove-if-not #'file-directory-p (directory-files *music-dir* t)))))
                        (let ((chosen-artist (completing-read "pick artist: "
                                                              artist-names
                                                              nil
                                                              nil
                                                              (current-mpv-artist))))
                          (browse-url (format "https://www.last.fm/music/%s" chosen-artist))))))

;; keybinding to evaluate math expressions
;; (general-define-key :states '(normal motion) :keymaps 'override "SPC m"
;;                     (lambda ()
;;                       (interactive)
;;                       (call-interactively 'calc-latex-language)
;;                       (let ((result (calc-eval (buffer-substring-no-properties (region-beginning) (region-end)))))
;;                         (end-of-line)
;;                         (insert " ")
;;                         (insert result))))

;; general keys
;; (general-define-key :states 'normal :keymaps 'override "SPC m" 'man)
(general-define-key :keymaps 'override (led "'") (general-simulate-key "C-c '"))
(general-define-key :keymaps 'override (led "w m")
                    (lambda () (interactive)
                      (when window-system (set-frame-size (selected-frame) 180 50))))
(general-define-key :keymaps 'override (led "s d") 'switch-to-dark-theme)
(general-define-key :keymaps 'override (led "s l") 'switch-to-light-theme)
(general-define-key :keymaps 'override (led "s e") 'eshell)
;; (general-define-key :keymaps 'override (led "s g") 'magit)
(general-define-key :keymaps 'override (led "s i")
                    (lambda ()
                      (interactive)
                      (let ((current-prefix-arg '-)) (call-interactively 'sly))))
(general-define-key :keymaps 'override (led "s r") #'eat);;'vterm)
(general-define-key :keymaps 'override (led "u") (general-simulate-key "C-u"))
(general-define-key :keymaps 'override (led "o l") 'avy-goto-line)
(general-define-key :keymaps 'override (led "o c") 'avy-goto-char)
(general-define-key :keymaps 'override (led "s s") 'spotify-lyrics)
(general-define-key :keymaps 'override (led "s w") 'open-spotify-lyrics-file)
(general-define-key :keymaps 'override (led "s t") #'consult-theme)
(general-define-key :keymaps 'override (led "s k") 'open-kitty-here)
;; (general-define-key :keymaps 'override "SPC l" 'calc)

;; agenda keys
(general-define-key :keymaps 'override (led "a a") (lambda () (interactive) (org-agenda nil "n")))
(general-define-key :keymaps 'org-agenda-mode-map "q" 'org-agenda-exit)
(general-define-key :keymaps 'override (led "a s") 'org-schedule)
(general-define-key :keymaps 'override (led "a d") 'org-deadline)
(general-define-key :keymaps 'org-mode-map (led "a j") 'org-clock-in)
(general-define-key :keymaps 'override (led "a J") 'org-clock-in-last)
(general-define-key :keymaps 'override (led "a k") 'org-clock-out)
(general-define-key :keymaps 'override (led "a b") 'org-clock-cancel)
(general-define-key :keymaps 'org-mode-map (led "a p") 'org-clock-display)
(general-define-key :keymaps 'override (led "a t") (lambda () (interactive) (org-roam-capture nil "t")))
(general-define-key :keymaps 'override (led "a n") 'today-entry)
(general-define-key :keymaps 'override (led "a o") 'open-todays-file)
(general-define-key :keymaps 'override (led "s n") 'yas-new-snippet)
(general-define-key :keymaps 'override (led "s v") 'yas-visit-snippet-file)
(general-define-key :keymaps 'override (led "s h") 'yas-insert-snippet)
(general-define-key :keymaps 'override (led "s a") 'dictionary-search)
(general-define-key :keymaps 'org-mode-map (led "r s") 'org-cite-insert)
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
            (general-define-key :keymaps 'local (led "c") (lambda () (interactive) (run-this-in-eshell "clear 1")))))
(general-define-key :keymaps 'eshell-mode-map (led "x") 'eshell-interrupt-process)
(general-define-key :keymaps 'lisp-mode-map (led "x") 'sly-compile-defun)
(general-define-key :keymaps 'lisp-mode-map (led "c") 'sly-eval-buffer)
(general-define-key :keymaps 'lisp-mode-map (led "z")
                    (lambda ()
                      (interactive)
                      (sly-end-of-defun)
                      (call-interactively 'sly-eval-last-expression-in-repl)))
(general-define-key :keymaps 'override (led "s e") 'eshell)
(general-define-key :keymaps 'override (led "s m") 'man)

;; language-specific keybindings
(general-define-key :keymaps 'lisp-mode-map (led "l i") 'sly-repl-inspect)
(general-define-key :keymaps 'sly-repl-mode-map (led "l i") 'sly-repl-inspect)
(general-define-key :keymaps 'sly-repl-mode-map (led "l s") 'sly-inspect-presentation-at-point)
(general-define-key :keymaps 'emacs-lisp-mode-map (led "c") 'eval-buffer)

;; common lisp/sly
(general-define-key :keymaps '(lisp-mode-map sly-mrepl-mode-map) (led "l d") 'sly-documentation-lookup)

;; julia
(general-define-key :keymaps 'override (led "s j") 'julia-snail)

;; python
(general-define-key :keymaps 'override (led "s p") 'run-python)
(general-define-key :keymaps 'python-mode-map (led "x") 'python-shell-send-defun)
(general-define-key :keymaps 'python-mode-map (led "l) x") 'python-shell-send-defun)
(general-define-key :keymaps 'python-mode-map (led "l t") 'python-shell-send-statement)
(general-define-key :keymaps 'python-mode-map (led "c") 'python-shell-send-buffer)
(general-define-key :keymaps 'python-mode-map (led "l b") 'python-shell-send-buffer)

;;sagemath
(general-define-key :keymaps 'sage-shell-mode-map (led "b k") 'comint-quit-subjob)

;; elisp
(general-define-key :keymaps 'emacs-lisp-mode-map (led "c") 'eval-buffer)

;; other keybinds
(general-define-key :keymaps 'override "C-S-k" 'kill-whole-line)
(general-define-key :keymaps 'override (led "h") (general-simulate-key "C-h"))
;; (general-define-key :keymaps 'override "M-RET"
;;                     (lambda ()
;;                       (interactive)
;;                       (end-of-line)
;;                       (call-interactively 'newline))) ;; call newline interactively for proper indentation in code
(general-define-key :keymaps 'override (led "w v") #'split-window-right)
(general-define-key :keymaps 'override (led "w s") #'split-window-below)
(general-define-key :keymaps 'override (led "w o") #'other-window)
(general-define-key :keymaps 'override (led "w c") #'delete-window)
(general-define-key :keymaps 'override (led "w t") #'recenter)
(general-define-key :keymaps '(org-mode-map TeX-mode-map) (led "v") #'open-current-document-this-window)
(keymap-global-set "M-o" ;; new line without breaking current line
                   (lambda ()
                     (interactive)
                     (end-of-line)
                     (newline-and-indent)))
(keymap-global-set "M-O" ;;"M-S-o" ;; new line above current line without breaking it
                   (lambda ()
                     (interactive)
                     (move-beginning-of-line nil)
                     (newline-and-indent)
                     (forward-line -1)
                     (indent-according-to-mode)))
;; (global-set-key (kbd "M-S-o"))
(general-define-key :keymaps 'override "M-z" #'zap-up-to-char)

;; i hate backspace and return
(define-key input-decode-map [?\C-m] [C-m]) ;; so that C-m wouldnt be attached to <return> anymore
(global-set-key (kbd "<C-m>") #'newline)
;; (keymap-global-set (kbd "<C-m>") #'newline) ;; not sure why this doesnt work like above
(keymap-global-unset "<RET>")
(keymap-global-unset "<backspace>")
(keymap-global-set "C-S-d" #'backward-delete-char-untabify)
(keymap-global-set "M-D" #'backward-kill-word)
(keymap-global-set "C-M-S-k" #'backward-kill-sexp)
(keymap-global-set "C-c c" #'recenter)
;; (keymap-global-set "C-'" #'save-buffer)
(general-define-key :keymaps 'override "C-'" #'save-buffer)

;; C-j in repl's to emulate RETURN
(define-key comint-mode-map (kbd "C-j") #'comint-send-input)
;; SPC g to cancel like C-g
(define-key key-translation-map (kbd (led "g")) (kbd "C-g"))

;; evaluate and insert without truncating output
(general-define-key
 "C-x C-S-e"
 (lambda ()
   (interactive)
   (let ((current-prefix-arg (list 0)))
     (call-interactively 'eros-eval-last-sexp))))

(keymap-global-set (led "t") #'treemacs)
;; (keymap-global-set "C-a" #'back-to-indentation)
;; (keymap-global-set "M-m" #'beginning-of-line)

(defun org-roam-capture-no-title-prompt (&optional goto keys &key filter-fn templates info)
  (interactive "P")
  (org-roam-capture- :goto goto
                     :info info
                     :keys keys
                     :templates templates
                     :node (org-roam-node-create :title "")
                     :props '(:immediate-finish nil)))

;; projectile
(use-package projectile
  :quelpa (:host github :repo "bbatsov/projectile")
  :config
  ;; (setq projectile-completion-system 'ivy)
  (projectile-mode +1))

;; auto completion
;; im sticking with company for now as corfu keeps crashing with org mode, plus slime doesnt work with corfu (for now)
(setq enable-company nil)
(setq completion-ignore-case t) ;; case-insensitivity
(if enable-company
    (progn
      (use-package company
        :quelpa (:host github :repo "company-mode/company-mode")
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
             (define-key company-active-map (kbd "TAB") 'company-complete-selection)
             (define-key company-active-map [tab] 'company-complete-selection)
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
      :custom
      (corfu-cycle t)
      (corfu-auto t) ;; i feel like this gets in the way so i wanna disable it
      (corfu-quit-no-match t)
      (corfu-auto-delay 0)
      ;; (corfu-separator ?_) ;; Set to orderless separator, if not using space
      ;; (corfu-separator " ") ;; Set to orderless separator, if not using space
      (corfu-count 10)
      (corfu-indexed-mode t)
      (corfu-echo-mode t) ;; display brief documentation in echo area
      (corfu-popupinfo-mode t) ;; display documentation in popup
      (corfu-quit-at-boundary nil)
      (corfu-on-exact-match nil) ;; dont auto insert when there is an exact match
      (corfu-popupinfo-delay (cons 0 0)) ;; dont auto insert when there is an exact match
      :config
      (unbind-key "RET" corfu-map)
      ;; (define-key corfu-map "\M-q" #'corfu-quick-complete)
      ;; (define-key corfu-map "\M-q" #'corfu-quick-insert)
      )

    (use-package kind-icon
      :ensure t
      :after corfu
      :custom
      (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
      (kind-icon-use-icons nil) ;; use text-based icons
      :config
      (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

    (use-package cape)

    ;; (use-package orderless
    ;;   :init
    ;;   ;; Configure a custom style dispatcher (see the Consult wiki)
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
      "Enable Corfu in the minibuffer if `completion-at-point' is bound."
      (when (where-is-internal #'completion-at-point (list (current-local-map)))
        ;; (setq-local corfu-auto nil) Enable/disable auto completion
        (corfu-mode 1)))
    (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)

    (use-package pcmpl-args)
    )
  )

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
;; (set-face-attribute 'default nil :family "Iosevka" :height 130)
;; (set-face-attribute 'default nil :family "Monaco" :height 120)
(ignore-errors
  (set-face-attribute 'default nil :font "Fira Code" :weight 'light :height 100)
  (set-face-attribute 'fixed-pitch nil :font "Fira Code" :weight 'light :height 100)
  (set-face-attribute 'variable-pitch nil :font "Fira Code":weight 'light :height 1.0)
  )
;; this font makes hebrew text unreadable, gotta disable it
(add-to-list 'face-ignored-fonts "Noto Rashi Hebrew")
(use-package darktooth-theme)
;; (use-package modus-themes)
(use-package ample-theme)
(use-package anti-zenburn-theme)
(use-package zenburn-theme)
(use-package poet-theme)
;; (use-package gruvbox-theme)
(use-package doom-themes)
(use-package inkpot-theme)
(use-package minimal-theme
  :quelpa (:host github :repo "mahmoodsheikh36/minimal-theme"))
(use-package soothe-theme)
;; (switch-to-dark-theme)
;; (switch-to-light-theme)
;; (load-theme 'minimal-light t)
;; (load-theme 'doom-gruvbox-light t)
;; (load-theme 'darktooth t)
;; (load-theme 'ample-flat t)
;; (modus-themes-load-operandi)
;; stop org src blocks from bleeding in doom themes (remove background)
;; (set-face-attribute 'org-block-end-line nil :background nil)
;; (set-face-attribute 'org-block-begin-line nil :background nil)

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
  ;; Ob-sagemath supports only evaluating with a session.
  (setq org-babel-default-header-args:sage '((:session . t)
                                             (:results . "drawer")))
  (setq sage-shell:input-history-cache-file (concat brain-path "/sage_history"))
  (add-hook 'sage-shell-after-prompt-hook #'sage-shell-view-mode))

;; better built-in help/documentation
(use-package helpful
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h a") #'helpful-symbol)
  (global-set-key (kbd "C-h k") #'helpful-key))

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
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

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
  (setq org-hugo-base-dir (file-truename "~/workspace/blog/"))
  (setq org-hugo-section "post")
  (setq org-more-dir (expand-file-name "~/workspace/blog/static/more/"))
  (ignore-errors (make-directory org-more-dir))
  (defconst *org-static-dir* (file-truename "~/workspace/blog/static/ox-hugo"))
  (add-to-list 'org-hugo-external-file-extensions-allowed-for-copying "webp")
  (add-to-list 'org-hugo-external-file-extensions-allowed-for-copying "html"))

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

;; (use-package vulpea)
;; (use-package dap-mode)

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
(use-package org-roam-ui)
;; (use-package jupyter)
;; (use-package plantuml-mode)
;; (use-package org-ref
;;   :config
;;   (setq bibtex-completion-bibliography '("~/brain/bib.bib"))
;;   (define-key org-mode-map (kbd "C-c ]") 'org-ref-insert-link-hydra/body))
;; (use-package code-compass)

;; best terminal emulation, required for julia-snail
;; (use-package vterm)

;; check which keys i press most
(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  (setq keyfreq-file (concat brain-path "emacs_keyfreq")))

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
;; (use-package avy)
;; (use-package auto-yasnippet)

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ;;("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
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

(quelpa '(eat :fetcher git
              :url "https://codeberg.org/akib/emacs-eat"
              :files ("*.el" ("term" "term/*.el") "*.texi"
                      "*.ti" ("terminfo/e" "terminfo/e/*")
                      ("terminfo/65" "terminfo/65/*")
                      ("integration" "integration/*")
                      (:exclude ".dir-locals.el" "*-tests.el"))))

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
(quelpa '(org-krita :fetcher github :repo "lepisma/org-krita" :files ("*.el" "resources")))
;; (add-hook 'org-mode-hook 'org-krita-mode)

;; like org-krita, crashes, unusable...
;; (quelpa '(org-xournalpp :fetcher gitlab :repo "vherrmann/org-xournalpp" :files ("*.el" "resources")))
;; (add-hook 'org-mode-hook 'org-xournalpp-mode)

;; perfectly aligned org mode tables
;; (use-package valign
;;   :hook
;;   (org-mode . valign-mode))

(use-package hydra
  :config
  (defhydra hydra-agenda (global-map "C-c a")
    "hydra-agenda"
    ("a" org-agenda-list "list")
    ("n" today-entry "entry for today")
    ("o" open-todays-file "open today's file")
    ("d" org-deadline "deadline")
    ("s" org-schedule "schedule"))
  (defhydra hydra-roam (global-map "C-c r")
    "hydra-roam"
    ("f" org-roam-node-find "find roam node")
    ("n" (lambda () (interactive) (org-roam-capture nil "n")) "create roam node")
    ))

(use-package vimrc-mode)

(use-package sly
  :quelpa (:host github :repo "joaotavora/sly")
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
  (setq sly-mrepl-history-file-name (concat brain-path "/sly_history"))
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
;;   (setq slime-repl-history-file (concat brain-path "/slime_history"))
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

;; emacs "workspaces"
(use-package perspective
  :init
  (persp-mode)
  :config
  (general-define-key :keymaps 'override (led "s c") 'persp-switch))
;; (add-hook 'kill-emacs-hook #'persp-state-save)) ;; need to provide a file for this to work tho

;; center buffer
(use-package olivetti)

;; depth-dependent coloring of code
(use-package prism
  :quelpa (prism :fetcher github :repo "alphapapa/prism.el"))

;; its great but it uses alot of cpu especially when the gif has a fast timer
(use-package org-inline-anim
  :config
  (add-hook 'org-mode-hook #'org-inline-anim-mode)
  ;; (add-hook 'org-mode-hook #'org-inline-anim-animate-all)
  (setq org-inline-anim-loop t)
  (add-hook 'org-babel-after-execute-hook 'org-inline-anim-animate))

(use-package julia-snail
  :hook (julia-mode . julia-snail-mode)
  :custom
  (julia-snail-extensions '(repl-history formatter ob-julia))
  (julia-snail/ob-julia-mirror-output-in-repl t)
  (julia-snail-terminal-type :eat))

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
;; Commands for Ido-like directory navigation.
;; Configure directory extension.
(use-package vertico-directory
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
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))
(use-package consult)
;; corfu with orderless
(defun orderless-fast-dispatch (word index total)
  (and (= index 0) (= total 1) (length< word 4)
       `(orderless-regexp . ,(concat "^" (regexp-quote word)))))
(orderless-define-completion-style orderless-fast
  (orderless-style-dispatchers '(orderless-fast-dispatch))
  (orderless-matching-styles '(orderless-literal orderless-regexp orderless-flex)))
(setq completion-styles '(substring orderless-fast basic))

(use-package all-the-icons-completion
  :config
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup))

;; prescient history location
;; (use-package prescient
;;   :config
;;   (prescient-persist-mode 1)
;;   (setq prescient-history-length 100000)
;;   (setq prescient-save-file (file-truename (concat brain-path "emacs_prescient")))) ;; save history to filesystem
;; (use-package vertico-prescient
;;   :config
;;   (vertico-prescient-mode))

;; virtual env integration for python
(use-package pyvenv)

(use-package multiple-cursors
  :config
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines))

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

;; (use-package dired-rsync)

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
;;   (setq roam-block-home (list brain-path)
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
;;(use-package google-translate)
;;(use-package google-maps)

(defun spotify-lyrics ()
  (interactive)
  (let* ((song (string-trim (shell-command-to-string "osascript -e 'tell application \"Spotify\" to name of current track as string'")))
         (artist (string-trim (shell-command-to-string "osascript -e 'tell application \"Spotify\" to artist of current track as string'")))
         (song-file (format "%s/lyrics/%s - %s" brain-path song artist)))
    (if (not (file-exists-p song-file))
        (progn
          (message "lyrics file doesnt exist")
          (let ((lyrics (shell-command-to-string (format "~/scripts/get_genius_lyrics.py '%s' '%s' 2>/dev/null" song artist))))
            (if (> (length lyrics) 0)
                (progn
                  (f-write-text lyrics 'utf-8 song-file)
                  (message "fetched lyrics for: %s - %s" song artist)
                  (find-file song-file))
              (message "couldnt fetch lyrics :("))))
      (find-file song-file))))

;; these spotify functions are old af, they were for when i had a mac
(defun delete-spotify-lyrics-file ()
  (interactive)
  (let* ((song (string-trim (shell-command-to-string "osascript -e 'tell application \"Spotify\" to name of current track as string'")))
         (artist (string-trim (shell-command-to-string "osascript -e 'tell application \"Spotify\" to artist of current track as string'")))
         (song-file (format "%s/lyrics/%s - %s" brain-path song artist)))
    (delete-file song-file)))

(defun open-spotify-lyrics-file ()
  (interactive)
  (let* ((song (string-trim (shell-command-to-string "osascript -e 'tell application \"Spotify\" to name of current track as string'")))
         (artist (string-trim (shell-command-to-string "osascript -e 'tell application \"Spotify\" to artist of current track as string'")))
         (song-file (format "%s/lyrics/%s - %s" brain-path song artist)))
    (find-file song-file)))

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
;; log state/schedule/deadline changes
(setq org-log-done 'time)
(setq org-log-reschedule 'time)
(setq org-log-redeadline 'time)
;; show images when opening a file.
(setq org-startup-with-inline-images nil)
;; show images after evaluating code blocks.
(add-hook 'org-babel-after-execute-hook (lambda ()
                                          (interactive)
                                          (clear-image-cache)
                                          ;; (org-latex-preview) ;; this makes everything super slow and even buggy
                                          (org-display-inline-images)))
;; render latex preview after evaluating code blocks, not needed anymore org detects changes by itself now
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
   (sql . t)
   (julia . t)
   (lua . t)))
;; make g++ compile with std=c++17 flag
(setq org-babel-C++-compiler "g++ -std=c++17")
;; make org babel default to python3
(setq org-babel-python-command "python3")
;; increase org table max lines
(setq org-table-convert-region-max-lines 10000)
;; to increase depth of the imenu in treemacs
(setq org-imenu-depth 4)
;; who cares about annoying broken link errors..
(setq org-export-with-broken-links 'mark)
;; thought org caching was the bottleneck for ox-hugo exports but it isnt, (wait, it apparently is.. but it isnt, as its just that a more recent version is the main cause)
;; these cause a delay when killing org buffers, disabling for now, disabling this also made rendering way faster
;; dont cache latex preview images
(setq org-latex-preview-persist nil)
(setq org-element-cache-persistent nil)
(setq org-element-use-cache nil)

(defun run-command-show-output (cmd)
  "run shell command and show continuous output in new buffer"
  (interactive)
  (progn
    (start-process-shell-command cmd cmd cmd)
    (display-buffer cmd)
    (end-of-buffer-other-window nil)))

;; hide unnecessary stuff
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
(setq dired-listing-switches "-Al") ;; default is ls -al
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
;; (define-key dired-mode-map (kbd "?") 'dired-get-size)
(general-define-key :states '(normal) :keymaps 'dired-mode-map "?" 'dired-get-size)

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
  "current filename without extension"
  (file-name-sans-extension
   (file-name-nondirectory (buffer-file-name))))

(defun get-latex-cache-dir-path ()
  "return the path for the directory that contains the compiled pdf latex documents"
  (interactive)
  (concat brain-path "/out/"))

(defun compile-latex-file (path)
  (start-process-shell-command "latex" "latex" (format "lualatex -shell-escape -output-directory=%s %s" (file-truename (get-latex-cache-dir-path)) path)))

(defun compile-current-document ()
  "compile the current latex document being edited"
  (interactive)
  (compile-latex-file (buffer-file-name)))

;; someone needed this so i whipped it up for them
;; (defun compile-this-latex-file (outfile)
;;   "compile the current latex document being edited"
;;   (interactive "sEnter output file: ")
;;   (message "outfile %s" outfile)
;;   (start-process-shell-command
;;    "latex"
;;    "latex"
;;    (format "pdflatex -shell-escape -jobname=%s %s"
;;            outfile (buffer-file-name))))

(defun open-current-document ()
  "open the pdf of the current latex document that was generated"
  (interactive)
  (find-file-other-window (concat (get-latex-cache-dir-path) (current-filename) ".pdf")))
(defun open-current-document-this-window ()
  (interactive)
  (let ((pdf-file (concat (get-latex-cache-dir-path) (current-filename) ".pdf")))
    (if (file-exists-p pdf-file)
        (find-file pdf-file)
      (message "pdf file hasnt been generated"))))

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
  (setq first-pdflatex-command (concat "(pdflatex --synctex=1 -output-directory=" (get-latex-cache-dir-path) " " (buffer-file-name) ";"))
  (setq last-pdflatex-command (concat (concat (concat "pdflatex --synctex=1 -output-directory=" (concat (get-latex-cache-dir-path) " ")) (buffer-file-name)) ")"))
  (concat first-pdflatex-command (concat (concat "(cd " (concat (get-latex-cache-dir-path) (concat "; sage " (concat (current-filename) ".sagetex.sage);")))) last-pdflatex-command)))
(defun compile-sagetex ()
  "compile the current latex document with support for sagetex"
  (interactive)
  (start-process-shell-command "latex" "latex" (compile-sagetex-command)))

;; dmenu like functions
(defun search-open-file (directory-path regex)
  "search for file and open it similar to dmenu"
  (interactive)
  (let ((my-file (completing-read "select file: " (directory-files-recursively directory-path regex t))))
    (browse-url (expand-file-name my-file))))

(defun search-open-file-in-emacs (directory-path regex)
  "search for a file recursively in a directory and open it in emacs"
  (let ((my-file (completing-read "select file: " (directory-files-recursively directory-path regex))))
    (find-file (expand-file-name my-file) "'")))

;; automatically run script being edited, demonstrates how we can auto compile files on save
;; (defun run-script ()
;;   "run the current bash script being edited"
;;   (interactive)
;;   (run-command-show-output (buffer-file-name)))
;; (add-hook 'sh-mode-hook
;;           (lambda ()
;;             (add-hook 'after-save-hook 'run-script 0 t)))

;; eshell configs
;; make the cursor stay at the prompt when scrolling
(setq eshell-scroll-to-bottom-on-input t)
;; file to store aliases automatically to
(setq eshell-aliases-file (concat brain-path "/eshell_aliases"))
(defun eshell-cd-and-ls (&rest args)           ; all but first ignored
  "cd into directory and list its contents"
  (interactive "P")
  (let ((path (car args)))
    (cd path)
    (eshell/ls)))
;; eshell history file location
(setq eshell-history-file-name (concat brain-path "/eshell_history")) ;; save history to filesystem
(setq eshell-history-size 100000000)

;; compile org docs to pdfs and put them in cache dir
(defun org-to-pdf ()
  (interactive)
  (let ((outfile (concat (file-truename (get-latex-cache-dir-path)) (current-filename) ".tex")))
    ;; (call-process-shell-command (format "rm %s*%s*" (file-truename (get-latex-cache-dir-path)) (current-filename)))
    (org-export-to-file 'latex outfile
      nil nil nil nil nil nil)
    (compile-latex-file outfile)))
;; make latex preview bigger
;; (plist-put org-format-latex-options :scale 1.5)
;; allow usage of #+BIND in latex exports
(setq org-export-allow-bind-keywords t)
;; decrease image size in latex exports
;; (setq org-latex-image-default-scale "0.6")
;; disable images from being scaled/their dimensions being changed
;; (setq org-latex-image-default-width "")
;; enable latex snippets in org mode
(defun my-org-latex-yas ()
  "Activate org and LaTeX yas expansion in org-mode buffers."
  (yas-minor-mode)
  (yas-activate-extra-mode 'latex-mode))
(add-hook 'org-mode-hook #'my-org-latex-yas)
;; preserve all line breaks when exporting
(setq org-export-preserve-breaks t)
;; indent headings properly
;; (add-hook 'org-mode-hook 'org-indent-mode)
(setq org-todo-keywords
      '((sequence
         "TODO(t!)"
         "GO(g@)";
         "WAIT(w@)"
         "REVIEW(r!)"
         "|" ; remaining close task
         "DONE(d@)"
         "CANCELED(c@)"
         "CANCELLED(C@)" ;; for backward compatibility
         )))
;; filter out entries with tag "ignore"
(setq org-agenda-tag-filter-preset '("-ignore"))

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
;; (require 'org-src)
;; make org-babel java act like other langs
(setq org-babel-default-header-args:java
      '((:dir . nil)
        (:results . "value")))
;; use unique id's to identify headers, better than using names cuz names could change
(setq org-id-link-to-org-use-id t)
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
;; dunno why \def\pgfsysdriver is needed (i think for htlatex)... gonna override the variable cuz that causes errors
(setq org-babel-latex-preamble
      (lambda (_)
        "\\documentclass[preview]{standalone}"))
;; to make gifs work
;; (setq org-format-latex-header (string-replace "{article}" "[tikz]{standalone}" org-format-latex-header))
;; (setq org-format-latex-header (string-replace "\\usepackage[usenames]{color}" "" org-format-latex-header))
;; (setq org-format-latex-header "\\documentclass[tikz]{standalone}")
;; i think this is irrelevant at this point
(defun space-x-with-latex-header-hack ()
  (interactive)
  (let ((org-format-latex-header "\\documentclass[tikz]{standalone}"))
    (org-ctrl-c-ctrl-c)))
;; make org babel use dvisvgm instead of inkscape for pdf->svg, way faster and has many more advtanges over inkscape
(setq org-babel-latex-pdf-svg-process "dvisvgm --pdf %f -o %O")
;; latex syntax highlighting in org mode (and more)
;; (setq org-highlight-latex-and-related '(latex))
(setq org-highlight-latex-and-related '(native latex script entities))
;; disable org-mode's mathjax because my blog's code uses another version
(setq org-html-mathjax-template "")
(setq org-html-mathjax-options '())
(setq org-babel-default-header-args:latex
      '((:results . "file graphics")
        ;; (:exports . "results")
        ;; (:fit . t)
        ;; (:imagemagick . t)
        ;; (:eval . "no-export")
        ;; (:headers . ("\\usepackage{\\string~/.emacs.d/common}"))
        ))
;; make org export deeply nested headlines as headlines still
(setq org-export-headline-levels 20)
;; workaround to make yasnippet expand after dollar sign in org mode
(add-hook 'org-mode-hook (lambda () (modify-syntax-entry ?$ "_" org-mode-syntax-table)))
;; also treat ' as a separator or whatever
(add-hook 'org-mode-hook (lambda () (modify-syntax-entry ?' "_" org-mode-syntax-table)))
;; startup with headlines and blocks folded or not
(setq org-startup-folded 'showall)
;; org-hide-block-startup t)
;; try to get the width from an #+ATTR.* keyword and fall back on the original width if none is found.
(setq org-image-actual-width nil)
;; dont center images/tables in latex
(setq org-latex-images-centered nil)
(setq org-latex-tables-centered nil)
;; get rid of background colors of block lines bleeding all over folded headlines
(setq org-fontify-whole-block-delimiter-line nil)
(setq org-fold-catch-invisible-edits 'smart
      org-agenda-span 20)
;; open agenda on startup
(add-hook 'after-init-hook
          (lambda ()
            (org-roam-db-sync)
            (org-agenda-list)
            (delete-other-windows)
            ;; (switch-to-light-theme)
            (switch-to-dark-theme)
            ))
;; disable multiplication precedence over division in calc
(setq calc-multiplication-has-precedence nil)
;; stop org mode from moving tags far after headers
(setq org-tags-column 0)
;; inherit attach folders
(setq org-attach-use-inheritance t)
;; use html5 for org exports
(setq org-html-html5-fancy t)

(defun generate-random-string (NUM)
  "generate a random alphanumerics string of length NUM."
  (interactive "P")
  (let* ((random-str "")
         (charset "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
         (baseCount (length charset)))
    (dotimes (_ (if (numberp NUM) (abs NUM) NUM))
      (setq random-str (concat random-str
                               (char-to-string (elt charset (random baseCount))))))
    random-str))
(defun temp-file (EXT)
  (format "%stmp_%s.%s" (concat brain-path "out/") (generate-random-string 7) EXT))
(global-set-key (kbd "C-c R") (lambda () (interactive) (insert (generate-random-string 7))))

(defun switch-to-dark-theme ()
  "switch to dark theme"
  (interactive)
  (disable-theme 'doom-gruvbox-light)
  (load-theme 'darktooth t)
  ;; (load-theme 'soothe t)
  ;; (load-theme 'minimal t)
  (set-face-attribute 'whitespace-space nil :background nil)
  (set-face-attribute 'whitespace-newline nil :background nil)
  ;; (global-org-modern-mode)
  ;; (add-hook 'pdf-view-mode-hook 'pdf-view-themed-minor-mode)
  (set-themed-pdf 1))

(defun switch-to-light-theme ()
  "switch to light theme"
  (interactive)
  (disable-theme 'darktooth)
  ;; (disable-theme 'soothe)
  (load-theme 'doom-gruvbox-light t)
  (set-face-attribute 'org-block nil :background nil)
  (set-face-attribute 'whitespace-space nil :background nil)
  (set-face-attribute 'whitespace-newline nil :background nil)
  ;; (global-org-modern-mode)
  ;; (set-face-background hl-line-face "PeachPuff3")
  ;; (remove-hook 'pdf-view-mode-hook 'pdf-view-themed-minor-mode)
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
(setq bookmark-file (concat brain-path "emacs_bookmarks"))

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
;; (defun non-relative-path (obj)
;;   "return non-relative path for hugo"
;;   (interactive)
;;   (if (eq (type-of obj) 'string)
;;       (progn
;;         ;; if we're here, obj is a link to a file, file-truename can be used to get the full path of it
;;         ;; (message (format "filename: %s" obj))
;;         (file-name-nondirectory obj))
;;     obj))
;; (advice-add 'org-export-resolve-id-link :filter-return #'non-relative-path)

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

(defun all-roam-files ()
  "return a list of all files in the org-roam database"
  (seq-uniq
   (seq-map
    #'car
    (org-roam-db-query
     `(:select file :from nodes)))))

(defun roam-files-with-tag (tag-name)
  "Return a list of note files containing a specific tag.";
  (seq-uniq
   (seq-map
    #'car
    (org-roam-db-query
     `(:select [nodes:file]
               :from tags
               :left-join nodes
               :on (= tags:node-id nodes:id)
               :where (like tag ,tag-name))))))

;; dynamic org-agenda
(add-to-list 'org-tags-exclude-from-inheritance "todo")
(add-to-list 'org-tags-exclude-from-inheritance "band")
(defun buffer-contains-todo ()
  "check if the buffer contains a TODO entry"
  (org-element-map
      (org-element-parse-buffer 'headline)
      'headline
    (lambda (h)
      (eq (org-element-property :todo-type h) 'todo))
    nil 'first-match))
(add-hook 'before-save-hook #'update-todo-tag)
(defun update-todo-tag ()
  "remove/add the todo tag to the buffer by checking whether it contains a TODO entry"
  (let ((kill-ring)) ;; keep kill ring, dont modify it
    (when (and (not (active-minibuffer-window))
               (is-buffer-roam-note))
      (save-excursion
        (goto-char (point-min))
        (if (buffer-contains-todo)
            (org-roam-tag-add '("todo"))
          (ignore-errors (org-roam-tag-remove '("todo")))))))
  (agenda-files-update))
(defun is-buffer-roam-note ()
  "return non-nil if the currently visited buffer is a note."
  (and buffer-file-name
       (string-prefix-p
        (expand-file-name (file-name-as-directory org-roam-directory))
        (file-name-directory buffer-file-name))))
(setq org-agenda-files (roam-files-with-tag "todo"))
(defun agenda-files-update (&rest _)
  "Update the value of `org-agenda-files'."
  (setq org-agenda-files (roam-files-with-tag "todo")))
(advice-add 'org-agenda :before #'agenda-files-update)
(advice-add 'org-todo-list :before #'agenda-files-update)
;; stop showing deadlines in today
(setq org-deadline-warning-days 0)
;; remove done items
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
;; show only the first occurrence of a recurring task
(setq org-agenda-show-future-repeats 'next)
;; make org-open-at-point open link in the same buffer
(setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)
;; render latex in org-mode using builtin function
(add-hook 'org-mode-hook 'org-latex-preview-auto-mode)
;; starting up with latex previews tremendously slows things down... (not really after disabling cache)
(setq org-startup-with-latex-preview t)
(setq org-latex-preview-preamble "\\documentclass{article}\n[DEFAULT-PACKAGES]\n[PACKAGES]\n\\usepackage{xcolor}\n\\usepackage{\\string\~/.emacs.d/common}") ;; use my ~/.emacs.d/common.sty
;; export to html using dvisvgm/mathjax/whatever
(setq org-html-with-latex 'dvisvgm)
;; not sure why org-mode 9.7-pre dev branch doesnt respect global visual line mode so imma add this for now
(add-hook 'org-mode-hook 'visual-line-mode)
;; for live previews below envs
(setq org-latex-preview-auto-generate 'live)
(setq org-latex-preview-debounce 0.1)
(setq org-latex-preview-throttle 0.01)
;; use dvisvgm instead of dvipng, obsolete
;; (setq org-preview-latex-default-process 'dvisvgm)
;; dont export headlines with tags
(setq org-export-with-tags nil)
;; allow characters as list modifiers in org mode
(setq org-list-allow-alphabetical t)
;; also number equations
(setq org-latex-preview-numbered t)
;; ;; tell org latex previews to use lualatex, its better (i need it for some tikz functionalities)
;; (setq pdf-latex-compiler "lualatex")
;; ;; make dvisvgm preview use lualatex
;; (let ((pos (assoc 'dvisvgm org-latex-preview-process-alist)))
;;   (plist-put (cdr pos) :programs '("lualatex" "dvisvgm")))
;; make org-agenda open up in the current window
(setq org-agenda-window-setup 'current-window)
;; dont prompt for downloading remote files on export
(setq org-resource-download-policy nil)
;; enable eval: keyword in local variables
;; (setq enable-local-eval t)
;; dont number headers on exports
(setq org-export-with-section-numbers nil)
(setq org-use-property-inheritance t)
;; increase max number of messages
(setq message-log-max 100000)
;; dont set a default width for latex previews
(setq org-latex-preview-width 1.0)

;; (defun go-through-all-roam-files (callback)
;;   "run a callback function on each file in the org-roam database"
;;   (dolist (file (all-roam-files))
;;     (if (not (eq callback nil))
;;         (with-current-buffer (or (find-buffer-visiting file) (find-file-noselect file))
;;           (if (is-buffer-roam-note)
;;               (funcall callback))))))

(defun go-through-roam-files-with-tag (tag-name callback)
  "run a callback function on each file tagged with tag-name"
  (dolist (file (roam-files-with-tag tag-name))
    (with-current-buffer (or (find-buffer-visiting file) (find-file-noselect file))
      (when (is-buffer-roam-note)
        (funcall callback)
        ;; (kill-this-buffer)
        ))))

(defun execute-code-files ()
  "execute files tagged with 'code'"
  (interactive)
  (go-through-roam-files-with-tag
   "code"
   (lambda ()
     (condition-case err
         (org-babel-execute-buffer)
       (error (message "got error %s while executing %s" err (buffer-file-name)))))))
(defun lob-reload ()
  "load files tagged with 'code' into the org babel library (lob - library of babel)"
  (interactive)
  (go-through-roam-files-with-tag
   "code"
   (lambda ()
     (org-babel-lob-ingest (buffer-file-name)))))
;; most/all of my code files are lisp, load them with sly/slime
(add-hook 'sly-connected-hook #'execute-code-files)
;; i need those in library of babel on startup too
(lob-reload)

;; (defun xenops-prerender ()
;;   "prerender latex blocks in roam files"
;;   (interactive)
;;   (go-through-roam-files-with-tag
;;    "math"
;;    (lambda ()
;;      (message "processing math file %s" (buffer-file-name)))))

;; (defun run-all-code-blocks ()
;;   "run code blocks in all org-roam files"
;;   (interactive)
;;   (go-through-all-roam-files
;;    (lambda ()
;;      (message "processing file %s" (buffer-file-name))
;;      (org-babel-execute-buffer))))

;; (go-through-roam-files-with-tag "math" (lambda () (message buffer-file-name)))
;; (defun publicize-files ()
;;   (interactive)
;;   (go-through-roam-files-with-tag
;;    "computer-science"
;;    (lambda ()
;;      (org-roam-tag-add '("public"))
;;      (save-buffer))))

(defun buffer-contains-substring (string)
  "check if the current buffer contains a specific string"
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (not (eq nil (search-forward string nil t))))))

(defun buffer-contains-math ()
  "check if the current buffer contains any math equations (latex blocks)"
  (or
   (buffer-contains-substring "$")
   (buffer-contains-substring "\\(")
   (buffer-contains-substring "\\[")))

(defmacro save-buffer-modified-p (&rest body)
  "Eval BODY without affected buffer modification status"
  `(let ((buffer-modified (buffer-modified-p))
         (buffer-undo-list t))
     (unwind-protect
         ,@body
       (set-buffer-modified-p buffer-modified))))

(defun update-math-file ()
  "add/remove the math tag to the file"
  (let ((kill-ring)  ;; keep kill ring, dont modify it
        (buffer-undo-list)) ;; keep the undo "ring" too, doesnt work tho, hmmmm
    (when (and (not (active-minibuffer-window))
               (is-buffer-roam-note))
      (save-excursion
        (goto-char (point-min))
        (if (buffer-contains-math)
            (org-roam-tag-add '("math"))
          (ignore-errors (org-roam-tag-remove '("math"))))))))
(add-hook 'before-save-hook #'update-math-file)

(defun find-math-files (basedir)
  "find all org files in a directory that are math files and tag them with 'math' tag"
  (interactive)
  (dolist (file (directory-files-recursively basedir ".*\\.org$"))
    (ignore-errors (with-current-buffer (or (find-buffer-visiting file) (find-file-noselect file))
                     (beginning-of-buffer)
                     (org-id-get-create)
                     (ignore-errors (update-math-file))
                     (save-buffer)))))

(defun update-math-files ()
  "go through all roam files and check each for math formulas and update the math tag"
  (dolist (file (all-roam-files))
    (with-current-buffer (or (find-buffer-visiting file) (find-file-noselect file))
      (update-math-file)
      (save-buffer))))

;; (defun buffer-contains-code ()
;;   "check if the buffer contains an org-babel source block"
;;   (interactive)
;;   (org-element-map
;;       (org-element-parse-buffer 'element)
;;       'src-block
;;     (lambda (h) t)
;;     ;; (or (eq (org-element-property :todo-type h)
;;     ;;         'todo)
;;     ;;     (eq (org-element-property :todo-type h)
;;     ;;         'done)))
;;     nil 'first-match))

;; (defun update-code-file ()
;;   "add/remove the code tag to the file"
;;   (when (and (not (active-minibuffer-window))
;;              (is-buffer-roam-note))
;;     (save-excursion
;;       (goto-char (point-min))
;;       (if (buffer-contains-code)
;;           (org-roam-tag-add '("math"))
;;         (org-roam-tag-remove '("math"))))))
;; (add-hook 'before-save-hook #'update-math-file)

;; (defun find-code-files (basedir)
;;   "find all org files in a directory that contain source blocks and tag them with 'code' tag"
;;   (interactive)
;;   (dolist (file (directory-files-recursively basedir ".*\\.org$"))
;;     (ignore-errors (with-current-buffer (or (find-buffer-visiting file) (find-file-noselect file))
;;       (beginning-of-buffer)
;;       (ignore-errors (update-code-file))
;;       (save-buffer)))))

(defun sudo-find-file (file-name)
  "like find file, but opens the file as root using tramp"
  (interactive (list (read-file-name "file: " "/sudo::/")))
  (let ((tramp-file-name (expand-file-name file-name)))
    (find-file tramp-file-name)))

(defun org-babel-fold-all-latex-src-blocks ()
  "toggle visibility of org-babel latex src blocks"
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    ;; (re-search-forward org-babel-src-block-regexp)
    (ignore-errors (org-babel-next-src-block))
    (let ((old-point nil)
          (current-point (point)))
      (while (not (eq old-point current-point))
        (progn
          (if (string= (org-element-property :language (org-element-at-point)) "latex")
              (org-cycle))
          (ignore-errors (org-babel-next-src-block))
          (setf old-point current-point)
          (setf current-point (point))))))
  (org-content))
;; (add-hook 'org-mode-hook 'org-babel-fold-all-latex-src-blocks)

(defun org-fold-all-answer-blocks ()
  "toggle visibility of answer special blocks, i.e. #+begin_answer"
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (let ((old-point nil)
          (current-point (point)))
      (while (not (eq old-point current-point))
        (progn
          (if (string= (org-element-property :type (org-element-at-point)) "answer")
              (org-cycle))
          (org-next-block 1)
          (setf old-point current-point)
          (setf current-point (point)))))))
;; (add-hook 'org-mode-hook 'org-fold-all-answer-blocks)

(defun org-current-headline-name ()
  "get the name of the current headline"
  (interactive)
  (save-excursion
    (org-previous-visible-heading 1)
    (org-element-property :raw-value (org-element-at-point))))

(defun org-parent-headline-name ()
  "get the name of the parent headline"
  (interactive)
  (save-excursion
    (org-up-heading-safe)
    (org-element-property :raw-value (org-element-at-point))))

(defun open-in-vscode ()
  "open current file in vscode"
  (interactive)
  (shell-command (format "code %s" (buffer-file-name))))

(defun kill-this-buffer-volatile ()
  "kill current buffer, even if it has been modified."
  (interactive)
  (set-buffer-modified-p nil)
  (kill-this-buffer))

(defun copy-file-path (&optional DirPathOnlyQ)
  "Copy current buffer file path or dired path.
Result is full path.
If `universal-argument' is called first, copy only the dir path.

If in dired, copy the current or marked files.

If a buffer is not file and not dired, copy value of `default-directory'.

URL `http://xahlee.info/emacs/emacs/emacs_copy_file_path.html'
Version 2018-06-18 2021-09-30"
  (interactive "P")
  (let (($fpath
         (if (string-equal major-mode 'dired-mode)
             (progn
               (let (($result (mapconcat 'identity (dired-get-marked-files) "\n")))
                 (if (equal (length $result) 0)
                     (progn default-directory )
                   (progn $result))))
           (if (buffer-file-name)
               (buffer-file-name)
             (expand-file-name default-directory)))))
    (kill-new
     (if DirPathOnlyQ
         (progn
           (message "Directory copied: %s" (file-name-directory $fpath))
           (file-name-directory $fpath))
       (progn
         (message "File path copied: %s" $fpath)
         $fpath)))))

(defun cached-file (filename)
  "return 'filename' prefixed with cache dir path"
  (concat brain-path "out/" filename))

;; disable stupid beep sounds on macos
(setq ring-bell-function #'ignore)

;; from https://www.emacswiki.org/emacs/FindingNonAsciiCharacters
(defun find-first-non-ascii-char ()
  "Find the first non-ascii character from point onwards."
  (interactive)
  (let (point)
    (save-excursion
      (setq point
            (catch 'non-ascii
              (while (not (eobp))
                (or (eq (char-charset (following-char))
                        'ascii)
                    (throw 'non-ascii (point)))
                (forward-char 1)))))
    (if point
        (goto-char point)
      (message "No non-ascii characters."))))
(defun find-next-unsafe-char (&optional coding-system)
  "Find the next character in the buffer that cannot be encoded by
coding-system. If coding-system is unspecified, default to the coding
system that would be used to save this buffer. With prefix argument,
prompt the user for a coding system."
  (interactive "Zcoding-system: ")
  (if (stringp coding-system) (setq coding-system (intern coding-system)))
  (if coding-system nil
    (setq coding-system
          (or save-buffer-coding-system buffer-file-coding-system)))
  (let ((found nil) (char nil) (csets nil) (safe nil))
    (setq safe (coding-system-get coding-system 'safe-chars))
    ;; some systems merely specify the charsets as ones they can encode:
    (setq csets (coding-system-get coding-system 'safe-charsets))
    (save-excursion
      ;;(message "zoom to <")
      (let ((end  (point-max))
            (here (point    ))
            (char  nil))
        (while (and (< here end) (not found))
          (setq char (char-after here))
          (if (or (eq safe t)
                  (< char ?\177)
                  (and safe  (aref safe char))
                  (and csets (memq (char-charset char) csets)))
              nil ;; safe char, noop
            (setq found (cons here char)))
          (setq here (1+ here))) ))
    (and found (goto-char (1+ (car found))))
    found))

(defun open-todays-file ()
  "open todays org file"
  (interactive)
  (let ((todays-file (format-time-string (concat brain-path "/daily/%Y-%m-%d.org"))))
    (if (not (file-exists-p todays-file))
        (org-roam-capture-no-title-prompt nil "d"))
    (find-file todays-file)))

(defun today-entry ()
  "insert an entry for today, an action/todo/whatever and clock in"
  (interactive)
  (open-todays-file)
  (org-insert-heading-respect-content)
  (org-clock-in)
  (org-up-heading-safe)
  (end-of-line)
  (ignore-errors (evil-insert 0)))

(defun check-svg-duplicates ()
  "check for code blocks with same svg files, TODO needs to be implemented"
  (interactive)
  (go-through-all-roam-files
   (lambda ()
     (interactive)
     (message (current-filename)))))

(defun open-kitty-here ()
  (interactive)
  (async-shell-command "kitty ."))

(defun push-blog-github ()
  (interactive)
  (execute-kbd-macro (read-kbd-macro "SPC d g SPC s e M-r reexport RET RET")))

;; make links like [[id::blockname]] work, need to rebuild database after defining the advice using org-roam-db-clear-all and then org-roam-db-sync
(defun +org--follow-search-string-a (fn link &optional arg)
  "Support ::SEARCH syntax for id::name links.
note that this doesnt work for exports"
  (save-match-data
    (cl-destructuring-bind (id &optional search)
        (split-string link "::")
      (prog1 (funcall fn id arg)
        (cond ((null search))
              ((string-match-p "\\`[0-9]+\\'" search)
               ;; Move N lines after the ID (in case it's a heading), instead
               ;; of the start of the buffer.
               (forward-line (string-to-number option)))
              ((string-match "^/\\([^/]+\\)/$" search)
               (let ((match (match-string 1 search)))
                 (save-excursion (org-link-search search))
                 ;; `org-link-search' only reveals matches. Moving the point
                 ;; to the first match after point is a sensible change.
                 (when (re-search-forward match)
                   (goto-char (match-beginning 0)))))
              ((org-link-search search)))))))
(advice-add 'org-id-open :around #'+org--follow-search-string-a)
(advice-add 'org-roam-id-open :around #'+org--follow-search-string-a)

(defun treemacs-remove-project-at-point-force ()
  (interactive)
  "force removal of project at point, even if its the last one"
  (treemacs-do-remove-project-from-workspace (treemacs-project-at-point) t))

;; TODO: add latex auto-completion to org-mode, requires auctex
;; (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point)))
;; (TeX--completion-at-point
;;  t
;;  LaTeX--arguments-completion-at-point)

(unbind-key "C-z")
(bind-keys :prefix-map mymap
           :prefix "C-z"
           ;;:prefix-docstring "Personal key bindings"
           ("v" . emacs-version)
           ("e" . (lambda () (interactive) (find-file user-init-file)))
           ("a" . kill-all-buffers)
           ("z" . zap-up-to-char)
           ("g" . deadgrep)
           )

(global-set-key (kbd "C-x k") 'kill-this-buffer-volatile)
(global-set-key (kbd "C-x K") 'kill-buffer-and-window)

;; temporary fix for ox-hugo exporting issues with org 9.7-pre
(defun org-html-format-latex (latex-frag processing-type info)
  "Format a LaTeX fragment LATEX-FRAG into HTML.
PROCESSING-TYPE designates the tool used for conversion.  It can
be `mathjax', `verbatim', `html', nil, t or symbols in
`org-preview-latex-process-alist', e.g., `dvipng', `dvisvgm' or
`imagemagick'.  See `org-html-with-latex' for more information.
INFO is a plist containing export properties."
  (let ((cache-relpath "") (cache-dir ""))
    (unless (or (eq processing-type 'mathjax)
                (eq processing-type 'html))
      (let ((bfn (or (buffer-file-name)
                     (make-temp-name
                      (expand-file-name "latex" temporary-file-directory))))
            (latex-header
             (let ((header (plist-get info :latex-header)))
               (and header
                    (concat (mapconcat
                             (lambda (line) (concat "#+LATEX_HEADER: " line))
                             (org-split-string header "\n")
                             "\n")
                            "\n")))))
        (setq cache-relpath
              (concat (file-name-as-directory org-preview-latex-image-directory)
                      (file-name-sans-extension
                       (file-name-nondirectory bfn)))
              cache-dir (file-name-directory bfn))
        ;; Re-create LaTeX environment from original buffer in
        ;; temporary buffer so that dvipng/imagemagick can properly
        ;; turn the fragment into an image.
        (setq latex-frag (concat latex-header latex-frag))))
    (with-temp-buffer
      (insert latex-frag)
      (org-format-latex cache-relpath nil nil cache-dir nil
                        "Creating LaTeX Image..." nil processing-type)
      (buffer-string))))

;; temporary fix for latex preview exports in html
(plist-put org-html-latex-image-options :inline "svg")
;; temporary fix for ox-hugo with new org latex preview system
(advice-add 'org-blackfriday--update-ltximg-path
            :around
            (lambda (orig-fn html-string)
              (if (plist-get org-html-latex-image-options :inline)
                  html-string
                (funcall orig-fn html-string)))
            '((name . inline-image-workaround)))
;; make org not evaluate code blocks on exporting
(add-to-list 'org-babel-default-header-args '(:eval . "no-export"))
(add-to-list 'org-babel-default-inline-header-args '(:eval . "no-export"))

(setq org-publish-project-alist
      '(("orgfiles"
         :base-directory "~/brain/notes/"
         :base-extension "org"
         :publishing-directory "~/publish/"
         :publishing-function org-html-publish-to-html
         ;; :exclude "PrivatePage.org" ;; regexp
         :headline-levels 3
         :section-numbers nil
         :with-toc nil
         :recursive t
         :html-preamble t)

        ("images"
         :base-directory "~/brain/"
         :base-extension "jpg\\|gif\\|png\\|webp\\|jpeg\\|svg"
         :publishing-directory "~/publish/"
         :recursive t
         :publishing-function org-publish-attachment)

        ("website" :components ("orgfiles" "images"))))

;; https://karthinks.com/software/a-consistent-structural-editing-interface/
(repeat-mode 1)
(defvar structural-edit-map
  (let ((map (make-sparse-keymap)))
    (pcase-dolist (`(,k . ,f)
                   '(("u" . backward-up-list)
                     ("f" . forward-sexp)
                     ("b" . backward-sexp)
                     ("d" . down-list)
                     ("k" . kill-sexp)
                     ("n" . forward-list)
                     ("p" . backward-list)
                     ("K" . sp-kill-hybrid-sexp)
                     ("]" . sp-forward-slurp-sexp)
                     ("[" . sp-backward-slurp-sexp)
                     ("}" . sp-forward-barf-sexp)
                     ("{" . sp-backward-barf-sexp)
                     ("C" . sp-convolute-sexp)
                     ("J" . sp-join-sexp)
                     ("S" . sp-split-sexp)
                     ("R" . sp-raise-sexp)
                     ("\\" . indent-region)
                     ("/" . undo)
                     ("t" . transpose-sexps)
                     ("x" . eval-defun)))
      (define-key map (kbd k) f))
    map))
(map-keymap
 (lambda (_ cmd)
   (put cmd 'repeat-map 'structural-edit-map))
 structural-edit-map)

;; org mode navigation map
(defvar org-nav-map
  (let ((map (make-sparse-keymap)))
    (pcase-dolist (`(,k . ,f)
                   '(("c" . org-babel-next-src-block)
                     ("C" . org-babel-previous-src-block)
                     ("h" . org-next-visible-heading)
                     ("H" . org-previous-visible-heading)
                     ("o" . org-next-block)
                     ("O" . org-previous-block)
                     ("i" . org-next-item)
                     ("I" . org-previous-item)
                     ("l" . org-next-link)
                     ("L" . org-previous-link)
                     ("e" . org-forward-element)
                     ("E" . org-backward-element)
                     ("s" . scroll-up-command)
                     ("S" . scroll-down-command)))
      (define-key map (kbd k) f))
    map))
(map-keymap
 (lambda (_ cmd)
   (put cmd 'repeat-map 'org-nav-map))
 org-nav-map)
(define-key org-mode-map (kbd "C-l") org-nav-map)

;; automatic recursive exporting of linked notes
(defun nodes-linked-from-node (node)
  "return list of roam nodes linked to from node with node-id";
  (when node
    (mapcar
     #'org-roam-node-from-id
     (mapcar
      #'car
      (org-roam-db-query
       [:select :distinct [dest]
                :from links
                :where (= source $s1)
                :and (= type "id")]
       (org-roam-node-id node))))))

(defun nodes-linked-from-node-file (node)
  (when node
    (mapcar
     #'org-roam-node-from-id
     (mapcar
      #'car
      (org-roam-db-query
       [:select :distinct [links:dest]
                :from links
                :left-join nodes
                :on (= links:source nodes:id)
                :left-join files
                :on (= files:file nodes:file)
                :where (= nodes:file $s1)]
       (org-roam-node-file node))))))

(defun this-buffer-roam-node ()
  "get the roam-node of the current buffer"
  (save-excursion
    (goto-char 0)
    (when (org-id-get)
      (org-roam-node-from-id (org-id-get)))))

(defun nodes-linked-from-this-node ()
  "nodes linked to from this node"
  (let ((node (this-buffer-roam-node)))
    (nodes-linked-from-node node)))

(defun nodes-linked-from-this-file ()
  "nodes linked to from this node"
  (let ((node (this-buffer-roam-node)))
    (nodes-linked-from-node-file node)))

;; old one
;; (defun export-node (node)
;;   "export a node's file to both hugo md and pdf"
;;   (condition-case err
;;       (let ((org-startup-with-latex-preview nil)
;;             (file (org-roam-node-file node)))
;;         (with-current-buffer (or (find-buffer-visiting file) (find-file-noselect file))
;;           ;; (org-to-pdf)
;;           (org-hugo-export-to-md)))
;;     (error
;;      (error (format "export-node failed %s, not retrying, err: %s" (org-roam-node-file node) err))
;;      ;; (org-latex-preview--clear-preamble-cache)
;;      ;; (org-latex-preview-clear-cache)
;;      ;; (export-node node)
;;      )))

(defun export-node (node)
  "export a node's file to both hugo md and pdf"
  (let ((org-startup-with-latex-preview nil)
        (file (org-roam-node-file node)))
    (with-current-buffer (or (find-buffer-visiting file) (find-file-noselect file))
      ;; (org-to-pdf)
      (org-hugo-export-to-md))))

(defun export-current-buffer ()
  "gets the node associated with the current buffer, exports it"
  (interactive)
  (let ((node (this-buffer-roam-node)))
    (when node
      (export-node node))))

(defun export-current-buffer-recursively ()
  "gets the node associated with the current buffer, exports it with export-node-recursively, see its docstring"
  (interactive)
  (let ((node (this-buffer-roam-node)))
    (when node
      (export-node-recursively node))))

(defun export-node-recursively (node &optional exceptions)
  "export node, export all nodes/files it links to, and all files linked from those and so on, basically we're exporting the connected subgraph the node exists in, `exceptions' is used for recursion to keep a record of exported nodes"
  ;; (message "%s" exceptions)
  (if (and node
           (when (not (cl-find node exceptions
                               :test (lambda (node1 node2)
                                       (string= (org-roam-node-file node1)
                                                (org-roam-node-file node2)))))))
      (progn
        (push node exceptions)
        (let ((nodes (nodes-linked-from-node-file node)))
          (dolist (other-node nodes)
            (when other-node (message (format "exporter jumping to: %s" (org-roam-node-file other-node))))
            (setf exceptions (export-node-recursively other-node exceptions))))
        (when (and node (member "public" (org-roam-node-tags node)))
          (message (format "exporting: %s" (org-roam-node-file node)))
          (export-node node))
        exceptions)
    exceptions))

(defun my-org-link-advice (fn link desc info)
  "when exporting a file, it may contain links to other org files via id's, if a file being exported links to a note that is not tagged 'public', dont transcode the link to that note, just insert its description 'desc'"
  (let* ((link-type (org-element-property :type link))
         (link-path (org-element-property :path link))
         (is-id-link (string= link-type "id")))
    (if is-id-link
        (condition-case err ;; handle error when org-roam cannot find link in database
            (let* ((node (org-roam-node-from-id link-path))
                   (tags (org-roam-node-tags node)))
              (if (and (member "public" tags) ;; if note is public export as usual, otherwise dont export it as link but just as text
                       (not (string-match-p "::" link-path))) ;; if link isnt of form [[id::block]], dont export it as link, we cant handle those yet
                  (funcall fn link desc info)
                (format "<b>%s</b>" desc)))
          (error (message "org-roam couldnt find link %s, error was: %s" link-path err)
                 (format "<b>%s</b>" desc))) ;; even when we cant find it in the database we still render it
      (funcall fn link desc info))))
(advice-add #'org-html-link :around #'my-org-link-advice)
(advice-add #'org-hugo-link :around #'my-org-link-advice)

(defun message-no-format (msg)
  "invoke 'message' without it invoking 'format' (not really)"
  (message "%s" msg))

;; set org-mode date's export according to file creation date
(defun file-modif-time (filepath)
  "the time the file was last modified"
  (let ((atr (file-attributes filepath)))
    (file-attribute-modification-time atr)))
;; (defun file-status-change-time (filepath)
;;   (let ((atr (file-attributes filepath)))
;;     (file-attribute-status-change-time atr)))
(defun file-creation-time (filepath)
  "get file creation timestamp, only works on ext4 (and other fs's that support 'crtime'),"
  (string-to-number
   (shell-command-to-string (format "stat --format='%%W' '%s'" filepath))))
(defun my-org-date-advice (fn info &optional fmt)
  (let ((myfile (plist-get info :input-file)))
    ;; (format-time-string "<%Y-%m-%d>" (file-modif-time myfile))))
    (format-time-string "<%Y-%m-%d>" (file-creation-time myfile))))
(advice-add #'org-export-get-date :around #'my-org-date-advice)

(defun export-all-public ()
  "export nodes with tag 'public'"
  (interactive)
  (let ((exceptions))
    (go-through-roam-files-with-tag
     "public"
     (lambda ()
       ;; (message "%s" exceptions)
       (setf exceptions
             (export-node-recursively (org-roam-node-from-id (org-id-get)) exceptions))))))

;; org-special-edit with lsp?, laggy af
;; (defun org-babel-edit-prep:python (babel-info)
;;   (setq-local buffer-file-name (->> babel-info caddr (alist-get :tangle)))
;;   (lsp))

;; dont override my labels, silly org
(setq org-latex-prefer-user-labels t)

;; prettify symbols..
(global-prettify-symbols-mode +1)
;; replace lambda text with symbol
(defconst lisp--prettify-symbols-alist
  '(("lambda"  . ?λ)))
;; convert back to text when cursor is over the symbol
(setq prettify-symbols-unprettify-at-point 'right-edge)
;; (add-hook 'emacs-lisp-mode-hook
;;             (lambda ()
;;               (push '(">=" . ?≥) prettify-symbols-alist)))
;; (defun org-set-prettify-symbols ()
;;   (setq-local prettify-symbols-alist
;;               (mapcan (lambda (x) (list x (cons (upcase (car x)) (cdr x))))
;;                       '(("#+begin_src" . ?➤)
;;                         ("#+end_src" . ?⮜)
;;                         ("#+begin_example" . ?)
;;                         ("#+end_example" . ?)
;;                         ("#+header:" . ?)
;;                         ("#+title:" . ?🌐)
;;                         ("#+results:" . ?)
;;                         ("#+name:" . ?📌)
;;                         ("#+call:" . ?)
;;                         (":properties:" . ?)
;;                         ("#+include:" . ?📎);;?🔗) ;;?📌)
;;                         ("#+setupfile:" . ?🔧)
;;                         ("#+filetags:" . "🔑")
;;                         ;;💡🔥🔑💡🚀🔥💎📝🎯📌🔒🎁⭐💌🌺☢️
;;                         ;; 𓍢ִ໋🌷͙֒
;;                         )))
;;   ;; org mode doesnt inherit the global mode for some reason so imma hook it manually
;;   (prettify-symbols-mode))
;; (add-hook 'org-mode-hook #'org-set-prettify-symbols)

(defun ascii-table ()
  "display basic ASCII table (0 thru 128)."
  (interactive)
  (switch-to-buffer "*ASCII*")
  (erase-buffer)
  (setq buffer-read-only nil)        ;; Not need to edit the content, just read mode (added)
  (local-set-key "q" 'bury-buffer)   ;; Nice to have the option to bury the buffer (added)
  (save-excursion (let ((i -1))
                    (insert "ASCII characters 0 thru 127.\n\n")
                    (insert " Hex  Dec  Char|  Hex  Dec  Char|  Hex  Dec  Char|  Hex  Dec  Char\n")
                    (while (< i 31)
                      (insert (format "%4x %4d %4s | %4x %4d %4s | %4x %4d %4s | %4x %4d %4s\n"
                                      (setq i (+ 1  i)) i (single-key-description i)
                                      (setq i (+ 32 i)) i (single-key-description i)
                                      (setq i (+ 32 i)) i (single-key-description i)
                                      (setq i (+ 32 i)) i (single-key-description i)))
                      (setq i (- i 96))))))

;; run some python code from my org notes on shell startup
(add-hook 'python-shell-first-prompt-hook #'execute-code-files)

(defun current-mpv-artist ()
  (shell-command-to-string "sh -c 'echo \"{ \\\"command\\\": [\\\"get_property\\\", \\\"metadata\\\"] }\" | socat - /tmp/mpv_socket | jq -j .data.artist' 2>/dev/null"))

;; tree-sitter
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;; install those grammars
;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))

;; remap some modes to their tree-sitter alternatives, tree-sitter isnt used by default, yet
(dolist (mapping '((python-mode . python-ts-mode)
                   (css-mode . css-ts-mode)
                   (js-mode . js-ts-mode)
                   (typescript-mode . tsx-ts-mode)
                   (json-mode . json-ts-mode)
                   (html-mode . html-ts-mode)
                   (yaml-mode . yaml-ts-mode)))
  (add-to-list 'major-mode-remap-alist mapping))
;; also make org-special-edit respect tree-sitter modes
(dolist (mapping major-mode-remap-alist)
  (let ((lang-name (car (split-string (symbol-name (car mapping)) "\\-"))))
    (add-to-list 'org-src-lang-modes (cons lang-name (concat lang-name "-ts")))))

;; change cursor color when repeat-map is active, buggy
;; (setq my-repeat-p nil)
;; (add-hook 'post-command-hook
;;           (defalias 'my/repeat-change-cursor ; change cursor to bar during repeat
;;             (let (my-repeat-p my-ccol)
;;               (lambda ()
;;                 (unless (eq my-repeat-p repeat-in-progress)
;;                   (if repeat-in-progress ; turning on
;; 		                  (setq my-ccol (face-background 'cursor)))
;;                   (setq my-repeat-p repeat-in-progress)
;;                   (set-cursor-color
;; 		               (if repeat-in-progress
;; 		                   (face-foreground 'error)
;; 		                 ccol))))))
;;           90)

;; FIRST: git clone https://github.com/casouri/tree-sitter-module
;;        bash batch.sh
;; THEN : sudo cp dist/* /usr/local/lib
;; FINALLY:
;;(setq treesit-extra-load-path '("/usr/local/lib"))
;;;; Treesit ;; Eglot
;;(setq treesit-eglot-modes
;;      '((:ts (bash-mode . bash-ts-mode) :pacman "bash-language-server")
;;        (:ts (c++-mode . c++-ts-mode) :pacman "ccls")
;;        (:ts (c-mode . c-ts-mode) :pacman "ccls")
;;        (:ts (cpp-mode . cpp-ts-mode) :pacman "ccls")
;;        (:ts (c-sharp-mode . sharp-ts-mode))
;;        (:ts (cmake-mode . cmake-ts-mode))
;;        (:ts (css-mode . css-ts-mode) :pacman "vscode-css-languageserver")
;;        (:ts (dockerfile-mode . dockerfile-ts-mode))
;;        (:ts (elixir-mode . elixir-ts-mode))
;;        (:ts (glsl-mode . glsl-ts-mode))
;;        (:ts (go-mode . go-ts-mode) :pacman "gopls")
;;        (:ts (heex-mode . heex-ts-mode))
;;        (:ts (html-mode . html-ts-mode) :pacman "vscode-html-languageserver")
;;        (:ts (java-mode . java-ts-mode))
;;        (:ts (javascript-mode . js-ts-mode) :pacman "typescript-language-server")
;;        (:ts (js-json-mode . json-ts-mode) :pacman "vscode-json-languageserver")
;;        (:ts (julia-mode . julia-ts-mode))
;;        (:ts (make-mode . make-ts-mode))
;;        (:ts (markdown-mode . markdown-ts-mode))
;;        (:ts (python-mode . python-ts-mode) :pacman "jedi-language-server")
;;        (:ts (typescript-mode . typescript-ts-mode) :pacman "typescript-language-server")
;;        (:ts (proto-mode . proto-ts-mode))
;;        (:ts (ruby-mode . ruby-ts-mode))
;;        (:ts (rust-mode . rust-ts-mode) :pacman "rust-analyzer")
;;        (:ts (sql-mode . sql-ts-mode))
;;        (:ts (toml-mode . toml-ts-mode))
;;        (:ts (tsx-mode . tsx-ts-mode))
;;        (:ts (verilog-mode . verilog-ts-mode))
;;        (:ts (vhdl-mode . vhdl-ts-mode))
;;        (:ts (wgsl-mode . wgsl-ts-mode))
;;        (:ts (yaml-mode . yaml-ts-mode) :pacman "yaml-language-server")))
;;;; Not mature yet:
;;;; (push '(org-mode . org-ts-mode) major-mode-remap-alist)
;;;; (push '(perl-mode . perl-ts-mode) major-mode-remap-alist)              ;; cpan Perl::LanguageServer
;;(require 'treesit)
;;
;;;; Function to parse the above and make an install command
;;(if (treesit-available-p)
;;    (let ((pacman-install-list (list )))
;;      (dolist (ts-pm treesit-eglot-modes)
;;        (let ((majmode-remap (plist-get ts-pm :ts))
;;              (pacman-cmd (plist-get ts-pm :pacman)))
;;          ;; bind default major-mode to ts-mode
;;          (push majmode-remap major-mode-remap-alist)
;;          ;; populate install cmd
;;          (if pacman-cmd
;;              (unless (member pacman-cmd pacman-install-list)
;;                (push pacman-cmd pacman-install-list)))))
;;      (let ((install-cmd (concat
;;                          "pacman -S --needed "
;;                          (--reduce (concat acc " " it) pacman-install-list))))
;;        (message install-cmd)))
;;  (user-error "Treesitter not available"))
;;

;; (setq treesit-language-source-alist
;;   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
;;     (c "https://github.com/tree-sitter/tree-sitter-c")
;;     (cmake "https://github.com/uyha/tree-sitter-cmake")
;;     (common-lisp "https://github.com/theHamsta/tree-sitter-commonlisp")
;;     (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
;;     (css "https://github.com/tree-sitter/tree-sitter-css")
;;     (csharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
;;     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
;;     (go "https://github.com/tree-sitter/tree-sitter-go")
;;     (go-mod "https://github.com/camdencheek/tree-sitter-go-mod")
;;     (html "https://github.com/tree-sitter/tree-sitter-html")
;;     (js . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
;;     (json "https://github.com/tree-sitter/tree-sitter-json")
;;     (lua "https://github.com/Azganoth/tree-sitter-lua")
;;     (make "https://github.com/alemuller/tree-sitter-make")
;;     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
;;     (python "https://github.com/tree-sitter/tree-sitter-python")
;;     (r "https://github.com/r-lib/tree-sitter-r")
;;     (rust "https://github.com/tree-sitter/tree-sitter-rust")
;;     (toml "https://github.com/tree-sitter/tree-sitter-toml")
;;     (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
;;     (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
;;     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
;;


;; persistent comint history
;; https://oleksandrmanzyuk.wordpress.com/2011/10/23/a-persistent-command-history-in-emacs/
(defun comint-write-history-on-exit (process event)
  (comint-write-input-ring)
  (let ((buf (process-buffer process)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (insert (format "\nProcess %s %s" process event))))))
(defun turn-on-comint-history ()
  (let ((process (get-buffer-process (current-buffer))))
    (when process
      (setq comint-input-ring-file-name
            (format "%s/inferior-%s-history"
                    brain-path (process-name process)))
      (comint-read-input-ring)
      (set-process-sentinel process
                            #'comint-write-history-on-exit))))
(add-hook 'inferior-haskell-mode-hook 'turn-on-comint-history)
(add-hook 'inferior-python-mode-hook 'turn-on-comint-history)
(add-hook 'kill-buffer-hook 'comint-write-input-ring)
(defun mapc-buffers (fn)
  (mapc (lambda (buffer)
          (with-current-buffer buffer
            (funcall fn)))
        (buffer-list)))
(defun comint-write-input-ring-all-buffers ()
  (mapc-buffers 'comint-write-input-ring))
(add-hook 'kill-emacs-hook 'comint-write-input-ring-all-buffers)

;; combobulate, see https://www.masteringemacs.org/article/combobulate-structured-movement-editing-treesitter
;; here’s some example code that navigates to the next dictionary, list or set:
(defun move-to-next-container ()
  (interactive)
  (with-navigation-nodes (:nodes '("dictionary" "set" "list"))
    (combobulate-visual-move-to-node
     (combobulate-nav-logical-next) t)))

(defvar combobulate-edit-map
  (let ((map (make-sparse-keymap)))
    (pcase-dolist (`(,k . ,f)
                   '(("u" . combobulate-navigate-up-list-maybe)
                     ("f" . combobulate-navigate-forward)
                     ("b" . combobulate-navigate-backward)
                     ("d" . combobulate-navigate-down-list-maybe)
                     ("k" . combobulate-kill-node-dwim)
                     ("n" . combobulate-navigate-next)
                     ("p" . combobulate-navigate-previous)
                     ("J" . combobulate-splice)
                     ("a" . combobulate-navigate-beginning-of-defun)
                     ("e" . combobulate-navigate-end-of-defun)
                     ("\\" . indent-region)
                     ("/" . undo)
                     ("t" . combobulate-transpose-sexps)
                     ("x" . eval-defun)))
      (define-key map (kbd k) f))
    map))
(map-keymap
 (lambda (_ cmd)
   (put cmd 'repeat-map 'combobulate-edit-map))
 combobulate-edit-map)

;; org html title of blocks
(defun my-org-block-advice (fn special-block contents info)
  "i use properties like :title <block title>, make those available in html output too"
  (let ((args (org-babel-parse-header-arguments
               (org-element-property :parameters
                                     special-block))))
    
    (if args
        (progn
          (let ((mytitle (alist-get :title args)))
            ;; this is temporary, it replaces all html attributes instead of appending
            (setf (plist-get (plist-get special-block 'special-block) :attr_html)
                  (list (format ":data-title %s" mytitle)))
            (funcall fn special-block contents info)))
      (funcall fn special-block contents info))))
(advice-add #'org-html-special-block :around #'my-org-block-advice)
(advice-add #'org-hugo-special-block :around #'my-org-block-advice)

;; (defun execute-src-block-with-dependencies (&optional arg info params executor-type)
;;   (save-excursion
;;     (let ((src-block-location (nth 5 info)))
;;       )
;;     (message "%s" info)))
;; (defun org-babel-execute-src-block-advice ()
;;   (message "hi"))
;; (advice-add #'org-babel-execute-src-block :before #'execute-src-block-with-dependencies)