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
;; (global-hl-line-mode)
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
;; break long lines into multiple
(global-visual-line-mode)
;; stop the annoying warnings from org mode cache
(setq warning-minimum-level :emergency)
;; use imagemagick for formats like webp
(setq image-use-external-converter t)
;; display white spaces and newlines
(setq whitespace-style '(face tabs spaces trailing space-before-tab newline indentation empty space-after-tab space-mark tab-mark newline-mark missing-newline-at-eof))
;; (global-whitespace-mode)
;; show zero-width characters
(set-face-background 'glyphless-char "red")
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
(setq enable-evil t)

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
  (org-roam-db-autosync-mode)
  (require 'org-roam-export)
  (require 'org-roam-protocol)
  (global-set-key (kbd "C-c r f") 'org-roam-node-find)
  (setq org-roam-capture-templates
        '(("n" "note" plain "%?"
           :if-new (file+head "notes/%<%Y%m%d%H%M%S>-${slug}.org" "#+setupfile: ~/.emacs.d/setup.org\n#+include: ~/.emacs.d/common.org\n#+title: ${title}")
           :kill-buffer :unnarrowed t)
          ("k" "quick note" plain "%?"
           :if-new (file+head "quick/%<%Y%m%d%H%M%S>.org" "#+filetags: :quick-note:")
           :kill-buffer :unnarrowed t)
          ("d" "daily" plain "* %<%Y-%m-%d %H:%M:%S> %?"
           :if-new (file+head "daily/%<%Y-%m-%d>.org" "#+filetags: :daily:"))
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
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z") 'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))

;; vertical completion interface
(use-package counsel
  :config
  (ivy-mode)
  (setq ivy-height 20)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file))

;; evil-mode
(if enable-evil
    (progn
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

      (evil-define-key 'normal 'TeX-mode-map (kbd "SPC v") 'open-current-document-this-window)
      (general-define-key :states 'normal "s" 'save-buffer)
      (general-define-key :states '(normal motion) :keymaps 'override "SPC d w" (lambda () (interactive) (dired "~/dl/")))
      (general-define-key :states '(normal motion) :keymaps 'override "SPC d a" (lambda () (interactive) (dired "~/data/")))
      (general-define-key :states '(normal motion) :keymaps 'override "SPC d l" (lambda () (interactive) (dired (get-latex-cache-dir-path))))
      (general-define-key :states '(normal motion) :keymaps 'override "SPC d b" (lambda () (interactive) (dired brain-path)))
      (general-define-key :states '(normal motion) :keymaps 'override "SPC d h" (lambda () (interactive) (dired "~/")))
      (general-define-key :states '(normal motion) :keymaps 'override "SPC d p" (lambda () (interactive) (dired "~/p/")))
      (general-define-key :states '(normal motion) :keymaps 'override "SPC d d" 'dired)
      (general-define-key :states '(normal motion) :keymaps 'override "SPC d c" (lambda () (interactive) (dired default-directory)))
      (general-define-key :states '(normal motion) :keymaps 'override "SPC d o" (lambda () (interactive) (dired "~/brain/out/")))
      (general-define-key :states '(normal motion) :keymaps 'override "SPC d g" (lambda () (interactive) (dired "~/workspace/blog/")))
      (general-define-key :states '(normal motion) :keymaps 'override "SPC d m" (lambda () (interactive) (dired "~/brain/music/")))
      (general-define-key :states '(normal motion) :keymaps 'override "SPC f f" 'counsel-find-file)
      (general-define-key :states '(normal motion) :keymaps 'override "SPC f s" 'sudo-find-file)
      (general-define-key :states '(normal treemacs motion) :keymaps 'override "SPC SPC" 'counsel-M-x)
      (general-define-key :states '(normal motion) :keymaps 'override "SPC b k" 'kill-this-buffer)
      (general-define-key :states '(normal motion) :keymaps 'override "SPC b K" 'kill-buffer-and-window)
      (general-define-key :states '(normal motion) :keymaps 'override "SPC b a" 'kill-all-buffers)
      (general-define-key :states '(normal motion) :keymaps 'override "SPC b s" 'counsel-switch-buffer)
      (general-define-key :states 'normal :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map) "SPC x" 'eval-defun)
      (general-define-key :states 'normal :keymaps 'override "SPC g" 'counsel-ag)
      (general-define-key :states 'normal :keymaps 'org-mode-map "SPC x" 'space-x-with-latex-header-hack)
      (general-define-key :states 'normal :keymaps 'TeX-mode-map "SPC x" 'compile-current-document)
      (general-define-key :states '(normal motion) :keymaps 'override "SPC e" (lambda () (interactive) (find-file user-init-file)))
      (general-define-key :states 'normal :keymaps 'override "SPC p" 'projectile-command-map)
      ;; (general-define-key :states 'normal :keymaps 'TeX-mode-map "SPC c" 'compile-sagetex)
      (general-define-key :states 'normal :keymaps 'pdf-view-mode-map "d" 'pdf-view-scroll-up-or-next-page)
      (general-define-key :states 'normal :keymaps 'pdf-view-mode-map "u" 'pdf-view-scroll-down-or-previous-page)
      (general-define-key :states 'normal :keymaps 'pdf-view-mode-map "K" 'pdf-view-enlarge)
      (general-define-key :states 'normal :keymaps 'pdf-view-mode-map "J" 'pdf-view-shrink)
      ;; (general-define-key :states 'normal :keymaps 'dired-mode-map "l" 'dired-find-file)
      ;; (general-define-key :states 'normal :keymaps 'dired-mode-map "h" 'dired-up-directory)
      (general-define-key :states 'normal :keymaps 'org-mode-map "SPC r k" 'org-insert-link)
      ;; (general-define-key :states 'normal :keymaps 'org-mode-map "SPC z"
      ;;                     (lambda ()
      ;;                       (interactive)
      ;;                       (if (not xenops-mode)
      ;;                           (xenops-mode)
      ;;                         (xenops-render))))
      (general-define-key :states 'normal :keymaps 'org-mode-map "SPC z" 'org-latex-preview)
      (general-define-key :states '(normal motion) :keymaps 'override "SPC w" 'evil-window-map)
      (general-define-key :states '(normal motion) :keymaps 'override "SPC h" (general-simulate-key "C-h"))
      (general-define-key :states '(normal motion) :keymaps 'override "SPC i"
                          (lambda ()
                            (interactive)
                            (org-insert-time-stamp (current-time) t)))
      ;;(define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)
      ;; (general-define-key :states 'normal :keymaps 'override "SPC r t" 'org-roam-buffer-toggle)
      (general-define-key :states '(normal motion) :keymaps 'override "SPC r f" 'org-roam-node-find)
      (general-define-key :states 'normal :keymaps 'override "SPC r i" 'org-roam-node-insert)
      (general-define-key :states 'normal :keymaps 'override "SPC r c" 'org-id-get-create)
      (general-define-key :states 'normal :keymaps 'override "SPC r o" 'org-open-at-point)
      (general-define-key :states 'normal :keymaps 'override "SPC r a" 'org-attach)
      (general-define-key :states 'normal :keymaps 'override "SPC r A" 'org-attach-open)
      (general-define-key :states 'normal :keymaps 'override "SPC r l" 'org-roam-alias-add)
      (general-define-key :states 'normal :keymaps 'override "SPC r n" (lambda () (interactive) (org-roam-capture nil "n")))
      (general-define-key :states 'normal :keymaps 'override "SPC r w" 'org-roam-tag-add)
      (general-define-key :states 'normal :keymaps 'override "SPC r q"
                          (lambda ()
                            (interactive)
                            (org-roam-capture-no-title-prompt nil "k"))) ;; spc r q - quick note
      ;; (general-define-key :states 'normal :keymaps 'override "SPC r h"
      ;;                     (lambda ()
      ;;                       (interactive)
      ;;                       (org-roam-capture nil "t")))
      (general-define-key :states 'normal :keymaps 'org-mode-map "SPC r x"
                          (lambda ()
                            (interactive)
                            (org-to-pdf)
                            (org-hugo-export-to-md)))
      (general-define-key :states 'normal :keymaps 'org-mode-map "SPC r e" 'org-babel-tangle)
      (general-define-key :states 'normal :keymaps 'org-mode-map "SPC r E" 'org-babel-tangle-file)
      ;; (general-define-key :states 'normal :keymaps 'org-mode-map "SPC r d" 'org-deadline)
      ;; (general-define-key :states 'normal :keymaps 'org-mode-map "SPC r s" 'org-schedule)
      (general-define-key :states 'normal :keymaps 'override "SPC r g"
                          (lambda ()
                            (interactive)
                            (find-file (concat brain-path "/bib.bib"))))
      (general-define-key :states 'normal :keymaps 'org-mode-map "SPC r v" 'org-babel-execute-buffer)
      (general-define-key :states 'normal :keymaps 'org-mode-map "SPC r r" 'org-redisplay-inline-images)
      (general-define-key :states 'normal :keymaps 'org-mode-map "SPC r P" 'org-set-property)
      (general-define-key :states 'normal :keymaps 'org-mode-map "SPC r z" 'org-add-note)
      ;; (general-define-key :states 'normal :keymaps 'override "/" 'swiper)
      (general-define-key :states 'normal :keymaps 'org-mode-map "SPC c" "C-c C-c")
      (general-define-key :states 'normal :keymaps 'org-mode-map "]k" 'org-babel-next-src-block)
      (general-define-key :states 'normal :keymaps 'org-mode-map "[k" 'org-babel-previous-src-block)
      (general-define-key :states 'normal :keymaps 'org-mode-map "]o" 'org-next-block)
      (general-define-key :states 'normal :keymaps 'org-mode-map "[o" 'org-previous-block)
      (general-define-key :states 'normal :keymaps 'override "SPC r d"
                          (lambda ()
                            (interactive)
                            (org-roam-capture-no-title-prompt nil "d")))

      ;; keys to search for files
      (general-define-key :states 'normal :keymaps 'override "SPC f b"
                          (lambda () (interactive) (search-open-file brain-path".*\\(pdf\\|tex\\|doc\\|mp4\\|png\\|org\\)")))
      (general-define-key :states 'normal :keymaps 'override "SPC F b"
                  (lambda () (interactive) (search-open-file-in-emacs brain-path ".*\\(pdf\\|tex\\|doc\\|org\\)")))

      (define-key evil-normal-state-map (kbd "SPC f d")
                  (lambda () (interactive) (search-open-file "~/data" "")))
      (define-key evil-normal-state-map (kbd "SPC F d")
                  (lambda () (interactive)
                    (search-open-file-in-emacs "~/data" "")))

      ;; keybinding to evaluate math expressions
      (general-define-key :states '(normal motion) :keymaps 'override "SPC m"
                          (lambda ()
                            (interactive)
                            (setq result (calc-eval (buffer-substring-no-properties (region-beginning) (region-end))))
                            (end-of-line)
                            (insert " ")
                            (insert result)))
      ;; general keys
      ;; (general-define-key :states 'normal :keymaps 'override "SPC m" 'man)
      (general-define-key :states 'normal :keymaps 'override "SPC '" (general-simulate-key "C-c '"))
      (general-define-key :states 'normal :keymaps 'override "SPC w m"
                          (lambda () (interactive)
                            (when window-system (set-frame-size (selected-frame) 180 50))))
      (general-define-key :states '(normal treemacs motion) :keymaps 'override ")" 'evil-scroll-page-down)
      (general-define-key :states '(normal treemacs motion) :keymaps 'override "(" 'evil-scroll-page-up)
      (general-define-key :states '(normal motion) :keymaps 'override "SPC s d" 'switch-to-dark-theme)
      (general-define-key :states '(normal motion) :keymaps 'override "SPC s l" 'switch-to-light-theme)
      (general-define-key :states '(normal motion) :keymaps 'override "SPC s e" 'eshell)
      (general-define-key :states '(normal motion) :keymaps 'override "SPC s g" 'magit)
      (general-define-key :states '(normal motion) :keymaps 'override "SPC s i"
                          (lambda ()
                            (interactive)
                            (let ((current-prefix-arg '-)) (call-interactively 'sly))))
      (general-define-key :states '(normal motion) :keymaps 'override "SPC u" (general-simulate-key "C-u"))
      (general-define-key :states '(normal motion) :keymaps 'prog-mode-map "K" 'evil-jump-to-tag)
      (general-define-key :states '(normal motion) :keymaps 'override "SPC o l" 'avy-goto-line)
      (general-define-key :states '(normal motion) :keymaps 'override "SPC o c" 'avy-goto-char)
      (general-define-key :states '(normal motion) :keymaps 'override "SPC s s" 'spotify-lyrics)
      (general-define-key :states '(normal motion) :keymaps 'override "SPC s w" 'open-spotify-lyrics-file)
      (general-define-key :states '(normal motion) :keymaps 'override "SPC s t" 'counsel-load-theme)
      (general-define-key :states '(normal motion) :keymaps 'override "SPC s k" 'open-kitty-here)
      (general-define-key :states '(normal motion) :keymaps 'override "{" 'evil-scroll-line-up)
      (general-define-key :states '(normal motion) :keymaps 'override "}" 'evil-scroll-line-down)
      (general-define-key :states '(normal motion) :keymaps 'override "SPC l" 'calc)

      ;; agenda keys
      (general-define-key :states '(normal motion) :keymaps 'override "SPC a a" 'org-agenda-list)
      (general-define-key :states '(normal motion) :keymaps 'org-agenda-mode-map "q" 'org-agenda-exit)
      (general-define-key :states '(normal motion) :keymaps 'override "SPC a s" 'org-schedule)
      (general-define-key :states '(normal motion) :keymaps 'override "SPC a d" 'org-deadline)
      (general-define-key :states '(normal motion) :keymaps 'org-mode-map "SPC a j" 'org-clock-in)
      (general-define-key :states '(normal motion) :keymaps 'override "SPC a J" 'org-clock-in-last)
      (general-define-key :states '(normal motion) :keymaps 'override "SPC a k" 'org-clock-out)
      (general-define-key :states '(normal motion) :keymaps 'override "SPC a b" 'org-clock-cancel)
      (general-define-key :states '(normal motion) :keymaps 'org-mode-map "SPC a p" 'org-clock-display)
      (general-define-key :states '(normal motion) :keymaps 'override "SPC a t" (lambda () (interactive) (org-roam-capture nil "t")))
      (general-define-key :states '(normal motion) :keymaps 'override "SPC a n" 'today-entry)
      (general-define-key :states '(normal motion) :keymaps 'override "SPC a o" 'open-todays-file)
      (general-define-key :states '(normal motion) :keymaps 'override "SPC s n" 'yas-new-snippet)
      (general-define-key :states '(normal motion) :keymaps 'override "SPC s v" 'yas-visit-snippet-file)
      (general-define-key :states '(normal motion) :keymaps 'override "SPC s h" 'yas-insert-snippet)

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
      (general-define-key :states '(normal) :keymaps 'eshell-mode-map "SPC x" 'eshell-interrupt-process)

      (general-define-key :states '(normal) :keymaps 'lisp-mode-map "SPC x" 'sly-compile-defun)
      (general-define-key :states '(normal) :keymaps 'lisp-mode-map "SPC c" 'sly-eval-buffer)

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

      (use-package evil-textobj-tree-sitter
        :config
        ;; bind `function.outer`(entire function block) to `f` for use in things like `vaf`, `yaf`
        (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
        ;; bind `function.inner`(function block without name and args) to `f` for use in things like `vif`, `yif`
        (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))
        ;; You can also bind multiple items and we will match the first one we can find
        (define-key evil-outer-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer")))
        (define-key evil-inner-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj ("conditional.inner" "loop.inner")))
        ;; Goto start of next function
        (define-key evil-normal-state-map (kbd "]f")
                    (lambda ()
                      (interactive)
                      (evil-textobj-tree-sitter-goto-textobj "function.outer")))
        ;; Goto start of previous function
        (define-key evil-normal-state-map (kbd "[f")
                    (lambda ()
                      (interactive)
                      (evil-textobj-tree-sitter-goto-textobj "function.outer" t)))
        ;; Goto end of next function
        (define-key evil-normal-state-map (kbd "]F")
                    (lambda ()
                      (interactive)
                      (evil-textobj-tree-sitter-goto-textobj "function.outer" nil t)))
        ;; Goto end of previous function
        (define-key evil-normal-state-map (kbd "[F")
                    (lambda ()
                      (interactive)
                      (evil-textobj-tree-sitter-goto-textobj "function.outer" t t)))
        )

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

      (use-package evil-snipe
        :config
        (evil-snipe-override-mode 1)
        (general-define-key :states '(normal motion) :keymaps 'override "SPC ;" 'evil-snipe-s))

      )
  (progn ;; if not using evil
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
    )
  )

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
  (setq projectile-completion-system 'ivy)
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

      (use-package company-prescient
        :config
        (company-prescient-mode))

      ;; (use-package slime-company
      ;;   :after (slime company)
      ;;   :config
      ;;   (slime-setup '(slime-fuzzy slime-scratch slime-asdf 
      ;;                              slime-fancy
      ;;                              slime-company))
      ;;   (add-hook 'slime-mode-hook 		
      ;;             (lambda ()
      ;;               (company-mode 1)
      ;;               (set (make-local-variable 'company-backends)
      ;;                    '(company-slime))))
      ;;   (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
      ;;   (setq slime-company-completion 'fuzzy
      ;;         slime-company-after-completion 'slime-company-just-one-space))
      )
  (progn ;; corfu autocompletion
    (use-package corfu
      ;; :quelpa (:files (:defaults "extensions/*"))
      :init
      (global-corfu-mode)
      :custom
      (corfu-cycle t)
      (corfu-auto t)
      (corfu-quit-no-match t)
      (corfu-auto-delay 0)
      ;; (corfu-separator ?_) ;; Set to orderless separator, if not using space
      ;; (corfu-separator " ") ;; Set to orderless separator, if not using space
      (corfu-count 10)
      (corfu-indexed-mode t)
      (corfu-echo-mode t) ;; display brief documentation in echo area
      (corfu-popupinfo-mode t) ;; display documentation in popup
      (corfu-quit-at-boundary t)
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
;; (set-face-attribute 'default nil :family "Monaco" :height 120)
(set-face-attribute 'default nil :font "Cascadia Code" :weight 'light :height 110)
(set-face-attribute 'fixed-pitch nil :font "Cascadia Code" :weight 'light :height 110)
(set-face-attribute 'variable-pitch nil :font "Cascadia Code":weight 'light :height 1.1)
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
;; (switch-to-dark-theme)
;; (switch-to-light-theme)
;; (load-theme 'minimal-light t)
(load-theme 'doom-gruvbox-light t)
;; (load-theme 'darktooth t)
;; (load-theme 'ample-flat t)
;; (modus-themes-load-operandi)
;; stop org src blocks from bleeding in doom themes (remove background)
(set-face-attribute 'org-block nil :background nil)
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

;; helps figure out which key runs which function
(use-package command-log-mode
  :config
  (global-command-log-mode))

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
      (defun my/orderless-dispatch-flex-first (_pattern index _total)
        (and (eq index 0) 'orderless-flex))
      (defun my/lsp-mode-setup-completion ()
        (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
              '(orderless)))
      ;; Optionally configure the first word as flex filtered.
      ;; (add-hook 'orderless-style-dispatchers #'my/orderless-dispatch-flex-first nil 'local)
      ;; Optionally configure the cape-capf-buster.
      (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point)))
      :hook
      (lsp-completion-mode . my/lsp-mode-setup-completion)
      ))

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
(use-package eval-sexp-fu)

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
(use-package ivy-prescient
  :config
  (ivy-prescient-mode)
  (prescient-persist-mode 1)
  (setq prescient-history-length 10000)
  (setq prescient-save-file (file-truename (concat brain-path "emacs_prescient")))) ;; save history to filesystem

;; ;; auto indentation
;; (use-package aggressive-indent
;;   :config
;;   (aggressive-indent-global-mode))

;; ;; auto pairs insertion
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
  (add-to-list 'org-hugo-external-file-extensions-allowed-for-copying "webp"))

;; best latex preview functionality
;; actually i dont use this anymore since org-mode devs implemented a better method
(use-package xenops
  :config
  (setq xenops-reveal-on-entry t
        xenops-math-latex-max-tasks-in-flight 3
        xenops-math-latex-process 'dvisvgm)
  ;; (add-hook 'LaTeX-mode-hook #'xenops-mode)
  ;; (add-hook 'org-mode-hook #'xenops-mode)
  (add-hook 'xenops-mode-hook 'xenops-render)
  ;; (add-hook 'xenops-mode-hook 'xenops-xen-mode)
  (add-hook 'org-babel-after-execute-hook (lambda ()
                                            (interactive)
                                            (ignore-errors (xenops-render))))
  ;; (setq xenops-math-image-scale-factor 1.1)
  ;; (setq xenops-math-image-current-scale-factor 1.1)
  (setcar (cdr (car xenops-elements))
          '(:delimiters
            ("^[ 	]*\\\\begin{\\(align\\|equation\\|gather\\)\\*?}" "^[ 	]*\\\\end{\\(align\\|equation\\|gather\\)\\*?}")
            ("^[ 	]*\\\\\\[" "^[ 	]*\\\\\\]")))
  ;; for inline previews
  (advice-add 'xenops-math-latex-get-colors :filter-return
              (lambda (col)
                (interactive)
                (list (org-latex-color :foreground) (org-latex-color :background))))
                ;; (list (org-latex-color :foreground) (org-latex-color :background))))
  ;; for code blocks i think, i make backgrounds transparent so dont gotta set a color
  (plist-put org-format-latex-options :foreground "black")
  ;; (add-to-list 'xenops-math-latex-process-alist
  ;;              '(mymath :programs ("latex" "dvisvgm")
  ;;                       :description "pdf > svg"
  ;;                       :message "you need to install the programs: latex and dvisvgm (they come together in texlive distro)."
  ;;                       :image-input-type "dvi"
  ;;                       :image-output-type "svg"
  ;;                       :image-size-adjust (1.7 . 1.5)
  ;;                       :latex-compiler ("latex -output-directory %o %f")
  ;;                       :image-converter ("dvisvgm %f -o %O")))
  ;; this fixes issues with dvisvgm, phhhewww, took me a long time to figure out.
  (defun preamble-advice (document)
    (concat "\\def\\pgfsysdriver{pgfsys-tex4ht.def}" document))
  (advice-add 'xenops-math-latex-make-latex-document :filter-return 'preamble-advice)
  )

(use-package mixed-pitch
  :hook
  (text-mode . mixed-pitch-mode))

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

;; ;; more featureful ivy menus, it causes some error when switching buffers
;; (use-package ivy-rich
;;   :config
;;   (ivy-rich-mode 1)
;;   (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

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
(use-package org-ref
  :config
  (setq bibtex-completion-bibliography '("~/brain/bib.bib"))
  (setq org-cite-global-bibliography '("~/brain/bib.bib"))
  (define-key org-mode-map (kbd "C-c ]") 'org-ref-insert-link-hydra/body))
;; (use-package code-compass)

;; best terminal emulation
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
(use-package counsel-gtags)

(use-package git-auto-commit-mode)
;; (use-package avy)
;; (use-package auto-yasnippet)

;; i think this is similar to ivy-rich
;; (use-package marginalia
;;   :ensure t
;;   :config
;;   (marginalia-mode))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; (use-package ialign)
;; (use-package 'org-protocol-capture-html)
;; (use-package org-download)
;;(use-package org-ql)

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

(use-package tree-sitter
  :config
  (global-tree-sitter-mode 1))
(use-package tree-sitter-langs)

;(use-package json-to-org-table :quelpa (:host github :repo "noonker/json-to-org-table"))

(use-package all-the-icons-ivy-rich
  :config (all-the-icons-ivy-rich-mode 1))

(use-package lsp-java)

(use-package emmet-mode
  :config
  (add-hook 'web-mode-hook 'emmet-mode))

(use-package org-transclusion)

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
(use-package org-superstar
  :config
  (add-hook 'org-mode-hook 'org-superstar-mode))

(use-package linum-relative
  :config
  (add-hook 'prog-mode-hook 'linum-relative-mode)
  ;; show the real line number at current line
  (setq linum-relative-current-symbol ""))

(use-package org-super-agenda)

;; krita-supported manual drawing with org mode
(quelpa '(org-krita :fetcher github :repo "lepisma/org-krita" :files ("*.el" "resources")))
(add-hook 'org-mode-hook 'org-krita-mode)


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
        '((sbcl ("sbcl" "--dynamic-space-size" "10GB"))
          (clisp ("clisp"))
          (ecl ("ecl"))
          (cmucl ("cmucl"))
          (ccl ("ccl"))
          (maxima ("rmaxima" "-r" "to_lisp();"))))
  ;; make org babel use sly instead of slime
  (setq org-babel-lisp-eval-fn #'sly-eval))
;; (use-package slime
;;   :config
;;   (setq inferior-lisp-program "")
;;   (setq slime-lisp-implementations
;;         '((sbcl ("sbcl" "--dynamic-space-size" "10GB"))
;;           (clisp ("clisp"))
;;           (ecl ("ecl"))
;;           (maxima ("rmaxima" "-r" "to_lisp();")))))

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
;; (use-package embark)
;; (use-package svg-tag-mode)

;; (use-package alert)
;; (use-package olivetti)
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
(setq org-startup-with-inline-images t)
;; show images after evaluating code blocks.
(add-hook 'org-babel-after-execute-hook (lambda ()
                                          (interactive)
                                          (clear-image-cache)
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
;; (setq org-export-with-broken-links t)
;; thought org caching was the bottleneck for ox-hugo exports but it isnt, (wait, it apparently is.. but it isnt, as its just that a more recent version is the main cause)
;; (setq org-element-cache-persistent nil)
;; (setq org-element-use-cache nil)

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

(defun open-current-document ()
  "open the pdf of the current latex document that was generated"
  (interactive)
  (find-file-other-window (concat (get-latex-cache-dir-path) (current-filename) ".pdf")))
(defun open-current-document-this-window ()
  (interactive)
  (find-file (concat (get-latex-cache-dir-path) (current-filename) ".pdf")))

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
  (let ((my-file (ivy-completing-read "select file: " (directory-files-recursively directory-path regex))))
    (browse-url (expand-file-name my-file))))

(defun search-open-file-in-emacs (directory-path regex)
  "search for a file recursively in a directory and open it in emacs"
  (let ((my-file (ivy-completing-read "select file: " (directory-files-recursively directory-path regex))))
    (find-file (expand-file-name my-file) "'")))

;; automatically run script being edited, demonstrates how we can auto compile files on save
(defun run-script ()
  "run the current bash script being edited"
  (interactive)
  (run-command-show-output (buffer-file-name)))
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
(setq org-latex-image-default-width "")
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
         "CANCELLED(c@)"
         "CANCELED(C@)" ;; for backward compatibility
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
;; use dvisvgm instead of dvipng
(setq org-preview-latex-default-process 'dvisvgm) ;; inkscape is required (is it?) for .svg
;; dunno why \def\pgfsysdriver is needed (i think for htlatex)... gonna override the variable cuz that causes errors
(setq org-babel-latex-preamble
      (lambda (_)
        "\\documentclass[preview]{standalone}"))
;; to make gifs work
;; (setq org-format-latex-header (string-replace "{article}" "[tikz]{standalone}" org-format-latex-header))
;; (setq org-format-latex-header (string-replace "\\usepackage[usenames]{color}" "" org-format-latex-header))
;; (setq org-format-latex-header "\\documentclass[tikz]{standalone}")
(defun space-x-with-latex-header-hack ()
  (interactive)
  (let ((org-format-latex-header "\\documentclass[tikz]{standalone}"))
    (org-ctrl-c-ctrl-c)))
;; make org babel use dvisvgm instead of inkscape for pdf->svg, way faster and has many more advtanges over inkscape
(setq org-babel-latex-pdf-svg-process "dvisvgm --pdf %f -o %O")
;; latex syntax highlighting in org mode
(setq org-highlight-latex-and-related '(latex))
;; disable org-mode's mathjax because my blog's code uses another version
(setq org-html-mathjax-template "")
(setq org-html-mathjax-options '())
(setq org-babel-default-header-args:latex
      '((:results . "file graphics")
        (:exports . "results")
        ;; (:fit . t)
        ;; (:imagemagick . t)
        (:eval . "no-export")
        (:headers . ("\\usepackage{\\string~/.emacs.d/common}"))))
;; make org export deeply nested headlines as headlines still
(setq org-export-headline-levels 20)
;; workaround to make yasnippet expand after dollar sign in org mode
(add-hook 'org-mode-hook (lambda () (modify-syntax-entry ?$ "_" org-mode-syntax-table)))
;; startup with headlines and blocks folded
(setq org-startup-folded 'content)
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
            ))
;; disable multiplication precedence over division
(setq calc-multiplication-has-precedence nil)
;; stop org mode from moving tags far after headers
(setq org-tags-column 0)

(defun generate-random-string (NUM)
  "generate a random alphanumerics string of length NUM."
  (interactive "P")
  (setq random-str "")
  (let* (($charset "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
         ($baseCount (length $charset)))
    (dotimes (_ (if (numberp NUM) (abs NUM) NUM))
      (setq random-str (concat random-str (char-to-string (elt $charset (random $baseCount)))))))
  random-str)
(defun temp-file (EXT)
  (format "%stmp_%s.%s" (concat brain-path "out/") (generate-random-string 7) EXT))
(global-set-key (kbd "C-c R") (lambda () (interactive) (insert (generate-random-string 7))))

(defun switch-to-dark-theme ()
  "switch to dark theme"
  (interactive)
  (disable-theme 'minimal-light)
  (load-theme 'darktooth t)
  ;; (load-theme 'minimal t)
  ;; (set-face-attribute 'whitespace-space nil :background nil)
  ;; (set-face-attribute 'whitespace-newline nil :background nil)
  ;; (global-org-modern-mode)
  ;; (add-hook 'pdf-view-mode-hook 'pdf-view-themed-minor-mode)
  (set-themed-pdf 1))

(defun switch-to-light-theme ()
  "switch to light theme"
  (interactive)
  (disable-theme 'minimal)
  (load-theme 'minimal-light t)
  ;; (set-face-attribute 'whitespace-space nil :background nil)
  ;; (set-face-attribute 'whitespace-newline nil :background nil)
  ;; (global-org-modern-mode)
  ;; (set-face-background hl-line-face "PeachPuff3"))
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
;; make hugo page dates be the export date of the document
(defun org-date-advice (whatever-time)
  (org-format-time-string (cdr org-time-stamp-formats)))
(advice-add 'org-hugo--get-date :filter-return 'org-date-advice)

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

;; idea adapted from https://d12frosted.io/posts/2021-01-16-task-management-with-roam-vol5.html
(add-to-list 'org-tags-exclude-from-inheritance "todo")
(add-to-list 'org-tags-exclude-from-inheritance "band")
(defun buffer-contains-todo ()
  "check if the buffer contains a TODO entry"
  (org-element-map
      (org-element-parse-buffer 'headline)
      'headline
    (lambda (h)
      (eq (org-element-property :todo-type h) 'todo))
    ;; (or (eq (org-element-property :todo-type h)
    ;;         'todo)
    ;;     (eq (org-element-property :todo-type h)
    ;;         'done)))
    nil 'first-match))
(add-hook 'before-save-hook #'update-todo-tag)
(defun update-todo-tag ()
  "remove/add the todo tag to the buffer by checking whether it contains a TODO entry"
  (when (and (not (active-minibuffer-window))
             (is-buffer-roam-note))
    (save-excursion
      (goto-char (point-min))
      (if (buffer-contains-todo)
          (org-roam-tag-add '("todo"))
        (ignore-errors (org-roam-tag-remove '("todo"))))))
  (agenda-files-update))
(defun is-buffer-roam-note ()
  "Return non-nil if the currently visited buffer is a note."
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
(setq org-startup-with-latex-preview t)
(setq org-latex-preview-preamble "\\documentclass{article}\n[DEFAULT-PACKAGES]\n[PACKAGES]\n\\usepackage{xcolor}\n\\usepackage{\\string\~/.emacs.d/common}") ;; use my ~/.emacs.d/common.sty
;; export to html using dvisvgm aswell
(setq org-html-with-latex 'dvisvgm)
;; not sure why org-mode 9.7-pre dev branch doesnt respect global visual line mode so imma add this for now
(add-hook 'org-mode-hook 'visual-line-mode)

(defun go-through-all-roam-files (callback)
  "run a callback function on each file in the org-roam database"
  (dolist (file (all-roam-files))
    (if (not (eq callback nil))
        (with-current-buffer (or (find-buffer-visiting file) (find-file-noselect file))
          (if (is-buffer-roam-note)
              (funcall callback))))))

(defun go-through-roam-files-with-tag (tag-name callback)
  "run a callback function on each file tagged with tag-name"
  (dolist (file (roam-files-with-tag tag-name))
    (if (not (eq callback nil))
        (with-current-buffer (or (find-buffer-visiting file) (find-file-noselect file))
          (if (is-buffer-roam-note)
              (funcall callback))))))

(defun lob-reload ()
  "load files tagged with 'code' into the org babel library, also execute them"
  (interactive)
  (go-through-roam-files-with-tag
   "code"
   (lambda ()
     (org-babel-execute-buffer)
     (org-babel-lob-ingest (buffer-file-name)))))
;; most/all of my code files are lisp, load them with sly
(add-hook 'sly-connected-hook 'lob-reload)
;; (lob-reload)

(defun xenops-prerender ()
  "prerender latex blocks in roam files"
  (interactive)
  (go-through-roam-files-with-tag
   "math"
   (lambda ()
     (message "processing math file %s" (buffer-file-name)))))

(defun run-all-code-blocks ()
  "run code blocks in all org-roam files"
  (interactive)
  (go-through-all-roam-files
   (lambda ()
     (message "processing file %s" (buffer-file-name))
     (org-babel-execute-buffer))))

;; (go-through-roam-files-with-tag "math" (lambda () (message buffer-file-name)))
(defun publicize-files ()
  (interactive)
  (go-through-roam-files-with-tag
   "computer-science"
   (lambda ()
     (org-roam-tag-add '("public"))
     (save-buffer))))

(defun buffer-contains-substring (string)
  "check if the current buffer contains a specific string"
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (not (eq nil (search-forward string nil t))))))

(defun buffer-contains-math ()
  "check if the current buffer contains any math equations (latex blocks)"
  (buffer-contains-substring "$"))

(defmacro save-buffer-modified-p (&rest body)
  "Eval BODY without affected buffer modification status"
  `(let ((buffer-modified (buffer-modified-p))
         (buffer-undo-list t))
     (unwind-protect
         ,@body
       (set-buffer-modified-p buffer-modified))))

(defun update-math-file ()
  "add/remove the math tag to the file"
  (when (and (not (active-minibuffer-window))
             (is-buffer-roam-note))
    (save-excursion
      (goto-char (point-min))
      (if (buffer-contains-math)
          (org-roam-tag-add '("math"))
        (ignore-errors (org-roam-tag-remove '("math")))))))
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
(add-hook 'org-mode-hook 'org-babel-fold-all-latex-src-blocks)

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
  "force removal of project at point, even if its the last one"
  (treemacs-do-remove-project-from-workspace (treemacs-project-at-point) t))

;; TODO: add latex auto-completion to org-mode, requires auctex
;; (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point)))
;; (TeX--completion-at-point
;;  t
;;  LaTeX--arguments-completion-at-point)