;; get rid of the stupid startup screen
(setq inhibit-startup-screen t)
;; fix undo-tree package not found error
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq package-check-signature nil)
;; install use-package if not installed
;; package archives
(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/")
             '("gnu" . "http://elpa.gnu.org/packages/"))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; set tabs to 4 spaces, 2 for javascript
(setq-default tab-width 4)
(setq js-indent-level 2)
(setq-default indent-tabs-mode nil)
;; set line numbers
;; (global-linum-mode 1)
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
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))
;; kill current buffer without prompt
(global-set-key (kbd "C-x k") 'kill-this-buffer)
;; disable cursor blink
(blink-cursor-mode 0)
;; treat underscore as part of word
(defun underscore-part-of-word-hook ()
  (modify-syntax-entry ?_ "w"))
(add-hook 'prog-mode-hook 'underscore-part-of-word-hook)
;; kill buffer and window shortcut
(global-set-key (kbd "C-x K") 'kill-buffer-and-window)
;; highlight current line
;; (global-hl-line-mode)
;; try awesomewm config
(defun try-awesome-config ()
  (interactive)
  (shell-command "Xephyr :5 & sleep 1 ; DISPLAY=:5 awesome"))
;; shortcut to open new eshell buffer
(global-set-key (kbd "C-c s") 'eshell)
;; zap-up-to-char not zap-to-char
(global-set-key (kbd "M-z") 'zap-up-to-char)
;; reload file automatically
(global-auto-revert-mode t)
;; enable all disabled commands
(setq disabled-command-function nil)

;; no damn fringes dude!
(set-fringe-style 0)

;; js2-mode as major mode for javascript - disabled some keybindings idk why
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; (add-hook 'js2-mode-hook 'js2-mode-hide-warnings-and-errors)
;; (add-hook 'js2-mode-hook 'define-window-switching-keys)

;; shortcuts
(defun open-config-file ()
  (interactive)
  (find-file user-init-file))
(global-set-key (kbd "C-c e") 'open-config-file)

;; handling large files, not very helpful tbh, still slow when loading images

;; ;; evil-mode
(use-package evil
  :ensure t
  :config
  (evil-mode 1))

;; smooth scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don"t accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

;; relative numbering
;; (use-package linum-relative
;;   :ensure t
;;   :config
;;   (linum-relative-mode)
;;   ;; show current line number not '0'
;;   (setq linum-relative-current-symbol ""))

;; theme
;; (setq spacemacs-theme-comment-bg nil)
;; (setq spacemacs-theme-comment-italic 1)
;; (setq spacemacs-theme-keyword-italic 1)
;; (use-package spacemacs-theme
;;   :ensure t
;;   :defer t
;;   :init (load-theme 'spacemacs-dark t))
;; (use-package gruvbox-theme
;;   :ensure t
;;   :config
;;   (load-theme 'gruvbox t))
;; (use-package zenburn-theme
;;   :ensure t
;;   :config
;;   (load-theme 'zenburn t))
(use-package monokai-theme
  :ensure t
  :config
  (load-theme 'monokai t))

;; helm
(use-package helm
  :ensure t
  :config
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f") #'helm-find-files))

;; treemacs
(use-package treemacs
  :ensure t
  :defer t
  :init
  (global-set-key (kbd "C-0") 'treemacs-select-window)
  :config
  (treemacs-resize-icons 15)
  (setq treemacs-width 25)
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))
(use-package treemacs-evil
  :after treemacs evil
  :ensure t)
(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)
(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))
(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

;; magit
(use-package magit
  :ensure t)
(use-package evil-magit
  :ensure t
  :config
  (require 'evil-magit))
  ;; (evil-define-key evil-magit-state magit-mode-map "?" 'evil-search-backward))

;; projectile
(use-package projectile
  :ensure t
  :config
  (setq projectile-completion-system 'ivy)
  (setq projectile-project-search-path '("~/workspace/" "~/"))
  (projectile-mode +1)
  ;; (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-globally-ignored-files (append '("*.py" "*.o" "*.so") projectile-globally-ignored-files)))

;; ivy for projectile
(use-package ivy
  :ensure t)

;; evil-surround for evil mode
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

;; fuzzy finder for emacs
(use-package fzf
  :ensure t)

;; vim-like increase and decrease numbers
(use-package evil-numbers
  :ensure t
  :config
  (global-set-key (kbd "C-c +") 'evil-numbers/inc-at-pt)
  (global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt))

(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (global-set-key (kbd "M-/") 'company-complete-common-or-cycle)
  (setq company-idle-delay 0)
  (setq company-require-match nil)
  (eval-after-load 'company
    '(progn
       (define-key company-active-map (kbd "TAB") 'company-complete-selection)
       (define-key company-active-map [tab] 'company-complete-selection))))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; (use-package evil-collection
;;   :ensure t
;;   :config
;;   (evil-collection-init))

;; dunno if async is useful for me tbh
(use-package async
  :ensure t
  :config
  (autoload 'dired-async-mode "dired-async.el" nil t)
  (dired-async-mode 1)
  (async-bytecomp-package-mode 1))

(use-package lua-mode
  :ensure t
  :config
  (setq lua-indent-level 2))

(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'css-mode-hook  'emmet-mode)) ;; enable Emmet's css abbreviation.

(use-package skewer-mode
  :ensure t
  :config
  (add-hook 'js2-mode-hook 'skewer-mode)
  (add-hook 'css-mode-hook 'skewer-css-mode)
  (add-hook 'html-mode-hook 'skewer-html-mode))

;; (use-package flycheck
;;   :config
;;   (global-flycheck-mode)
;;   (setq flycheck-checker-error-threshold 4000))

;; C/C++ packages
(use-package irony
  :ensure t)
(use-package company-irony
  :ensure t)
(use-package flycheck-irony
  :ensure t)
(use-package company-irony-c-headers
  :ensure t)

;; (use-package rainbow-mode
;;   :ensure t)

;; (use-package centaur-tabs
;;   :ensure t
;;   :config
;;   (centaur-tabs-mode t)
;;   :bind
;;   ("C-j" . centaur-tabs-backward)
;;   ("C-k" . centaur-tabs-forward))

(use-package expand-region
  :ensure t
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

;; (use-package popwin
;;   :ensure t
;;   :config
;;   (require 'popwin)
;;   (popwin-mode 1)
;;   (push '(dired-mode :position top) popwin:special-display-config)
;;   (push '("*eshell*" :position bottom :stick non-nil) popwin:special-display-config))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
(use-package evil-org
  :ensure t)

(use-package lsp-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'lsp)
  (remove-hook 'html-mode-hook #'lsp))

(use-package company-lsp
  :ensure t)

;; (use-package lsp-java
;;   :ensure t
;;   :config
;;   (define-key java-mode-map (kbd "C-c i") 'lsp-java-organize-imports)
;;   (define-key java-mode-map (kbd "C-c g") 'lsp-java-generate-getters-and-setters))

(use-package avy
  :ensure t
  :config
  (global-set-key (kbd "C-;") 'avy-goto-char))

(use-package doom-modeline
      :ensure t
      :config
      (doom-modeline-mode))

;; (use-package indent-guide
;;   :ensure t
;;   :config
;;   (indent-guide-global-mode)
;;   (setq indent-guide-recursive t))

(use-package yasnippet
  :ensure t)

;; function to refactor json files
(defun beautify-json ()
  "Function to beautify current buffer considering it is in json format."
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
     "python -mjson.tool" (current-buffer) t)))

;; transparency
(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(92 . 92) '(100 . 100)))))
 (global-set-key (kbd "C-c t") 'toggle-transparency)
;; Set transparency of emacs
(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))
(transparency 90)

;; start server
(server-start)

;; my hooks
(defun run-project ()
    "compile the project using compile script compile.sh"
    (interactive)
    (call-process-shell-command (concat (concat "cd " (projectile-project-root)) " && ./compile && ./run") nil 0))
(global-set-key (kbd "C-c r") 'run-project)

;; change region highlight color
(set-face-attribute 'region nil :background "#000")
(set-frame-font "Inconsolata 11" nil t)

;; ;; workspaces
;; (setq current-workspace-index 1)
;; (setq workspaces '())
;; (defun switch-to-workspace (workspace-index)
;;   "switch to a workspace"
;;   (interactive)
;;   (let (workspace) (nth workspace-index workspaces)
;;         (progn
;;           (setcar (nthcdr current-workspace-index workspaces) (current-window-configuration))
;;           (setq current-workspace-index workspace-index)
;;           (if (window-configuration-p workspace)
;;               (set-window-configuration workspace)
;;             (delete-other-windows)))))
