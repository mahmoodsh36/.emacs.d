
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
(global-linum-mode 1)
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
;; disable new line at the end
;; dunno if the 1st line is necessary
;; (setq mode-require-final-newline nil)
;; (setq require-final-newline nil)
;; disable cursor blink
(blink-cursor-mode 0)
;; treat underscore as part of word
(defun underscore-part-of-word-hook ()
  (modify-syntax-entry ?_ "w"))
(add-hook 'prog-mode-hook 'underscore-part-of-word-hook)
;; kill buffer and window shortcut
(global-set-key (kbd "C-x K") 'kill-buffer-and-window)
;; highlight current line
(global-hl-line-mode)
;; try awesomewm config
(defun try-awesome-config ()
  (interactive)
  (shell-command "Xephyr :5 & sleep 1 ; DISPLAY=:5 awesome"))

;; js2-mode as major mode for javascript
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; shortcuts
(defun open-config-file ()
  (interactive)
  (find-file user-init-file))
(global-set-key (kbd "C-c e") 'open-config-file)

;; handling large files, not very helpful tbh, still slow when loading images
(defun my-find-file-check-make-large-file-read-only-hook ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) 100000)
    (message "File is large, entering read-only mode")
    (setq buffer-read-only t)
    (setq-default bidi-display-reordering nil)
    (buffer-disable-undo)))
;; (fundamental-mode)))
(add-hook 'find-file-hook 'my-find-file-check-make-large-file-read-only-hook)

;; evil-mode
(use-package evil
  :ensure t
  :config
  (evil-mode 1))

;; smooth scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don"t accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; relative numbering
(use-package linum-relative
  :ensure t
  :config
  (linum-relative-mode)
  ;; show current line number not '0'
  (setq linum-relative-current-symbol ""))

;; theme
;; (setq spacemacs-theme-comment-bg nil)
;; (setq spacemacs-theme-comment-italic 1)
;; (use-package spacemacs-theme
;;   :ensure t
;;   :defer t
;;   :init (load-theme 'spacemacs-dark t))
(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox t))

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
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
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
  (require 'evil-magit)
  (evil-define-key evil-magit-state magit-mode-map "?" 'evil-search-backward))

;; projectile
(use-package projectile
  :ensure t
  :config
  (setq projectile-completion-system 'ivy)
  (setq projectile-project-search-path '("~/workspace/" "~/"))
  (projectile-mode +1)
  ;; (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; ivy for projectile
(use-package ivy
  :ensure t)

;; eyebrowse for workspaces
(use-package eyebrowse
  :ensure t
  :config
  (eyebrowse-mode t)
  (eyebrowse-setup-opinionated-keys))

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
  (global-set-key (kbd "C-c c") 'company-complete))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package evil-collection
  :ensure t
  :config
  (evil-collection-init))

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

;; (use-package rainbow-mode
;;   :ensure t)

;; eshell

;; clear command to clear the eshell buffer.
(defun eshell/clear ()
  (let ((eshell-buffer-maximum-lines 0)) (eshell-truncate-buffer)))
(defun eshell-define-clear-command ()
  (interactive)
  (defun eshell/clear ()
    (let ((eshell-buffer-maximum-lines 0)) (eshell-truncate-buffer))))
(add-hook 'eshell-load-hook #'eshell-define-clear-command)

;; aliases
(defalias 'e 'find-file)
(defalias 'openo 'find-file-other-window)
(defalias 'try-awesome-config '(shell-command "Xephyr :5 & sleep 1 ; DISPLAY=:5 awesome"))

;; function to refactor json files
(defun beautify-json ()
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
          '(85 . 50) '(100 . 100)))))
 (global-set-key (kbd "C-c t") 'toggle-transparency)

;; Set transparency of emacs
(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (rainbow-mode emmet-mode zenburn-theme use-package treemacs-projectile treemacs-magit treemacs-icons-dired treemacs-evil spacemacs-theme skewer-mode rainbow-delimiters powerline lua-mode linum-relative ivy helm gruvbox-theme fzf flycheck eyebrowse evil-surround evil-numbers evil-magit evil-collection dracula-theme doom-themes doom-modeline company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
