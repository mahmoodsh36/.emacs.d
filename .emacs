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
             '("melpa" . "http://melpa.org/packages/")
             '("gnu" . "http://elpa.gnu.org/packages/"))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;(async-bytecomp-package-mode 1)
(require 'use-package)

;; set tabs to 4 spaces
(setq-default tab-width 2)
(setq js-indent-level 2)
(setq-default c-basic-offset 2)
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
(defun try-qtile-config ()
  (interactive)
  (shell-command "/home/mahmooz/workspace/dotfiles/.config/qtile/xephyr.sh 2>&1 > /dev/null"))
;; shortcut to open new eshell buffer
(global-set-key (kbd "C-c s") 'eshell)
;; zap-up-to-char not zap-to-char
(global-set-key (kbd "M-z") 'zap-up-to-char)
;; reload file automatically
(global-auto-revert-mode t)
;; enable all disabled commands
(setq disabled-command-function nil)
;; initial frame size
(when window-system (set-frame-size (selected-frame) 160 48))
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
   (lua . t)))

;; general keys
(global-set-key (kbd "C-M-S-x") 'eval-region)
(global-set-key (kbd "C-x D") 'image-dired)
(global-set-key (kbd "C-c f") 'find-function-at-point)

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

;; evil-mode
(setq evil-want-keybinding nil)
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode))
(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-tree)
  (evil-set-initial-state 'image-dired-thumbnail-mode 'emacs)
  (evil-set-initial-state 'dired-mode 'emacs)) ;; disable evil for dired

;; smooth scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don"t accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

;; relative numbering
(use-package linum-relative
  :ensure t
  :config
  (linum-relative-mode)
  ;; show current line number not '0'
  (setq linum-relative-current-symbol ""))

;; helm
(use-package helm
  :ensure t
  :config
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f") #'helm-find-files))

;; magit
(use-package magit
  :ensure t)
;;(use-package evil-magit
;;  :ensure t
;;  :config
;;  (require 'evil-magit))

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
;; (use-package ivy
;;   :ensure t)

;; evil-surround for evil mode
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

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

(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'css-mode-hook  'emmet-mode)) ;; enable Emmet's css abbreviation.

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
(use-package evil-org
  :ensure t)

;;(use-package lsp-mode
;;:ensure t
;;:config
;;(add-hook 'prog-mode-hook #'lsp)
;;(remove-hook 'html-mode-hook #'lsp))

;; ====== gruvbox
;;(use-package gruvbox-theme
;;  :ensure t
;;  :config
;;  (load-theme 'gruvbox))
;; ====== spacemacs
;;(setq spacemacs-theme-comment-bg nil)
;;(setq spacemacs-theme-comment-italic t)
;;(use-package spacemacs-theme
;;:defer t
;;:init (load-theme 'spacemacs-dark t))
(use-package almost-mono-themes
  :config
  (load-theme 'almost-mono-black t))

(use-package avy
  :ensure t
  :config
  (global-set-key (kbd "C-;") 'avy-goto-char))

;;(use-package evil-collection
;;  :after (evil)
;;  :config
;;  (setq evil-collection-mode-list '(dired))
;;  (evil-collection-init))

;; (use-package ein
;;   :ensure t)

(use-package dart-mode
  :ensure t)

(use-package web-mode
  :ensure t
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
  :ensure t
  :config
  (setq lua-indent-level 2))

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p)
  :config
  (unless (find-font (font-spec :name "all-the-icons"))
    (all-the-icons-install-fonts t)))

(use-package request
  :ensure t)

(use-package counsel
  :ensure t
  :config (ivy-mode t))

(use-package slime-company
  :ensure t)

(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "sbcl")
  (slime-setup '(slime-company)))

(use-package dumb-jump
  :ensure t
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package mmm-mode
  :ensure t)

(use-package rust-mode
  :ensure t)

;;(use-package racer
;;  :ensure t
;;  :config
;;  (add-hook 'rust-mode-hook #'racer-mode)
;;  (add-hook 'racer-mode-hook #'eldoc-mode))

(use-package rainbow-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-mode))

;;(use-package aggressive-indent
;;:ensure t
;;:config
;;(global-aggressive-indent-mode 1))

;;(use-package emojify
;;  :ensure t
;;  :hook (after-init . global-emojify-mode))

;; the emacs media player or whatever
(use-package emms
  :ensure t
  :config
  (require 'emms-setup)
  (emms-all)
  (emms-default-players))

;; helps figure out which key runs which function
(use-package command-log-mode
  :ensure t
  :config
  (global-command-log-mode))

;;(use-package vterm
  ;;:ensure t)

;; the silver searcher, an alternative to grep
(use-package ag
  :ensure t
  :config
  (global-set-key (kbd "C-c g") 'counsel-ag))

;; transmission-daemon client
(use-package transmission
  :ensure t)

;; save undos/redos even when buffer is killed or emacs restarts
(use-package undo-fu-session
  :ensure t
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (global-undo-fu-session-mode))

(use-package nix-mode
  :ensure t)
(use-package company-nixos-options
  :ensure t)
(use-package helm-nixos-options
  :ensure t)

(use-package math-symbol-lists
  :ensure t)

(use-package latex-extra
  :ensure t)

(use-package company-auctex
  :ensure t)

(use-package google-translate
  :ensure t)

(require 'tex-site)

;; function to refactor json files
(defun beautify-json ()
  "Function to beautify current buffer considering it is in json format."
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
                             "python -mjson.tool" (current-buffer) t)))

;; change region highlight color, set it to black,
;; makes things more visible
;;(set-face-attribute 'region nil :background "#000")
(set-frame-font "Fantasque Sans Mono 12" nil t)

;; start server
(server-start)

;; Set transparency of emacs
(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))
(transparency 90)

;; function to make printing easier for many languages
(defun current-line-to-empty-class ()
  (interactive)
  (if (string= major-mode "python-mode")
      (progn
        (back-to-indentation)
        (insert "class ")
        (end-of-line)
        (insert ":")
        (newline-and-indent)
        (insert "def __init__(self):")
        (newline-and-indent))))
(defun current-line-to-print-statement ()
    (interactive)
    (if (string= major-mode "python-mode")
        (progn
          (back-to-indentation)
          (insert "print(")
          (end-of-line)
          (insert ")")))
    (if (string= major-mode "c-mode")
        (progn
          (back-to-indentation)
          (insert "printf(")
          (end-of-line)
          (insert ");")))
    (if (string= major-mode "emacs-lisp-mode")
        (progn
          (back-to-indentation)
          (insert "(message ")
          (end-of-line)
          (insert ")"))))
(global-set-key (kbd "C-x p") 'current-line-to-print-statement)

;; c-x c-l to complete line like vim
(defun my-expand-lines ()
  (interactive)
  (let ((hippie-expand-try-functions-list
         '(try-expand-line)))
    (call-interactively 'hippie-expand)))
(define-key evil-insert-state-map (kbd "C-x C-l") 'my-expand-lines)

;; org mode configs
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(setq org-log-done 'time)

;; functions for working with images
(defun insert-image-from-file (filepath)
  "insert image from the given filepath"
  (interactive "f")
  (insert-image (create-image filepath nil nil :width 200)))

(defun insert-image-from-url (url width height)
  (interactive)
  (unless url (setq url (url-get-url-at-point)))
  (unless url
    (error "Couldn't find URL."))
  (let ((buffer (url-retrieve-synchronously url)))
    (unwind-protect
         (let ((data (with-current-buffer buffer
                       (goto-char (point-min))
                       (search-forward "\n\n")
                       (buffer-substring (point) (point-max)))))
           (insert-image (create-image data 'imagemagick t
                                       :max-width width :max-height height) "i"))
      (kill-buffer buffer))))

(defun insert-image-from-url-no-predefined-size (url)
  (interactive)
  (unless url (setq url (url-get-url-at-point)))
  (unless url
    (error "Couldn't find URL."))
  (let ((buffer (url-retrieve-synchronously url)))
    (unwind-protect
         (let ((data (with-current-buffer buffer
                       (goto-char (point-min))
                       (search-forward "\n\n")
                       (buffer-substring (point) (point-max)))))
           (insert-image (create-image data 'imagemagick t) "i"))
      (kill-buffer buffer))))

;; mouse scroll behavior
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;; set window size
(when window-system (set-frame-size (selected-frame) 160 45))

;; image viewing
(defun show-images-from-directory (directory-path)
  (dolist (file-path (directory-files directory-path t directory-files-no-dot-files-regexp))
    (if (file-regular-p file-path)
        (progn
          (setq image (create-image file-path nil nil :width 100))
          (if (not (eq image nil))
            (progn
              (insert-image image)
              (newline)))))))

(defun run-command-show-output ()
  (interactive)
  (setq cmd (read-from-minibuffer "$ "))
  (progn
    (start-process-shell-command cmd cmd cmd)
    (switch-to-buffer-other-window cmd)
    (evil-insert nil)
    (end-of-buffer)))
(global-set-key (kbd "C-x $") 'run-command-show-output)

;; dired file management
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
(setq dired-listing-switches "-l")

;; images
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

;;(progn
;;  (end-of-buffer)
;;  (newline)
;;  (show-images-from-directory "/home/mahmooz/"))

;; my config for latex
;; on save compile the document using pdflatex and put it in /tmp/
(defun current-filename ()
  (file-name-sans-extension
   (file-name-nondirectory (buffer-file-name))))
(defun compile-current-document ()
  (interactive)
  (call-process-shell-command (concat (concat "pdflatex -output-directory=/tmp " buffer-file-name) "&"))
  (message (concat "compiled " buffer-file-name)))
(defun launch-zathura-for-current-document ()
  (interactive)
  (call-process-shell-command (concat "zathura /tmp/" (concat (current-filename) ".pdf &"))))
(add-hook
 'LaTeX-mode-hook
 (lambda ()
   (compile-current-document)
   (add-hook 'after-save-hook 'compile-current-document 0 t)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("a0feb1322de9e26a4d209d1cfa236deaf64662bb604fa513cca6a057ddf0ef64" "04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" default))
 '(safe-local-variable-values
   '((eval modify-syntax-entry 43 "'")
     (eval modify-syntax-entry 36 "'")
     (eval modify-syntax-entry 126 "'")
     (eval let
           ((root-dir-unexpanded
             (locate-dominating-file default-directory ".dir-locals.el")))
           (when root-dir-unexpanded
             (let*
                 ((root-dir
                   (expand-file-name root-dir-unexpanded))
                  (root-dir*
                   (directory-file-name root-dir)))
               (unless
                   (boundp 'geiser-guile-load-path)
                 (defvar geiser-guile-load-path 'nil))
               (make-local-variable 'geiser-guile-load-path)
               (require 'cl-lib)
               (cl-pushnew root-dir* geiser-guile-load-path :test #'string-equal))))
     (eval setq-local guix-directory
           (locate-dominating-file default-directory ".dir-locals.el")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
