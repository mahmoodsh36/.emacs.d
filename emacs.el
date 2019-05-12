(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ace-window-display-mode t)
 '(blink-cursor-alist nil)
 '(blink-cursor-mode nil)
 '(custom-safe-themes
   (quote
    ("01e067188b0b53325fc0a1c6e06643d7e52bc16b6653de2926a480861ad5aa78" "73a13a70fd111a6cd47f3d4be2260b1e4b717dbf635a9caee6442c949fad41cd" "8d5f22f7dfd3b2e4fc2f2da46ee71065a9474d0ac726b98f647bc3c7e39f2819" "721bb3cb432bb6be7c58be27d583814e9c56806c06b4077797074b009f322509" "946e871c780b159c4bb9f580537e5d2f7dba1411143194447604ecbaf01bd90c" "b59d7adea7873d58160d368d42828e7ac670340f11f36f67fa8071dbf957236a" default)))
 '(delete-selection-mode t)
 '(package-selected-packages (quote nil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#282828" :foreground "#fdf4c1" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 80 :width normal :foundry "ADBO" :family "Source Code Pro")))))

;; make emacs save backup files to ~/.saves
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))

;; add line numbers
(global-linum-mode t)

;; enable smoooth scrolling
;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; setting up use-package
;; keep links in http protocol not https, https causes emacs trouble no idea why
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
;; require use-package after setting the melpa repo no idea why
(require 'use-package)

(use-package powerline
  :ensure t
  :config
  (powerline-center-theme))

;; gruvbox theme
(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox t))

;; ace-window
(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "M-o") 'ace-window)
  (ace-window-display-mode)
  (setq aw-dispatch-always t))

;; helm
(use-package helm
  :ensure t
  :config
  (require 'helm-config)
  (helm-mode 1)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
  (global-set-key (kbd "C-x C-f") 'helm-find-files))

;; relative numbers
(use-package linum-relative
  :ensure t
  :config
  (require 'linum-relative)
  (linum-relative-mode))

;; command logs
(use-package command-log-mode
  :ensure t)

(use-package idle-highlight-mode
  :ensure t
  :config ;; last lines of this config i don't understand
  (require 'idle-highlight-mode)
  (setq idle-highlight-idle-time 0.1)
  (defun idle-highlight-hook () (idle-highlight-mode t))
  (mapc (lambda (m) (add-hook m 'idle-highlight-hook))
        '(emacs-lisp-mode-hook js2-mode-hook ruby-mode-hook objc-mode-hook)))

(use-package airline-themes
  :ensure t
  :config
  (require 'airline-themes)
  (load-theme 'airline-wombat))

;; (use-package evil
;;   :ensure t
;;   :config
;;   (require 'evil)
;;   (evil-mode 1))

;; enable parenthases coloring and matching
(show-paren-mode 1)
;; highlight current line
(global-hl-line-mode +1)
;; save state on exit
(desktop-save-mode 1)
;; zap up to char without char
(global-set-key "\M-z" 'zap-up-to-char)
;; overwrite selection on yanking or whatever
(delete-selection-mode)

;; use only spaces, screw tabs
(setq-default indent-tabs-mode nil)

;; screw gui buttons, remove all accessories
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; keybindings to scroll screen without cursor
(global-set-key "\M-n" "\C-u1\C-v")
(global-set-key "\M-p" "\C-u1\M-v")
