(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ace-window-display-mode t)
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(blink-cursor-alist nil)
 '(blink-cursor-mode nil)
 '(custom-enabled-themes (quote (spacemacs-dark)))
 '(custom-safe-themes
   (quote
    ()))
 '(delete-selection-mode t)
 '(eshell-output-filter-functions
   (quote
    (eshell-handle-control-codes eshell-handle-ansi-color eshell-watch-for-password-prompt)))
 '(package-selected-packages
   (quote
    (spacemacs-theme exwm expand-region use-package linum-relative idle-highlight-mode helm goto-chg command-log-mode airline-themes)))
 '(pixel-scroll-mode t)
 '(scroll-bar-mode nil))
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
(setq scroll-conservatively 10000) ;; i think this line fixes the random jumps
(setq auto-window-vscroll nil)

;; setting up use-package
;; keep links in http protocol not https, https causes emacs trouble no idea why
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(package-initialize)
;; require use-package after setting the melpa repo no idea why
(require 'use-package)

(use-package powerline
  :ensure t
  :config
  (powerline-center-theme))

;; gruvbox theme
(use-package gruvbox-theme
  :ensure t)
  ;; :config
  ;;(load-theme 'gruvbox t))

;; ace-window
;; (use-package ace-window
;;   :ensure t
;;   :config
;;   (global-set-key (kbd "M-o") 'ace-window)
;;   (ace-window-display-mode)
;;   (setq aw-dispatch-always t))

;; helm7
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
;; (use-package command-log-mode
;;   :ensure t)

(use-package idle-highlight-mode
  :ensure t
  :config ;; last lines of this config i don't understand
  (require 'idle-highlight-mode)
  (setq idle-highlight-idle-time 0.1)
  (defun idle-highlight-hook () (idle-highlight-mode t))
  (mapc (lambda (m) (add-hook m 'idle-highlight-hook))
        '(emacs-lisp-mode-hook js2-mode-hook ruby-mode-hook objc-mode-hook)))

(use-package airline-themes
  :ensure t)
  ;; :config
  ;; (require 'airline-themes)
  ;; (load-theme 'airline-kolor))

(use-package expand-region
  :ensure t
  :config
  (require 'expand-region)
  (global-set-key (kbd "C-;") 'er/expand-region))

(use-package exwm
  :ensure t
  :config
  (require 'exwm)
  (setq exwm-workspace-number 4)
  ;; start media keys config
  (exwm-input-set-key (kbd "<XF86AudioRaiseVolume>")
                      (lambda ()
                        (interactive)
                        (start-process-shell-command  "amixer set Master 10%+" nil
                                                      "amixer set Master 10%+")))
  (exwm-input-set-key (kbd "<XF86AudioLowerVolume>")
                      (lambda ()
                        (interactive)
                        (start-process-shell-command  "amixer set Master 10%-" nil
                                                      "amixer set Master 10%-")))
  (exwm-input-set-key (kbd "<XF86AudioPlay>")
                      (lambda ()
                        (interactive)
                        (start-process-shell-command  "playerctl play-pause" nil
                                                      "playerctl play-pause")))
  (exwm-input-set-key (kbd "<XF86AudioNext>")
                      (lambda ()
                        (interactive)
                        (start-process-shell-command  "playerctl next" nil
                                                      "playerctl next")))
  (exwm-input-set-key (kbd "<XF86AudioPrev>")
                      (lambda ()
                        (interactive)
                        (start-process-shell-command  "playerctl previous" nil
                                                      "playerctl previous")))
  ;; end media keys config
  (setq exwm-input-global-keys `(
                                 ([?\s-l] . windmove-right)
                                 ([?\s-h] . windmove-left)
                                 ([?\s-k] . windmove-up)
                                 ([?\s-j] . windmove-down)
                                 ([?\s-r] . exwm-reset)
                                 ([?\s-w] . exwm-workspace-switch)
                                 ,@(mapcar (lambda (i)
                                              `(,(kbd (format "s-%d" i)) .
                                                (lambda ()
                                                  (interactive)
                                                  (exwm-workspace-switch-create ,i))))
                                            (number-sequence 0 9))
                                 ([?\s-&] . (lambda (command)
                                              (interactive (list (read-shell-command "$ ")))
                                              (start-process-shell-command command nil command)))
                                 ([s-f2] . (lambda ()
                                             (interactive)
                                             (start-process "" nil "usr/bin/slock")))))
  (setq exwm-input-simulation-keys
      '(([?\C-b] . [left])
        ([?\C-f] . [right])
        ([?\C-p] . [up])
        ([?\C-n] . [down])
        ([?\C-a] . [home])
        ([?\C-e] . [end])
        ([?\M-v] . [prior])
        ([?\C-v] . [next])
        ([?\C-d] . [delete])
        ([?\C-k] . [S-end delete])))
  (exwm-enable))

;; xelb for exwm
(use-package xelb
  :ensure t
  :config
  (require 'xelb))

;; (use-package evil
;;   :ensure t
;;   :config
;;   (require 'evil)
;;   (evil-mode 1))

(use-package spacemacs-theme
  :ensure t
  :defer t ;; dont require spacemacs-theme, it doesn't exist
  :init
  (load-theme 'spacemacs-dark t)
  (powerline-reset))

(use-package spaceline
  :ensure t
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme))

;; enable parenthases coloring and matching
(show-paren-mode 1)
;; highlight current line
(global-hl-line-mode +1)
;; save state on exit
;; (desktop-save-mode 1)
;; zap up to char without char
(global-set-key "\M-z" 'zap-up-to-char)
;; overwrite selection on yanking or whatever
(delete-selection-mode)
;; insert newlines if the point is at the end of the buffer
;; (setq next-line-add-newlines t)

;; use only spaces, screw tabs
(setq-default indent-tabs-mode nil)

;; screw gui buttons, remove all accessories
(menu-bar-mode -1)
(menu-bar-no-scroll-bar)
(tool-bar-mode -1)

;; keybindings to scroll screen without cursor
(global-set-key "\M-n" "\C-u1\C-v")
(global-set-key "\M-p" "\C-u1\M-v")

;; start server when emacs loads
(server-start)

;; function that kills all buffers and windows except current one
(defun kill-other-windows ()
      "Kill all other buffers."
      (interactive)
      (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
      (delete-other-windows))
(put 'erase-buffer 'disabled nil)

;; clear eshell terminal screen
(defun eshell-clear-buffer ()
  "Clear terminal"
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))
(add-hook 'eshell-mode-hook
      '(lambda()
          (local-set-key (kbd "C-l") 'eshell-clear-buffer)))
