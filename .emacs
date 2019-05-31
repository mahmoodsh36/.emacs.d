(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (spacemacs-dark)))
 '(custom-safe-themes
   (quote
    ("26d49386a2036df7ccbe802a06a759031e4455f07bda559dcf221f53e8850e69" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476")))
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (evil eclim moe-theme spacemacs-theme helm gruvbox-theme dracula-theme)))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

; list of packages
(setq package-list '(
		     org
		     helm
		     spacemacs-theme
		     evil))

;; repos containing packages
(setq
 package-enable-at-startup nil
 package-archives
 '(("melpa-stable" . "http://stable.melpa.org/packages/")
   ("melpa" . "http://melpa.org/packages/")
   ("marmalade"   . "http://marmalade-repo.org/packages/")
   ("gnu"         . "http://elpa.gnu.org/packages/")))

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; show matching parenthases
(show-paren-mode 1)

;; helm
(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M)

(global-set-key (kbd "C-x M-k") 'kill-buffer-and-window)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key [C-backspace] 'kill-whole-line)

;; window switching
(global-set-key (kbd "C-c l") 'windmove-right)
(global-set-key (kbd "C-c h") 'windmove-left)
(global-set-key (kbd "C-c j") 'windmove-down)
(global-set-key (kbd "C-c k") 'windmove-up)

;; spacemacs theme
(setq spacemacs-theme-keyword-italic 1)
(setq spacemacs-theme-comment-bg   nil)
(load-theme 'spacemacs-dark)

;; no more yes-or-no, y-or-n
(defalias 'yes-or-no-p 'y-or-n-p)

;; overwrite highlited text
(delete-selection-mode 1)

;; enable evil mode plz
(evil-mode 1)
