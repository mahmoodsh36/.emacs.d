(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("c616e584f7268aa3b63d08045a912b50863a34e7ea83e35fcab8537b75741956" "bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9" "")))
 '(package-selected-packages
   (quote
    (helm linum-relative idle-highlight-mode airline-themes powerline expand-region projectile magit darktooth-theme monokai-theme use-package)))
 '(scroll-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(package-initialize)

;; install use-package if not installed
(when (not (require 'use-package nil 'noerror))
  (package-refresh-contents)
  (package-initialize)
  (package-install 'use-package))
(require 'use-package)

;; load the org init file
(require 'org)
(org-babel-load-file
 (expand-file-name "config.org"
                   user-emacs-directory))
