(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3eb93cd9a0da0f3e86b5d932ac0e3b5f0f50de7a0b805d4eb1f67782e9eb67a4" "946e871c780b159c4bb9f580537e5d2f7dba1411143194447604ecbaf01bd90c" "88049c35e4a6cedd4437ff6b093230b687d8a1fb65408ef17bfcf9b7338734f6" "c616e584f7268aa3b63d08045a912b50863a34e7ea83e35fcab8537b75741956" "bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9" default)))
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
