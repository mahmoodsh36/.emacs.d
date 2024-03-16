;; (set-face-attribute 'default nil :family "Comic Sans ms" :height 120)
;; (set-face-attribute 'default nil :family "Cascadia Code" :height 130)
;; (set-face-attribute 'default nil :family "Iosevka" :height 130)
;; (set-face-attribute 'default nil :family "Monaco" :height 120)
(ignore-errors
  (set-face-attribute 'default nil :font "Iosevka" :weight 'light :height 100)
  (set-face-attribute 'fixed-pitch nil :font "Iosevka" :weight 'light :height 100)
  (set-face-attribute 'variable-pitch nil :font "Fira Code" :weight 'normal :height 1.0)
  ;; this font makes hebrew text unreadable, gotta disable it
  (add-to-list 'face-ignored-fonts "Noto Rashi Hebrew")
  )
(use-package darktooth-theme)
(use-package modus-themes)
(use-package ample-theme)
(use-package anti-zenburn-theme)
(use-package zenburn-theme)
(use-package poet-theme)
(use-package gruvbox-theme)
(use-package doom-themes)
(use-package inkpot-theme)
(use-package ef-themes
  :elpaca
  (ef-themes :type git :host github :repo "protesilaos/ef-themes"))
;; (use-package minimal-theme
;;   :quelpa (:host github :repo "mahmoodsheikh36/minimal-theme"))
(use-package soothe-theme)
(use-package stimmung-themes
  ;; :config
  ;; (setq stimmung-themes-comment 'foreground)
  )
(use-package acme-theme)
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

(defun clear-face (face)
  (set-face-attribute face nil
                      ;; there are more attributes not specified here
                      :underline 'unspecified
                      :slant 'unspecified
                      :weight 'unspecified
                      :height 'unspecified
                      :family 'unspecified
                      :foreground 'unspecified
                      :background 'unspecified
                      :inherit 'src))

(with-eval-after-load 'org
  ;; to get rid of the block fontification done by org, its horrible..
  (defun org-fontify-meta-lines-and-blocks (_)
    )
  (font-lock-add-keywords 'org-mode '(("#\\+begin_.*\\|#\\+end_.*" 0 '(highlight :foreground 'cyan :background 'black))))

  ;; this doesnt work, why?
  ;; (font-lock-add-keywords 'org-mode '(("#\\+begin_" . font-lock-keyword-face)))
  ;; (font-lock-remove-keywords 'org-mode '(("#\\+begin_.*" . font-lock-keyword-face)))
  ;; (font-lock-add-keywords 'org-mode
  ;;                         '(("#\\+begin_.*" . (:foreground "cyan"))))
  )

(defun switch-to-theme (theme)
  "remove current theme, switch to another"
  (when (car custom-enabled-themes)
    (disable-theme (car custom-enabled-themes)))
  (load-theme theme t)
  (clear-face 'org-block)
  (clear-face 'org-block-begin-line)
  (clear-face 'org-block-end-line)
  ;; (custom-set-faces '(org-block ((t (:inherit 'src)))))
  ;; (set-face-attribute 'whitespace-space nil :background nil)
  ;; (set-face-attribute 'whitespace-newline nil :background nil)
  (set-themed-pdf t))

(defun switch-to-darktooth-theme ()
  "switch to dark theme"
  (interactive)
  (switch-to-theme 'darktooth-dark)
  ;; (load-theme 'soothe t)
  ;; (load-theme 'minimal t)
  ;; (set-face-attribute 'whitespace-space nil :background nil)
  ;; (set-face-attribute 'whitespace-newline nil :background nil)
  ;; (global-org-modern-mode)
  ;; (add-hook 'pdf-view-mode-hook 'pdf-view-themed-minor-mode)
  (set-themed-pdf t))

(defun switch-to-tango-theme ()
  "switch to dark theme"
  (interactive)
  (switch-to-theme 'tango)
  (set-face-background hl-line-face "PeachPuff3")
  (set-themed-pdf t))

(defun switch-to-gruvbox-light-theme ()
  "switch to light theme"
  (interactive)
  ;; (disable-theme 'doom-tomorrow-night)
  (disable-theme 'darktooth-dark)
  ;; (disable-theme 'soothe)
  (load-theme 'doom-gruvbox-light t)
  (set-face-attribute 'org-block nil :background nil)
  (set-face-attribute 'whitespace-space nil :background nil)
  (set-face-attribute 'whitespace-newline nil :background nil)
  ;; (global-org-modern-mode)
  ;; (set-face-background hl-line-face "PeachPuff3")
  ;; (remove-hook 'pdf-view-mode-hook 'pdf-view-themed-minor-mode)
  (set-themed-pdf t))

(defun set-themed-pdf (should-be-themed)
  "if 1 is passed the buffers with pdf files open will be themed using pdf-tools, unthemed if 0"
  (if should-be-themed
      (add-hook 'pdf-view-mode-hook 'pdf-view-themed-minor-mode)
    (remove-hook 'pdf-view-mode-hook 'pdf-view-themed-minor-mode))
  (dolist (buffer (buffer-list))
    (if (buffer-name buffer)
        (if (string-match ".*.pdf$" (buffer-name buffer))
            (with-current-buffer (buffer-name buffer)
              (message "%s" (buffer-name buffer) should-be-themed)
              (if should-be-themed
                  (progn
                    (pdf-view-themed-minor-mode 1)
                    (pdf-view-refresh-themed-buffer t))
                (pdf-view-themed-minor-mode -1)))))))

(provide 'setup-theme)