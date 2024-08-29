;; (set-face-attribute 'default nil :family "Comic Sans ms" :height 120)
;; (set-face-attribute 'default nil :family "Cascadia Code" :height 130)
;; (set-face-attribute 'default nil :family "Iosevka" :height 130)
;; (set-face-attribute 'default nil :family "Monaco" :height 120)
(ignore-errors
  (set-face-attribute 'default nil :font "inconsolata" :weight 'normal :height 130)
  (set-face-attribute 'fixed-pitch nil :font "inconsolata" :weight 'normal :height 130)
  (set-face-attribute 'variable-pitch nil :font "inconsolata" :weight 'normal :height 1.3)
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
                      :inherit 'unspecified))

(defun my-org-fontification ()
  (font-lock-add-keywords 'org-mode '(("^#\\+[^\s\n:]*" 0 font-lock-keyword-face))) ;; #+keyword
  (font-lock-add-keywords 'org-mode '(("\s\\(:[a-zA-Z]+\\)" 1 font-lock-keyword-face))) ;; :keyword
  (font-lock-add-keywords 'org-mode (list (list org-link-any-re 0 'font-lock-keyword-face))) ;; [[link]]
  (font-lock-add-keywords 'org-mode '(("\\[cite[^\\[]+\\]" 0 font-lock-keyword-face))) ;; [cite..]
  ;; (setq-default font-lock-multiline t) ;; needed for the following regex to work
  ;; (font-lock-add-keywords 'org-mode (list (cons "\\\\begin{\\([a-zA-Z]+\\)}\\(.\\|\n\\)*?\\\\end{\\1}" 'font-lock-keyword-face))) ;; latex env, doesnt work..

  (font-lock-add-keywords 'org-mode '(("\\\\\\[.*?\\\\]" 0 font-lock-string-face))) ;; to highlight display math
  (font-lock-add-keywords 'org-mode '(("\\\\(.*?\\\\)" 0 font-lock-string-face))) ;; to highlight inline math

  (font-lock-add-keywords 'org-mode '(("<<<.*?>>>" 0 font-lock-keyword-face)))

  (defun my-latex-env-fontlock (bound)
    "depends on blk and auctex, for highlighting latex blocks."
    (let* ((origin-point (point))
           (env-bounds (ignore-errors (blk-auctex-env-at-point-bounds)))
           (begin (car env-bounds))
           (end (cdr env-bounds)))
      (when (not env-bounds)
        (ignore-errors
          (search-forward "\\begin{")
          ;; (goto-char (match-beginning 0)) (forward-char)
          (setq env-bounds (ignore-errors (blk-auctex-env-at-point-bounds))
                begin (car env-bounds)
                end (cdr env-bounds))))
      (if env-bounds
          (progn
            (when (> end bound) (setq end bound))
            (when (< begin origin-point) (setq begin origin-point))
            (goto-char (1+ end))
            (set-match-data (list begin end))
            t)
        (progn (goto-char (1+ origin-point))
               nil))))
  (font-lock-add-keywords 'org-mode (list (list 'my-latex-env-fontlock 0 'font-lock-string-face))))

;; to get rid of the fontification done by org, its horrible.., this also gets rid of some lag, i think..
(defconst use-my-org-fontification t)
(with-eval-after-load-all
 '(org ol)
 (when use-my-org-fontification
   (defun org-set-font-lock-defaults ()
     )
   (my-org-fontification))
 )

(defvar my-current-theme nil)
(add-to-list 'savehist-additional-variables 'my-current-theme)
(defun switch-to-theme (theme)
  "remove current theme, switch to another"
  (when (car custom-enabled-themes)
    (disable-theme (car custom-enabled-themes)))
  (load-theme theme t)
  (setq my-current-theme theme)
  ;; (clear-face 'org-block)
  ;; (clear-face 'org-block-begin-line)
  ;; (clear-face 'org-block-end-line)
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