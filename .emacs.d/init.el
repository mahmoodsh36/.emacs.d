;; disable customization using the interactive interface
(setq custom-file "/dev/null")
;; get rid of the stupid startup screen
(setq inhibit-startup-screen t)

;; setup use-package
(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/")
             '("gnu" . "http://elpa.gnu.org/packages/"))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; set tabs to 2 spaces
(setq-default tab-width 2)
(setq-default js-indent-level 2)
(setq-default python-indent-offset 4)
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
(setq backup-directory-alist
      `((".*" . ,"~/.emacs.d/backup/")))
(setq auto-save-file-name-transforms
      `((".*" ,"~/.emacs.d/backup/" t)))
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
;; reload file automatically
(global-auto-revert-mode t)
;; enable all disabled commands
(setq disabled-command-function nil)
;; initial frame size
(when window-system (set-frame-size (selected-frame) 105 59))
;; enable which-function-mode that shows the current function being edited in the bottom bar
(add-hook 'prog-mode-hook 'which-function-mode)
;; key to start calc mode
(global-set-key (kbd "C-c c") 'calc)
;; no damn fringes dude!
(set-fringe-style 0)
;; set font
;;(set-frame-font "Fantasque Sans Mono 12" nil t)
;; display only buffer name in modeline
(setq-default mode-line-format (list " " mode-line-modified "%e %b"))

;; general keys
(global-set-key (kbd "C-M-S-x") 'eval-region)
(global-set-key (kbd "C-x D") 'image-dired)
(global-set-key (kbd "C-c f") 'find-function-at-point)

;; fix evil-mode issue with undo, this is required
(setq evil-want-keybinding nil)
(use-package undo-tree
  :config
  (global-undo-tree-mode))

;; evil-mode
(use-package evil
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-tree)
  (evil-set-initial-state 'image-dired-thumbnail-mode 'emacs)
  (define-key evil-insert-state-map (kbd "C-e") 'evil-scroll-line-down)
  (define-key evil-insert-state-map (kbd "C-y") 'evil-scroll-line-up))

;; evil-mode bindings here, after the package is installed
;; keybinding to quickly open config file
(define-key evil-normal-state-map (kbd "SPC e") (lambda () (interactive) (find-file user-init-file)))

;; smooth scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don"t accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

;; relative numbering
(use-package linum-relative
  :config
  (linum-relative-mode)
  ;; show current line number not '0'
  (setq linum-relative-current-symbol ""))

;; helm
(use-package helm
  :config
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f") #'helm-find-files))

;; projectile
(use-package projectile
  :config
  (setq projectile-completion-system 'ivy)
  (setq projectile-project-search-path '("~/workspace/"))
  (projectile-mode +1)
  ;; (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-globally-ignored-files (append '("*.py" "*.o" "*.so") projectile-globally-ignored-files)))

;; evil-surround for evil mode
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;; ide-like features
(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (global-set-key (kbd "M-/") 'company-complete-common-or-cycle)
  (setq company-idle-delay 0)
  (setq company-require-match nil)
  (eval-after-load 'company
    '(progn
       (define-key company-active-map (kbd "TAB") 'company-complete-selection)
       (define-key company-active-map [tab] 'company-complete-selection))))

;; colorful delimiters
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; nice bullets for headlines instead of just stars
(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; evil mode support for org
(use-package evil-org)

;; ====== gruvbox
(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox t))

;; support to make evil more compatible with the whole of emacs
(use-package evil-collection
  :after (evil)
  :config
  (setq evil-collection-mode-list '(dired)) ;; enable for dired
  (evil-collection-init))

;; helps with dart/flutter dev
(use-package dart-mode)

(use-package web-mode
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
  :config
  (setq lua-indent-level 2))

;; package to help making http requests
(use-package request)

(use-package counsel
  :config (ivy-mode t))

;; highlights color names with the corresponding color
(use-package rainbow-mode
  :config
  (add-hook 'prog-mode-hook 'rainbow-mode))

;; helps figure out which key runs which function
(use-package command-log-mode
  :config
  (global-command-log-mode))

;; the silver searcher, an alternative to grep
(use-package ag
  :config
  (global-set-key (kbd "C-c g") 'counsel-ag))

;; save undos/redos even when buffer is killed or emacs restarts
(use-package undo-fu-session
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (global-undo-fu-session-mode))

(use-package company-auctex
  :config
  (company-auctex-init))

;; executing sage in org babel
(use-package ob-sagemath
  :config
  ;; Ob-sagemath supports only evaluating with a session.
  (setq org-babel-default-header-args:sage '((:session . t)
                                             (:results . "output")))
  ;; C-c c for asynchronous evaluating (only for SageMath code blocks).
  (with-eval-after-load "org"
    (define-key org-mode-map (kbd "C-c E") 'ob-sagemath-execute-async)))

;; better built-in help/documentation
(use-package helpful
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h a") #'helpful-symbol)
  (global-set-key (kbd "C-h k") #'helpful-key))

;; yasnippet
(use-package yasnippet-snippets)
(use-package yasnippet
  :config
  ;;(setq yas-snippet-dirs
  ;;      `(,(concat user-emacs-directory "snippets")))
  (yas-global-mode 1)
  (global-set-key (kbd "C-'") 'yas-expand))

;; highlight errors in code
(use-package flycheck
  :config
  (global-flycheck-mode))

;; edit multiple instances of a word simulataneously
(use-package iedit)

;; integration with powerthesaurus.org
(use-package powerthesaurus)

;; key guide
(use-package guide-key
  :config
  (setq guide-key/guide-key-sequence t)
  (guide-key-mode 1))

;; highlight surrounding parentheses
(use-package highlight-parentheses
  :config
  (add-hook 'prog-mode-hook #'highlight-parentheses-mode))

(use-package literate-calc-mode
  :ensure t)

;; generating linear ranges quickly
(use-package tiny
  :config
  (global-set-key (kbd "C-c t") 'tiny-expand))

;; start server
(ignore-errors (server-start))

;; Set transparency of emacs
(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))
;;(transparency 90)

;; function to make printing easier for many languages
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
    (if (or (string= major-mode "emacs-lisp-mode") (string= major-mode "lisp-interaction-mode"))
        (progn
          (back-to-indentation)
          (insert "(message ")
          (end-of-line)
          (insert ")"))))
(define-key evil-normal-state-map (kbd "SPC p") 'current-line-to-print-statement)

;; c-x c-l to complete line like vim
(defun my-expand-lines ()
  (interactive)
  (let ((hippie-expand-try-functions-list
         '(try-expand-line)))
    (call-interactively 'hippie-expand)))
(define-key evil-insert-state-map (kbd "C-x C-l") 'my-expand-lines)

;; org mode config
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(setq org-log-done 'time)
;; Show images when opening a file.
(setq org-startup-with-inline-images t)
;; Show images after evaluating code blocks.
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
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
   (latex . t)
   (lua . t)))
;; require org-tempo to enable <s expansion
(require 'org-tempo)
;; make org babel default to python3
(setq org-babel-python-command "python3")

(defun run-command-show-output (cmd)
  "run shell command and show continuous output in new buffer"
  (interactive)
  (progn
    (start-process-shell-command cmd cmd cmd)
    (display-buffer cmd)
    (end-of-buffer-other-window nil)))

;; hide config
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
(setq dired-listing-switches "-l")
(setq dired-dwim-target t) ;; moving files in a smart way when window is split into 2
(add-hook 'dired-mode-hook 'auto-revert-mode) ;; hook to make dired auto refresh files when they get edited/changed/created/whatever

;; function to get size of files in dired
(defun dired-get-size ()
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "du" nil t nil "-sch" files)
      (message "Size of all marked files: %s"
               (progn 
                 (re-search-backward "\\(^[0-9.,]+[A-Za-z]+\\).*total$")
                 (match-string 1))))))
 (define-key dired-mode-map (kbd "?") 'dired-get-size)

;; vim like keys for dired image viewer
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

;; my config for latex
;; this disables the error when trying to insert dollar after \(
(define-key TeX-mode-map "$" nil)
(defun current-filename ()
  (file-name-sans-extension
   (file-name-nondirectory (buffer-file-name))))

(defun get-latex-cache-dir-path ()
  "return the path for the directory that contains the compiled pdf latex documents"
  (interactive)
  (setq dir-path (concat (expand-file-name user-emacs-directory) "latex/"))
  (ignore-errors (make-directory dir-path))
  dir-path)

(defun compile-current-document ()
  "compile the current latex document being edited"
  (interactive)
  (call-process-shell-command (concat (concat (concat "pdflatex -output-directory=" (concat (get-latex-cache-dir-path) " ")) (buffer-file-name)) "&"))
  (message (concat "compiled " (buffer-file-name))))

(defun open-current-document ()
  "open the pdf of the current latex document that was generated"
  (interactive)
  (call-process-shell-command (concat (concat "open " (get-latex-cache-dir-path)) (concat (current-filename) ".pdf &"))))

(evil-define-key 'normal 'LaTeX-mode-map (kbd "SPC x") 'compile-sagetex-show-output)
(evil-define-key 'normal 'LaTeX-mode-map (kbd "SPC v") 'open-current-document)
(add-hook
 'LaTeX-mode-hook
 (lambda ()
   (compile-current-document)
   (add-hook 'after-save-hook 'compile-sagetex 0 t)))

(defun compile-sagetex-command ()
  "return the command needed to compile sagetex"
  (interactive)
  (setq first-pdflatex-command (concat "(" (concat (concat (concat "pdflatex -output-directory=" (concat (get-latex-cache-dir-path) " ")) (buffer-file-name)) ";")))
  (setq last-pdflatex-command (concat (concat (concat "pdflatex -output-directory=" (concat (get-latex-cache-dir-path) " ")) (buffer-file-name)) ")"))
  (concat first-pdflatex-command (concat (concat "(cd " (concat (get-latex-cache-dir-path) (concat "; sage " (concat (current-filename) ".sagetex.sage);")))) last-pdflatex-command)))

(defun compile-sagetex ()
  "compile the current latex document with support for sagetex"
  (interactive)
  (call-process-shell-command (concat (compile-sagetex-command) "&")))

(defun compile-sagetex-show-output ()
  "compile sagetex and show compilation output in an emacs window"
  (interactive)
  (run-command-show-output (concat (compile-sagetex-command) "&& echo compilation succeeded || echo failed")))

;; this is a function to change the text between two $'s since i do that alot in latex
(defun change-text-between-dollar-signs ()
  "change the text between 2 dollar signs surrounding the cursor"
  (interactive)
  (search-backward "$")
  (forward-char)
  (zap-up-to-char 1 ?$)
  (evil-insert nil))
(define-key evil-normal-state-map (kbd "SPC c") 'change-text-between-dollar-signs)

;; dmenu like functions
(defun search-open-file (directory-path regex)
  "search for file and open it similar to dmenu"
  (interactive)
  (let ((my-file (helm-comp-read "select file: " (directory-files-recursively directory-path regex))))
    (call-process-shell-command (concat "open '" (concat (expand-file-name my-file) "'")))))

(defun search-open-file-in-emacs (directory-path regex)
  (let ((my-file (helm-comp-read "select file: " (directory-files-recursively directory-path regex))))
    (find-file (expand-file-name my-file) "'")))

(define-key evil-normal-state-map (kbd "SPC f c")
  (lambda () (interactive) (search-open-file "~/Desktop/college" ".*\\(pdf\\|tex\\|doc\\|mp4\\|png\\)")))
(define-key evil-normal-state-map (kbd "SPC F c")
  (lambda () (interactive)
    (search-open-file-in-emacs "~/Desktop/college" ".*\\(pdf\\|tex\\|doc\\|org\\)")))
(define-key evil-normal-state-map (kbd "SPC f p")
  (lambda () (interactive) (search-open-file "~/Desktop/p" "")))
(define-key evil-normal-state-map (kbd "SPC f b")
  (lambda () (interactive) (search-open-file "~/Desktop/books" "")))
(define-key evil-normal-state-map (kbd "SPC f d")
  (lambda () (interactive) (search-open-file "~/Desktop" "")))
(define-key evil-normal-state-map (kbd "SPC F d")
  (lambda () (interactive)
    (search-open-file-in-emacs "~/Desktop" "")))

;; automatically run script being edited, demonstrates how we can auto compile files on save
(defun run-script ()
  "run the current bash script being edited"
  (interactive)
  (run-command-show-output (buffer-file-name)))

(add-hook 'sh-mode-hook
          (lambda ()
            (add-hook 'after-save-hook 'run-script 0 t)))
