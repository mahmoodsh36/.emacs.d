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
(when window-system (set-frame-size (selected-frame) 115 58))
;; enable which-function-mode that shows the current function being edited in the bottom bar
(add-hook 'prog-mode-hook 'which-function-mode)
;; key to start calc mode
(global-set-key (kbd "C-c c") 'calc)
;; no damn fringes dude!
(set-fringe-style 0)
;; set font
(set-frame-font "Fantasque Sans Mono 11" nil t)
;; display only buffer name in modeline
(setq-default mode-line-format (list " " mode-line-modified "%e %b"))

;; smooth scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don"t accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

;; needed for evil mode
(use-package undo-fu)

;; evil-mode
(setq evil-want-keybinding nil)
(use-package evil
  :config
  (evil-mode 1)
  (evil-set-initial-state 'image-dired-thumbnail-mode 'emacs)
  (define-key evil-insert-state-map (kbd "C-e") 'evil-scroll-line-down)
  (define-key evil-insert-state-map (kbd "C-y") 'evil-scroll-line-up)
  ;; undo/redo keys
  (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
  (define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo)
  ;; make ESC cancel all
  (define-key key-translation-map (kbd "ESC") (kbd "C-g")))

;; evil-surround for evil mode
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;; exchange words more easily with evil
(use-package evil-exchange
  :config
  (evil-exchange-install))

;; search for the current visual selection with */#
(use-package evil-visualstar
  :config
  (global-evil-visualstar-mode))

;; support to make evil more compatible with the whole of emacs
(use-package evil-collection
  :after (evil)
  :config
  (setq evil-collection-mode-list '(dired)) ;; enable for dired
  (evil-collection-init))

;; display marks visually
(use-package evil-visual-mark-mode
  :config
  (add-hook 'evil-mode-hook 'evil-visual-mark-mode))

;; display visual hints for evil actions
(use-package evil-goggles
  :ensure t
  :config
  (evil-goggles-mode))

;; increase/decrease numbers like in vim
(use-package evil-numbers
  :config
  (global-set-key (kbd "C-c +") 'evil-numbers/inc-at-pt)
  (global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt)
  (global-set-key (kbd "C-c C-+") 'evil-numbers/inc-at-pt-incremental)
  (global-set-key (kbd "C-c C--") 'evil-numbers/dec-at-pt-incremental))

;; multiple cursors for evil mode
(use-package evil-mc
  :config
  (global-evil-mc-mode 1))

;; make line a text object - yil dil cil, etc..
(use-package evil-textobj-line)

;; quick commenting
(use-package evil-leader) ;; this provides the leader key needed for nerd commenter
(use-package evil-nerd-commenter
  :config
  (global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines))

;; evil-mode bindings here, after the package is installed
;; keybinding to quickly open config file
(define-key evil-normal-state-map (kbd "SPC e") (lambda () (interactive) (find-file user-init-file)))

;; relative numbering
(use-package linum-relative
  :config
  (linum-relative-mode)
  ;; show current line number not '0'
  (setq linum-relative-current-symbol ""))

(use-package counsel
  :config
  (ivy-mode)
  (setq ivy-height 25)
  (define-key evil-normal-state-map (kbd "SPC g") 'counsel-ag)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file))

;; projectile
(use-package projectile
  :config
  (setq projectile-completion-system 'ivy)
  ;;(setq projectile-project-search-path '("~/Desktop/"))
  (projectile-mode +1)
  (define-key evil-normal-state-map (kbd "SPC p") 'projectile-command-map))

;; auto completion
(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (global-set-key (kbd "M-/") 'company-complete-common-or-cycle)
  (setq company-idle-delay 0)
  (setq company-require-match nil)
  (eval-after-load 'company
    '(progn
       (define-key company-active-map (kbd "TAB") 'company-complete-selection)
       (define-key company-active-map [tab] 'company-complete-selection)
       (unbind-key "RET" company-active-map)
       (unbind-key "<return>" company-active-map))))

;; popup documentation for quick help for company
(use-package company-quickhelp
  :config
  (company-quickhelp-mode))

;; company completion with icons
(use-package company-box
  :hook (company-mode . company-box-mode))

;; company auctex support for latex
(use-package company-auctex
  :config
  (company-auctex-init))

;; anaconda for python
(use-package company-anaconda
  :config
  (eval-after-load "company"
    '(add-to-list 'company-backends 'company-anaconda))
  (add-hook 'python-mode-hook 'anaconda-mode))

;; helm integration for company
(use-package helm-company
  :config
  (eval-after-load 'company
    '''(progn
         (define-key company-mode-map (kbd "C-:") 'helm-company)
         (define-key company-active-map (kbd "C-:") 'helm-company))))

;; company for web mode
(use-package company-web)

;; company for shell scripting
(use-package company-shell
  :config
  (add-to-list 'company-backends '(company-shell company-shell-env)))

;; colorful delimiters
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; evil mode support for org
(use-package evil-org)

;; themes
(use-package doom-themes
  :config
  (load-theme 'doom-molokai t))

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

;; highlights color names with the corresponding color
(use-package rainbow-mode
  :config
  (add-hook 'prog-mode-hook 'rainbow-mode))

;; helps figure out which key runs which function
(use-package command-log-mode
  :config
  (global-command-log-mode))

;; save undos/redos even when buffer is killed or emacs restarts
(use-package undo-fu-session
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (global-undo-fu-session-mode))

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
  (global-set-key (kbd "M-<tab>") 'yas-expand))

;; highlight errors in code
(use-package flycheck
  :config
  (global-flycheck-mode))

;; edit multiple instances of a word simulataneously
(use-package iedit)

;; integration with powerthesaurus.org
(use-package powerthesaurus)

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

;; icons for dired
(use-package all-the-icons)
(use-package all-the-icons-dired
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;; modern API for working with files/dirs
(use-package f)

;; language server protocol support
(use-package lsp-mode
  :config
  (add-hook 'prog-mode-hook 'lsp-mode))

;; show simple info on the right
(use-package lsp-ui
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;; lsp support for treemacs
(use-package lsp-treemacs
  :config
  (lsp-treemacs-sync-mode 1)
  (treemacs-resize-icons 15)
  (setq treemacs-width 30))

;; ivy integration for lsp
(use-package lsp-ivy)

;; for evil mode compatibility
(use-package treemacs-evil
  :config
  (define-key evil-normal-state-map (kbd "SPC t") 'treemacs))

;; highlight uncommited changes
(use-package diff-hl
  :config
  (add-hook 'prog-mode-hook 'diff-hl-mode))

;; ensure the PATH variable is set according to the users shell, solves some issues on macos
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; provides command to restart emacs
(use-package restart-emacs)

;; display available keybindings
(use-package which-key
  :config
  (which-key-mode 1))

;; small flash when evaluating a sexp
(use-package eval-sexp-fu)

;; start server
(server-start)

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
(define-key evil-normal-state-map (kbd "SPC o") 'current-line-to-print-statement)

;; c-x c-l to complete line like vim
(defun my-expand-lines ()
  (interactive)
  (let ((hippie-expand-try-functions-list
         '(try-expand-line)))
    (call-interactively 'hippie-expand)))
(define-key evil-insert-state-map (kbd "C-x C-l") 'my-expand-lines)

;; org mode config
;; keybinding to export to html
(evil-define-key 'normal 'org-mode-map (kbd "SPC x") 'org-html-export-to-html)
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
   (C . t)
   (shell . t)
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

(defun run-command-save-output (the-cmd command-name)
  "run a shell command and save its output in a buffer"
  (interactive)
  (progn
    (start-process-shell-command command-name command-name the-cmd)))

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
;; make vip/vap/dap/cip etc.. in latex work properly
(defun my-LaTeX-mode-hook()
  (setq paragraph-start "\f\\|[ 	]*$")
  (setq paragraph-separate "[ 	\f]*$"))
(add-hook 'LaTeX-mode-hook 'my-LaTeX-mode-hook)

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
  (call-process-shell-command (concat (concat "xdg-open " (get-latex-cache-dir-path)) (concat (current-filename) ".pdf &"))))

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
  (start-process-shell-command "latex" "latex" (compile-sagetex-command)))

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
  "search for a file recursively in a directory and open it - works on macos"
  "search for file and open it similar to dmenu"
  (interactive)
  (let ((my-file (ivy-completing-read "select file: " (directory-files-recursively directory-path regex))))
    (call-process-shell-command (concat (concat "xdg-open '" (concat (expand-file-name my-file) "'")) "&"))))

(defun search-open-file-in-emacs (directory-path regex)
  "search for a file recursively in a directory and open it in emacs"
  (let ((my-file (ivy-completing-read "select file: " (directory-files-recursively directory-path regex))))
    (find-file (expand-file-name my-file) "'")))

;; keys to search for files
(define-key evil-normal-state-map (kbd "SPC f c")
  (lambda () (interactive) (search-open-file "~/workspace/college" ".*\\(pdf\\|tex\\|doc\\|mp4\\|png\\)")))
(define-key evil-normal-state-map (kbd "SPC F c")
  (lambda () (interactive)
    (search-open-file-in-emacs "~/Desktop/college" ".*\\(pdf\\|tex\\|doc\\|org\\)")))
(define-key evil-normal-state-map (kbd "SPC f p")
  (lambda () (interactive) (search-open-file "~/data/p" "")))
(define-key evil-normal-state-map (kbd "SPC f b")
  (lambda () (interactive) (search-open-file "~/data/books" "")))
(define-key evil-normal-state-map (kbd "SPC f d")
  (lambda () (interactive) (search-open-file "~/data" "")))
(define-key evil-normal-state-map (kbd "SPC F d")
  (lambda () (interactive)
    (search-open-file-in-emacs "~/data" "")))

;; other keybindings
(global-set-key (kbd "C-M-S-x") 'eval-region)
(global-set-key (kbd "C-x D") 'image-dired)
(global-set-key (kbd "C-c f") 'find-function-at-point)
(define-key evil-normal-state-map (kbd "SPC r d") (lambda () (interactive) (dired "~/dl/")))
(define-key evil-normal-state-map (kbd "SPC r a") (lambda () (interactive) (dired "~/data/")))
(define-key evil-normal-state-map (kbd "SPC f f") 'counsel-find-file)
(define-key evil-normal-state-map (kbd "SPC SPC") 'counsel-M-x)
(define-key evil-normal-state-map (kbd "SPC b k") 'kill-this-buffer)
(define-key evil-normal-state-map (kbd "SPC b K") 'kill-buffer-and-window)
(define-key evil-normal-state-map (kbd "SPC b s") 'counsel-switch-buffer)

;; automatically run script being edited, demonstrates how we can auto compile files on save
(defun run-script ()
  "run the current bash script being edited"
  (interactive)
  (run-command-show-output (buffer-file-name)))
(add-hook 'sh-mode-hook
          (lambda ()
            (add-hook 'after-save-hook 'run-script 0 t)))