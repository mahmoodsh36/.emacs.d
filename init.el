;; (toggle-debug-on-error)
;; disable annoying warnings
(setq native-comp-async-report-warnings-errors nil)
;; disable customization using the interactive interface and remove startup screen
(setq custom-file "/dev/null")
;; disable stupid startup screen
(setq inhibit-startup-screen t)

;; add ~/.emacs.d to load-path and load some files
(push (concat user-emacs-directory "/lisp") load-path)
(require 'setup-utils)
(require 'setup-constants)
(require 'setup-android)
(require 'setup-elpaca)
(require 'setup-other)

;; set tab size to 2 spaces except 4 for python
(setq-default ;tab-width 2
              js-indent-level 2
              c-basic-offset 2
              indent-tabs-mode nil
              python-indent-offset 4)
(setq evil-shift-width 2)
;; overwrite highlighted text
(delete-selection-mode 1)
;; show matching parenthases
(show-paren-mode 1)
;; disable upper bars and scrollbar
(when (not (is-android-system))
  ;; (menu-bar-mode -1) ;; enable it so that emacs acts like a normal app on macos
  (when window-system (set-frame-size (selected-frame) 100 50))
  (toggle-scroll-bar -1)
  (tool-bar-mode -1)
  ;; margin around the windows
  ;; (set-fringe-style '(12 . 0))
  (set-fringe-style '(0 . 0))
  )
;; always follow symlinks
(setq vc-follow-symlinks t)
;; y-or-n instead of yes-or-no
(defalias 'yes-or-no-p 'y-or-n-p)
;; all backups to one folder
(setq backup-directory-alist `((".*" . ,"~/.emacs.d/backup/"))
      auto-save-file-name-transforms `((".*" ,"~/.emacs.d/backup/" t)))
;; disable cursor blink
(blink-cursor-mode 0)
;; treat underscore as part of word
(defun underscore-part-of-word-hook ()
  (modify-syntax-entry ?_ "w"))
;;(add-hook 'text-mode-hook 'underscore-part-of-word-hook)
;; highlight current line, slows down buffer redisplay, causes cpu spikes like linum-relative
(global-hl-line-mode)
;; reload file automatically
(global-auto-revert-mode t)
;; enable all disabled commands
(setq disabled-command-function nil)
;; initial frame size
;; (when window-system (set-frame-size (selected-frame) 120 48))
;; display only buffer name in modeline
;; the following line enables L<line number> at the end
(when (not (is-android-system))
  (setq-default mode-line-format (list " " mode-line-modified "%e %b" mode-line-position-line-format " " '(:eval (persp-current-name)) " " '(:eval (current-buffer-title-if-org)))))
(defun current-buffer-title-if-org ()
  (if (derived-mode-p 'org-mode)
      (org-get-title)
    ""))
;; (setq-default mode-line-format (list " " mode-line-modified "%e %b"))
;; restore default status line for pdf mode
(let ((hooks '(pdf-view-mode-hook doc-view-mode-hook)))
  (dolist (hook hooks)
    (add-hook
     hook
     (lambda ()
       (interactive)
       (setq-local mode-line-format
                   (eval (car (get 'mode-line-format 'standard-value))))))))
;; kill buffer without confirmation when its tied to a process
(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))
;; make tab complete current word
(setq dabbrev-case-replace nil)
;; (global-set-key "\t" 'dabbrev-completion)
;; save open buffers on exit
;; (desktop-save-mode 1)
;; save minibuffer history
(setq savehist-file (expand-file-name (from-brain "emacs_savehist")))
(savehist-mode 1)
(add-to-list 'savehist-additional-variables 'search-ring)
(add-to-list 'savehist-additional-variables 'regexp-search-ring)
(add-to-list 'savehist-additional-variables 'kill-ring)
(add-to-list 'savehist-additional-variables 'command-history)
(add-to-list 'savehist-additional-variables 'compile-command)
(add-to-list 'savehist-additional-variables 'compile-history)
(add-to-list 'savehist-additional-variables 'blk-hist)
(add-to-list 'savehist-additional-variables 'files-to-export)
(setq savehist-save-minibuffer-history t)
(setq history-length t) ;; no limit to history length
;; break long lines into multiple, dont cut-off words
(global-visual-line-mode)
;; stop the annoying warnings from org mode cache
(setq warning-minimum-level :emergency)
;; use imagemagick for formats like webp
(setq image-use-external-converter t)
;; display white spaces and newlines
(setq whitespace-style '(face tabs spaces trailing space-before-tab newline indentation empty space-after-tab space-mark tab-mark newline-mark missing-newline-at-eof)) ;; use this to highlight everything including space
;; (setq whitespace-style '(face tabs spaces indentation trailing space-before-tab newline empty space-after-tab tab-mark newline-mark missing-newline-at-eof)) ;; use this to not highlight spaces, works with org better and some themes.. (a fallback)
;; show zero-width characters
(set-face-background 'glyphless-char "red")
;; change newline character
;;(setf (elt (car (cdr (cdr (assoc 'newline-mark whitespace-display-mappings)))) 0) ?⤸)
(global-whitespace-mode) ;; for functions to act on visual lines rather than actual lines
;; relative line numbers
;; (add-hook 'prog-mode-hook
;;           (lambda ()
;;             (setq display-line-numbers 'relative)))
;; disable annoying local variable prompt
(setq enable-local-variables :all)
;; stop always inserting a newline at the end of a file
(setq require-final-newline nil) ;; not sure if this is needed
(setq mode-require-final-newline nil)
;; dont deactivate the mark on non-shift commands
(setq shift-select-mode 'permanent)
(setq bookmark-file (from-brain "emacs_bookmarks"))
;; (global-highlight-changes-mode)
;; ;; remove highlights after save
;; (add-hook 'after-save-hook
;;           '(lambda ()
;;              (if (boundp 'highlight-changes-mode)
;;                  (highlight-changes-remove-highlight (point-min) (point-max)))))
;; return propertized strings from completing-read (when theyre passed) [[denote:20240321T195503][alternative completing read]]
(setq minibuffer-allow-text-properties t)
;; save cursor position of files when you exit them
(save-place-mode 1)
(setq save-place-file (from-brain "emacs_save_place"))
;; disable lock files
(setq create-lockfiles nil)
;; disable stupid beep sounds on macos
(setq ring-bell-function #'ignore)
;; increase max number of messages
(setq message-log-max 100000)
;; dont ask to confirm when opening large files
(setq large-file-warning-threshold nil)
;; disable multiplication precedence over division in calc
(setq calc-multiplication-has-precedence nil)
;;(which-key-mode)

;; for M-x term
;; (setq explicit-shell-file-name "zsh")
;; (setq shell-file-name "zsh")
;; (setq explicit-zsh-args '("--login" "--interactive"))
;; (setq explicit-zsh-args '("--interactive"))
;; (defun zsh-shell-mode-setup ()
;;   (setq-local comint-process-echoes t))
;; (add-hook 'shell-mode-hook #'zsh-shell-mode-setup)

(setq shell-file-name *shell-program*)

;; "smooth" scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don"t accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

;; start server
(server-start)

;; eshell configs
;; make the cursor stay at the prompt when scrolling
(setq eshell-scroll-to-bottom-on-input t)
;; file to store aliases automatically to
(setq eshell-aliases-file (from-brain "eshell_aliases"))
(defun eshell-cd-and-ls (&rest args) ; all but first ignored
  "cd into directory and list its contents"
  (interactive "P")
  (let ((path (car args)))
    (cd path)
    (eshell/ls)))
;; eshell history file location
(setq eshell-history-file-name (from-brain "eshell_history")) ;; save history to filesystem
(setq eshell-history-size 100000000)

;; enable recentf for recently opened file history, https://www.emacswiki.org/emacs/RecentFiles#toc21
(recentf-mode 1)
(setq recentf-max-menu-items 10000000)
(setq recentf-max-saved-items 10000000)

;; prettify symbols..
(global-prettify-symbols-mode +1)
;; replace lambda text with symbol
(defconst lisp-prettify-symbols-alist
  '(("lambda"  . ?λ)
    ;; ("let" . ?≜)
    ;; ("nil" . (?· (Br . Bl) ?· (Br . Bl) ?∅))
    ;; ("sqrt" . ?√)
    ;; ("sum" . (?· (Br . Bl) ?· (Br . Bl) ?∑))
    ;; ("equal" . (?· (Br . Bl) ?· (Br . Bl) ?· (Br . Bl) ?· (Br . Bl) ?≡))
    ;; ("defun" . ?⪮)
    ;; ("<=" . ?≤)
    ;; ("<=" . (?· (Br . Bl) ?≤))
    ;; (">=" . (?· (Br . Bl) ?≥))
    ;; ("->" . ?→)
    ;; ("->" . (?· (Br . Bl) ?→))
    ;; ("->>" . (?· (Br . Bl) ?· (Br . Bl) ?↠))
    ;; ("=>" . ?⇒)
    ;; ("map" . (?· (Br . Bl) ?· (Br . Bl) ?↦))
    ;; ("/=" . ?≠)
    ;; ("!=" . ?≠)
    ;; ("==" . ?≡)
    ;; ("<<" . ?≪)
    ;; (">>" . ?≫)
    ;; ("<=<" . ?↢)
    ;; (">=>" . ?↣)
    ;; ("&&" . ?∧)
    ;; ("and" . (?· (Br . Bl) ?· (Br . Bl) ?∧))
    ;; ("or" . (?· (Br . Bl) ?∨))
    ;; ("progn" . ?∘)
    ;; ("not" . (?· (Br . Bl) ?· (Br . Bl) ?¬))
    ))
;; convert back to text when cursor is over the symbol
(setq prettify-symbols-unprettify-at-point 'right-edge)

;; persistent comint history
;; https://oleksandrmanzyuk.wordpress.com/2011/10/23/a-persistent-command-history-in-emacs/
(defun comint-write-history-on-exit (process event)
  (comint-write-input-ring)
  (let ((buf (process-buffer process)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (insert (format "\nProcess %s %s" process event))))))
(defun turn-on-comint-history ()
  (let ((process (get-buffer-process (current-buffer))))
    (when process
      (setq comint-input-ring-file-name
            (format "%s/inferior-%s-history"
                    *brain-dir* (process-name process)))
      (comint-read-input-ring)
      (set-process-sentinel process
                            #'comint-write-history-on-exit))))
(defun mapc-buffers (fn)
  (mapc (lambda (buffer)
          (with-current-buffer buffer
            (funcall fn)))
        (buffer-list)))
(defun comint-write-input-ring-all-buffers ()
  (mapc-buffers 'comint-write-input-ring))
(with-eval-after-load 'comint
  (add-hook 'kill-emacs-hook 'comint-write-input-ring-all-buffers)
  (add-hook 'inferior-haskell-mode-hook 'turn-on-comint-history)
  (add-hook 'inferior-python-mode-hook 'turn-on-comint-history)
  (add-hook 'shell-mode-hook 'turn-on-comint-history)
  (add-hook 'kill-buffer-hook 'comint-write-input-ring))

;; load other elisp files
(require 'setup-org)
(require 'setup-packages)
(require 'setup-evil)
(require 'setup-theme)
(require 'setup-dired)
(require 'setup-eglot)
(require 'setup-blk)

;; open agenda on startup
(add-hook 'elpaca-after-init-hook
          (lambda ()
            (require 'setup-keys) ;; load setup-org.el
            ;; (with-eval-after-load-all
            ;; '(evil general)
            (when (file-exists-p persp-state-default-file)
              (persp-state-load persp-state-default-file)
              (persp-switch "main"))
            ;; (switch-to-theme 'minimal-light)
            ;; (switch-to-theme 'darktooth-darker)
            ;; (switch-to-theme 'ample-flat)
            ;; (switch-to-theme 'ample-light)
            ;; (switch-to-theme 'ample)
            ;; (switch-to-theme 'acme)
            ;; (switch-to-theme 'doom-gruvbox-light)
            ;; (switch-to-theme 'stimmung-themes-light)
            ;; (switch-to-theme 'stimmung-themes-dark)
            ;; (switch-to-theme 'ef-tritanopia-dark)
            ;; (switch-to-theme 'ef-melissa-dark)

            ;; (switch-to-light-theme)
            ;; (switch-to-dark-theme)
            (when my-current-theme
              (switch-to-theme my-current-theme))
            ;; (switch-to-theme 'ef-autumn)
            ;; (switch-to-theme 'poet-dark)
            ;; (switch-to-theme 'modus-operandi-tinted)
            ;; (switch-to-theme 'ef-melissa-light)
            ;; (switch-to-theme 'gruvbox-dark-hard)

            ;; (switch-to-theme 'gruvbox-light-soft)
            ;; (switch-to-theme 'modus-operandi)
            ;; (switch-to-theme 'modus-vivendi)
            ;; (switch-to-tango-theme)
            ;; (set-face-background hl-line-face "PeachPuff3")
            ;; (switch-to-theme 'doom-sourcerer)
            ;;(switch-to-darktooth-theme)
            ))

;; disable repeat-mode
;; (repeat-mode -1)

;; enable flyspell (spell checking)
;; (dolist (hook '(text-mode-hook))
;;   (add-hook hook (lambda () (flyspell-mode 1))))
;; ;; flyspell buffer when its opened
;; (add-hook 'flyspell-mode-hook #'flyspell-buffer)

;; disable some modes for large files (otherwise emacs will hang..)
;; there's also find-file-literally i guess
(defun conditional-disable-modes ()
  (unless (any #'derived-mode-p '(org-mode pdf-view-mode image-mode doc-view-mode archive-mode arc-mode jka-compr-mode))
    (when (> (buffer-size) (* 1024 512))
      (message "entering fundamental-mode from %s" major-mode)
      (when (fboundp 'flycheck-mode) (flycheck-mode -1))
      (flyspell-mode -1)
      (font-lock-mode -1)
      (fundamental-mode)
      (which-function-mode -1)
      (when (fboundp 'linum-mode) (linum-mode 0))
      (lsp-mode 0)
      )))
(add-hook 'find-file-hook #'conditional-disable-modes)

;; transparency
;; (set-frame-parameter nil 'alpha-background 90)
;; (add-to-list 'default-frame-alist '(alpha-background . 90))

;; http://xahlee.info/emacs/emacs/emacs_file_encoding.html
;; utf-8 as default encoding
(set-language-environment 'utf-8)
(set-default-coding-systems 'prefer-utf-8) ;; this makes it work when exiting with ivy-prescient persist enabled
(set-keyboard-coding-system 'utf-8-unix)

;; tell erc to save logs
(require 'erc)
(require 'erc-log)
(setq erc-log-channels-directory (from-brain "erc"))
(setq erc-save-buffer-on-part t)

;; this causes errors so disable it
(setq text-mode-ispell-word-completion nil)

;; limit the scope of bidirectional display to single line (make each line its own paragraph) https://emacs.stackexchange.com/questions/38464/limit-the-scope-of-bidirectional-display-to-single-line
(setq bidi-paragraph-separate-re "^"
      bidi-paragraph-start-re "^")
;; this is needed to make it work with org
(setq-default bidi-paragraph-separate-re "^"
              bidi-paragraph-start-re "^")

;; make some buffers open in the current window (works for some commands like `list-buffers')
(setq display-buffer-alist '((t display-buffer-same-window)))

(setq comint-scroll-to-bottom-on-output t)