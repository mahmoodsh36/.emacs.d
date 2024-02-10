;; (toggle-debug-on-error)
;; disable annoying warnings
(setq native-comp-async-report-warnings-errors nil)
;; disable customization using the interactive interface and remove startup screen
(setq custom-file "/dev/null")
;; disable stupid startup screen
(setq inhibit-startup-screen t)

;; setup use-package, it provides stable packages unlike straight.el so i use it as the default package manager
;; (require 'package)
;; (add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;; (package-initialize)
;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))
;; (unless package-archive-contents
;;   (package-refresh-contents))
;; (eval-and-compile
;;   (setq use-package-always-ensure t
;;         use-package-expand-minimally t))
;; (require 'use-package)

;; setup quelpa
;; (setq quelpa-update-melpa-p nil)
;; (unless (package-installed-p 'quelpa)
;;   (with-temp-buffer
;;     (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
;;     (eval-buffer)
;;     (quelpa-self-upgrade)))
;; (quelpa
;;  '(quelpa-use-package
;;    :fetcher git
;;    :url "https://github.com/quelpa/quelpa-use-package.git"))
;; (require 'quelpa-use-package)
;; ;; make sure it works with use-package-always-ensure set to t
;; (setq use-package-ensure-function 'quelpa)
;; (quelpa-use-package-activate-advice)

(setq package-enable-at-startup nil) ;; disable package.el, needed for straight.el or elpaca to work properly with use-package

;; setup straight
;; (defvar bootstrap-version)
;; (let ((bootstrap-file
;;        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;;       (bootstrap-version 6))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;;          "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
;;          'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))

;; setup elpaca package manager
(defvar elpaca-installer-version 0.6)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))
;; install use-package support
(elpaca elpaca-use-package
        ;; enable use-package :ensure support for elpaca.
        (elpaca-use-package-mode)
        (setq use-package-always-ensure t))
;; block until current queue processed.
(elpaca-wait)

(use-package org
  :defer
  :elpaca `(org
              :fork (:host nil
                           :repo "https://git.tecosaur.net/tec/org-mode.git"
                           :branch "dev"
                           :remote "tecosaur")
              :files (:defaults "etc")
              :build t
              :pre-build
              (with-temp-file "org-version.el"
                (require 'lisp-mnt)
                (let ((version
                       (with-temp-buffer
                         (insert-file-contents "lisp/org.el")
                         (lm-header "version")))
                      (git-version
                       (string-trim
                        (with-temp-buffer
                          (call-process "git" nil t nil "rev-parse" "--short" "HEAD")
                          (buffer-string)))))
                  (insert
                   (format "(defun org-release () \"The release version of Org.\" %S)\n" version)
                   (format "(defun org-git-version () \"The truncate git commit hash of Org mode.\" %S)\n" git-version)
                   "(provide 'org-version)\n")))
              :pin nil))

;; path where all my notes etc go
(setq *brain-dir* (file-truename "~/brain/"))
(defconst *music-dir* (file-truename "~/music/"))
(defun brain-file (filename)
  "return `filename', prefixed by the path to the brain dir"
  (concat *brain-dir* filename))

;; set tab size to 2 spaces except 4 for python
(setq-default ;; tab-width 2
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
;; (menu-bar-mode -1) ;; enable it so that emacs acts like a normal app on macos
(toggle-scroll-bar -1)
(tool-bar-mode -1)
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
;; highlight current line
(global-hl-line-mode)
;; reload file automatically
(global-auto-revert-mode t)
;; enable all disabled commands
(setq disabled-command-function nil)
;; initial frame size
;; (when window-system (set-frame-size (selected-frame) 120 48))
(when window-system (set-frame-size (selected-frame) 100 50))
;; margin around the windows
;; (set-fringe-style '(12 . 0))
(set-fringe-style '(0 . 0))
;; display only buffer name in modeline
;; the following line enables L<line number> at the end
;; (setq-default mode-line-format (list " " mode-line-modified "%e %b" mode-line-position-line-format " " '(:eval (persp-current-name))))
;; (setq-default mode-line-format (list " " mode-line-modified "%e %b"))
;; restore default status line for pdf mode
;; (add-hook 'pdf-view-mode-hook
;;           (lambda ()
;;             (interactive)
;;             (setq-local mode-line-format (eval (car (get 'mode-line-format 'standard-value))))))
;; kill buffer without confirmation when its tied to a process
(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))
;; make tab complete current word
(setq dabbrev-case-replace nil)
;; (global-set-key "\t" 'dabbrev-completion)
;; save open buffers on exit
;; (desktop-save-mode 1)
;; save minibuffer history
(setq savehist-file (expand-file-name (concat *brain-dir* "/emacs_savehist")))
(savehist-mode 1)
(add-to-list 'savehist-additional-variables 'search-ring)
(add-to-list 'savehist-additional-variables 'regexp-search-ring)
(add-to-list 'savehist-additional-variables 'kill-ring)
(setq history-length t) ;; no limit to history length
;; break long lines into multiple
;;(global-visual-line-mode)
;; stop the annoying warnings from org mode cache
(setq warning-minimum-level :emergency)
;; use imagemagick for formats like webp
(setq image-use-external-converter t)
;; display white spaces and newlines
(setq whitespace-style '(face tabs spaces trailing space-before-tab newline indentation empty space-after-tab space-mark tab-mark newline-mark missing-newline-at-eof))
;; show zero-width characters
(set-face-background 'glyphless-char "red")
;; change newline character
;;(setf (elt (car (cdr (cdr (assoc 'newline-mark whitespace-display-mappings)))) 0) ?⤸)
;; (global-whitespace-mode)
;; relative line numbers
;; (add-hook 'prog-mode-hook
;;           (lambda ()
;;             (setq display-line-numbers 'relative)))
;; disable annoying local variable prompt
(setq enable-local-variables :all)
;; stop always inserting a newline at the end of a file
(setq require-final-newline nil) ;; not sure if this is needed
(setq mode-require-final-newline nil)

;; smooth scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don"t accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

(defun kill-all-buffers ()
  "kill all buffers excluding internal buffers (buffers starting with a space)"
  (interactive)
  (setq list (buffer-list))
  (while list
    (let* ((buffer (car list))
           (name (buffer-name buffer)))
      (and name
           (not (string-equal name ""))
           (/= (aref name 0) ?\s)
           (kill-buffer buffer)))
    (setq list (cdr list))))

;;(defconst *leader-key* "C-z");;"SPC")
(defconst *leader-key* "<SPC>")
;; (global-set-key (kbd "<SPC>") (make-sparse-keymap))
;;(defconst *leader-key* "<escape>")
(defun led (key-str)
  "return the keybinding `key-str', which is taken as a string, prefixed by the leader key defined in '*leader-key*'"
  (concat *leader-key* " " key-str))
;; (defun mykbd ()
;;   "to be written"
;;     )
;; backup/alternative for leader key in cases where it doesnt work like evil-state=emacs
(defun led-kbd (binding function &rest args)
  "define a keybinding prefixed by `*leader-key*'"
  (interactive)
  (let ((mykeymaps (plist-get args :keymaps)))
    (if mykeymaps
        (progn
          (general-define-key :states '(normal visual motion operator)
                              :keymaps mykeymaps
                              (led binding) function)
          (general-define-key :states '(normal visual motion operator emacs)
                              :keymaps mykeymaps
                              (concat "C-' " binding) function))
      (progn (general-define-key :states '(normal visual motion operator)
                                 :keymaps 'override
                                 (led binding) function)
             (general-define-key :states '(insert normal motion visual emacs)
                                 :keymaps 'override
                                 (concat "C-' " binding) function)))))
(defun my-kbd (binding function &rest args)
  )

;; start server
(server-start)

(defun run-command-show-output (cmd)
  "run shell command and show continuous output in new buffer"
  (interactive)
  (progn
    (start-process-shell-command cmd cmd cmd)
    (display-buffer cmd)
    (end-of-buffer-other-window nil)))

(defun current-filename ()
  "current filename without extension"
  (file-name-sans-extension
   (file-name-nondirectory (buffer-file-name))))

(defun get-latex-cache-dir-path ()
  "return the path for the directory that contains the compiled pdf latex documents"
  (interactive)
  (concat *brain-dir* "/out/"))

(defun compile-latex-file (path)
  (start-process-shell-command "latex" "latex" (format "lualatex -shell-escape -output-directory=%s %s" (file-truename (get-latex-cache-dir-path)) path)))

(defun compile-current-document ()
  "compile the current latex document being edited"
  (interactive)
  (compile-latex-file (buffer-file-name)))

(defun open-current-document ()
  "open the pdf of the current latex document that was generated"
  (interactive)
  (find-file-other-window (concat (get-latex-cache-dir-path) (current-filename) ".pdf")))
(defun open-current-document-this-window ()
  (interactive)
  (let ((pdf-file (concat (get-latex-cache-dir-path) (current-filename) ".pdf")))
    (if (file-exists-p pdf-file)
        (find-file pdf-file)
      (message "pdf file hasnt been generated"))))

;; tex hook to auto compile on save
;; (add-hook
;;  'TeX-mode-hook
;;  (lambda ()
;;    (compile-current-document)
;;    (add-hook 'after-save-hook 'compile-current-document 0 t)))

;; dmenu like functions
(defun search-open-file (directory-path regex)
  "search for file and open it similar to dmenu"
  (interactive)
  (let ((my-file (completing-read "select file: " (directory-files-recursively directory-path regex t))))
    (browse-url (expand-file-name my-file))))

(defun search-open-file-in-emacs (directory-path regex)
  "search for a file recursively in a directory and open it in emacs"
  (let ((my-file (completing-read "select file: " (directory-files-recursively directory-path regex))))
    (find-file (expand-file-name my-file) "'")))

;; automatically run script being edited, demonstrates how we can auto compile files on save
;; (defun run-script ()
;;   "run the current bash script being edited"
;;   (interactive)
;;   (run-command-show-output (buffer-file-name)))
;; (add-hook 'sh-mode-hook
;;           (lambda ()
;;             (add-hook 'after-save-hook 'run-script 0 t)))

;; eshell configs
;; make the cursor stay at the prompt when scrolling
(setq eshell-scroll-to-bottom-on-input t)
;; file to store aliases automatically to
(setq eshell-aliases-file (concat *brain-dir* "/eshell_aliases"))
(defun eshell-cd-and-ls (&rest args)           ; all but first ignored
  "cd into directory and list its contents"
  (interactive "P")
  (let ((path (car args)))
    (cd path)
    (eshell/ls)))
;; eshell history file location
(setq eshell-history-file-name (concat *brain-dir* "/eshell_history")) ;; save history to filesystem
(setq eshell-history-size 100000000)

;; disable multiplication precedence over division in calc
(setq calc-multiplication-has-precedence nil)

(defun generate-random-string (NUM)
  "generate a random alphanumerics string of length NUM."
  (interactive "P")
  (let* ((random-str "")
         (charset "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
         (baseCount (length charset)))
    (dotimes (_ (if (numberp NUM) (abs NUM) NUM))
      (setq random-str (concat random-str
                               (char-to-string (elt charset (random baseCount))))))
    random-str))
(defun insert-random-string (&optional num)
  (interactive)
  (or num (setq num 7))
  (insert (generate-random-string num)))
(defun temp-file (EXT)
  (format "%stmp_%s.%s" (concat *brain-dir* "out/") (generate-random-string 7) EXT))
(global-set-key (kbd "C-c R") #'insert-random-string)

(defun kill-all-dired-buffers ()
  "Kill all dired buffers."
  (interactive)
  (save-excursion
    (let ((count 0))
      (dolist (buffer (buffer-list))
        (set-buffer buffer)
        (when (equal major-mode 'dired-mode)
          (setq count (1+ count))
          (kill-buffer buffer)))
      (message "Killed %i dired buffer(s)." count))))

;; workaround for pdf-tools not reopening to last-viewed page of the pdf:
;; https://github.com/politza/pdf-tools/issues/18#issuecomment-269515117
(defun brds/pdf-set-last-viewed-bookmark ()
  (interactive)
  (when (eq major-mode 'pdf-view-mode)
    (bookmark-set (brds/pdf-generate-bookmark-name))))
(defun brds/pdf-jump-last-viewed-bookmark ()
  (bookmark-set "fake") ; this is new
  (when
      (brds/pdf-has-last-viewed-bookmark)
    (bookmark-jump (brds/pdf-generate-bookmark-name))))
(defun brds/pdf-has-last-viewed-bookmark ()
  (assoc
   (brds/pdf-generate-bookmark-name) bookmark-alist))
(defun brds/pdf-generate-bookmark-name ()
  (concat "PDF-LAST-VIEWED: " (buffer-file-name)))
(defun brds/pdf-set-all-last-viewed-bookmarks ()
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (brds/pdf-set-last-viewed-bookmark))))
(add-hook 'kill-buffer-hook 'brds/pdf-set-last-viewed-bookmark)
(add-hook 'pdf-view-mode-hook 'brds/pdf-jump-last-viewed-bookmark)
(unless noninteractive  ; as `save-place-mode' does
  (add-hook 'kill-emacs-hook #'brds/pdf-set-all-last-viewed-bookmarks))
(setq bookmark-file (concat *brain-dir* "emacs_bookmarks"))

(defun yas-delete-if-empty ()
  "function to remove _{} or ^{} fields, used by some of my latex yasnippets"
  (interactive)
  (point-to-register 'my-stored-pos)
  (save-excursion
    (while (re-search-backward "\\(_{}\\)" (line-beginning-position) t)
      (progn
        (replace-match "" t t nil 1)
        (jump-to-register 'my-stored-pos)))
    (while (re-search-backward "\\(\\^{}\\)" (line-beginning-position) t)
      (progn
        (replace-match "" t t nil 1)
        (jump-to-register 'my-stored-pos)))))

;; move over text object
;; (evil-define-motion evil-forward-text-object
;;   (count &optional text-object)
;;   "move to the end of following input text-object define
;; in evil-inner-text-objects-map ."
;;   (unless text-object
;;       (setf text-object
;;             (let ((key (read-key-sequence "text-object:")))
;;               (lookup-key evil-inner-text-objects-map key))))
;;   (let* ((region (funcall text-object count))
;;          (end (nth 1 region)))
;;     (goto-char end)))
;; (define-key evil-motion-state-map (kbd "M-w")
;;   #'evil-forward-text-object)

;; (evil-define-motion evil-backward-text-object
;;   (count &optional text-object)
;;   "move to the begin of following input text-object define
;; in evil-inner-text-objects-map ."
;;   (unless text-object
;;       (setf text-object
;;             (let ((key (read-key-sequence "text-object:")))
;;               (lookup-key evil-inner-text-objects-map key))))
;;   (let* ((region (funcall text-object count))
;;          (start (nth 0 region)))
;;     (goto-char start)))
;; (define-key evil-motion-state-map (kbd "M-b")
;;   #'evil-backward-text-object)

;; increase max number of messages
(setq message-log-max 100000)
;; dont ask to confirm when opening large files
(setq large-file-warning-threshold nil)

(defun sudo-find-file (file-name)
  "like find file, but opens the file as root using tramp"
  (interactive (list (read-file-name "file: " "/sudo::/")))
  (let ((tramp-file-name (expand-file-name file-name)))
    (find-file tramp-file-name)))

(defun open-in-vscode ()
  "open current file in vscode"
  (interactive)
  (shell-command (format "code %s" (buffer-file-name))))

(defun kill-this-buffer-volatile ()
  "kill current buffer, even if it has been modified."
  (interactive)
  (set-buffer-modified-p nil)
  (kill-this-buffer))

(defun copy-file-path (&optional DirPathOnlyQ)
  "Copy current buffer file path or dired path.
Result is full path.
If `universal-argument' is called first, copy only the dir path.

If in dired, copy the current or marked files.

If a buffer is not file and not dired, copy value of `default-directory'.

URL `http://xahlee.info/emacs/emacs/emacs_copy_file_path.html'
Version 2018-06-18 2021-09-30"
  (interactive "P")
  (let (($fpath
         (if (string-equal major-mode 'dired-mode)
             (progn
               (let (($result (mapconcat 'identity (dired-get-marked-files) "\n")))
                 (if (equal (length $result) 0)
                     (progn default-directory )
                   (progn $result))))
           (if (buffer-file-name)
               (buffer-file-name)
             (expand-file-name default-directory)))))
    (kill-new
     (if DirPathOnlyQ
         (progn
           (message "Directory copied: %s" (file-name-directory $fpath))
           (file-name-directory $fpath))
       (progn
         (message "File path copied: %s" $fpath)
         $fpath)))))

(defun cached-file (filename)
  "return 'filename' prefixed with cache dir path"
  (concat *brain-dir* "out/" filename))

;; disable stupid beep sounds on macos
(setq ring-bell-function #'ignore)

;; from https://www.emacswiki.org/emacs/FindingNonAsciiCharacters
(defun find-first-non-ascii-char ()
  "Find the first non-ascii character from point onwards."
  (interactive)
  (let (point)
    (save-excursion
      (setq point
            (catch 'non-ascii
              (while (not (eobp))
                (or (eq (char-charset (following-char))
                        'ascii)
                    (throw 'non-ascii (point)))
                (forward-char 1)))))
    (if point
        (goto-char point)
      (message "No non-ascii characters."))))
(defun find-next-unsafe-char (&optional coding-system)
  "Find the next character in the buffer that cannot be encoded by
coding-system. If coding-system is unspecified, default to the coding
system that would be used to save this buffer. With prefix argument,
prompt the user for a coding system."
  (interactive "Zcoding-system: ")
  (if (stringp coding-system) (setq coding-system (intern coding-system)))
  (if coding-system nil
    (setq coding-system
          (or save-buffer-coding-system buffer-file-coding-system)))
  (let ((found nil) (char nil) (csets nil) (safe nil))
    (setq safe (coding-system-get coding-system 'safe-chars))
    ;; some systems merely specify the charsets as ones they can encode:
    (setq csets (coding-system-get coding-system 'safe-charsets))
    (save-excursion
      ;;(message "zoom to <")
      (let ((end  (point-max))
            (here (point    ))
            (char  nil))
        (while (and (< here end) (not found))
          (setq char (char-after here))
          (if (or (eq safe t)
                  (< char ?\177)
                  (and safe  (aref safe char))
                  (and csets (memq (char-charset char) csets)))
              nil ;; safe char, noop
            (setq found (cons here char)))
          (setq here (1+ here))) ))
    (and found (goto-char (1+ (car found))))
    found))

(defun open-kitty-here ()
  (interactive)
  (async-shell-command "kitty ."))

(defun treemacs-remove-project-at-point-force ()
  (interactive)
  "force removal of project at point, even if its the last one"
  (treemacs-do-remove-project-from-workspace (treemacs-project-at-point) t))

(unbind-key "C-z")
(bind-keys :prefix-map mymap
           :prefix "C-z"
           ;;:prefix-docstring "Personal key bindings"
           ("v" . emacs-version)
           ("e" . (lambda () (interactive) (find-file user-init-file)))
           ("a" . kill-all-buffers)
           ("z" . zap-up-to-char)
           ("g" . deadgrep)
           )

;; https://karthinks.com/software/a-consistent-structural-editing-interface/
(repeat-mode 1)
(defvar structural-edit-map
  (let ((map (make-sparse-keymap)))
    (pcase-dolist (`(,k . ,f)
                   '(("u" . backward-up-list)
                     ("f" . forward-sexp)
                     ("b" . backward-sexp)
                     ("d" . down-list)
                     ("k" . kill-sexp)
                     ("n" . forward-list)
                     ("p" . backward-list)
                     ("K" . sp-kill-hybrid-sexp)
                     ("]" . sp-forward-slurp-sexp)
                     ("[" . sp-backward-slurp-sexp)
                     ("}" . sp-forward-barf-sexp)
                     ("{" . sp-backward-barf-sexp)
                     ("C" . sp-convolute-sexp)
                     ("J" . sp-join-sexp)
                     ("S" . sp-split-sexp)
                     ("R" . sp-raise-sexp)
                     ("\\" . indent-region)
                     ("/" . undo)
                     ("t" . transpose-sexps)
                     ("x" . eval-defun)))
      (define-key map (kbd k) f))
    map))
(map-keymap
 (lambda (_ cmd)
   (put cmd 'repeat-map 'structural-edit-map))
 structural-edit-map)

(defun message-no-format (msg)
  "invoke 'message' without it invoking 'format' (not really)"
  (message "%s" msg))

;; prettify symbols..
(global-prettify-symbols-mode +1)
;; replace lambda text with symbol
(defconst lisp--prettify-symbols-alist
  '(("lambda"  . ?λ)))
;; convert back to text when cursor is over the symbol
(setq prettify-symbols-unprettify-at-point 'right-edge)
;; (add-hook 'emacs-lisp-mode-hook
;;             (lambda ()
;;               (push '(">=" . ?≥) prettify-symbols-alist)))
;; (defun org-set-prettify-symbols ()
;;   (setq-local prettify-symbols-alist
;;               (mapcan (lambda (x) (list x (cons (upcase (car x)) (cdr x))))
;;                       '(("#+begin_src" . ?➤)
;;                         ("#+end_src" . ?⮜)
;;                         ("#+begin_example" . ?)
;;                         ("#+end_example" . ?)
;;                         ("#+header:" . ?)
;;                         ("#+title:" . ?🌐)
;;                         ("#+results:" . ?)
;;                         ("#+name:" . ?📌)
;;                         ("#+call:" . ?)
;;                         (":properties:" . ?)
;;                         ("#+include:" . ?📎);;?🔗) ;;?📌)
;;                         ("#+setupfile:" . ?🔧)
;;                         ("#+filetags:" . "🔑")
;;                         ;;💡🔥🔑💡🚀🔥💎📝🎯📌🔒🎁⭐💌🌺☢️
;;                         ;; 𓍢ִ໋🌷͙֒
;;                         )))
;;   ;; org mode doesnt inherit the global mode for some reason so imma hook it manually
;;   (prettify-symbols-mode))
;; (add-hook 'org-mode-hook #'org-set-prettify-symbols)

(defun ascii-table ()
  "display basic ASCII table (0 thru 128)."
  (interactive)
  (switch-to-buffer "*ASCII*")
  (erase-buffer)
  (setq buffer-read-only nil)        ;; Not need to edit the content, just read mode (added)
  (local-set-key "q" 'bury-buffer)   ;; Nice to have the option to bury the buffer (added)
  (save-excursion (let ((i -1))
                    (insert "ASCII characters 0 thru 127.\n\n")
                    (insert " Hex  Dec  Char|  Hex  Dec  Char|  Hex  Dec  Char|  Hex  Dec  Char\n")
                    (while (< i 31)
                      (insert (format "%4x %4d %4s | %4x %4d %4s | %4x %4d %4s | %4x %4d %4s\n"
                                      (setq i (+ 1  i)) i (single-key-description i)
                                      (setq i (+ 32 i)) i (single-key-description i)
                                      (setq i (+ 32 i)) i (single-key-description i)
                                      (setq i (+ 32 i)) i (single-key-description i)))
                      (setq i (- i 96))))))

(defun current-mpv-artist ()
  (shell-command-to-string "sh -c 'echo \"{ \\\"command\\\": [\\\"get_property\\\", \\\"metadata\\\"] }\" | socat - /tmp/mpv_socket | jq -j .data.artist' 2>/dev/null"))

;; tree-sitter
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;; install those grammars
;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))

;; remap some modes to their tree-sitter alternatives, tree-sitter isnt used by default, yet
(dolist (mapping '((python-mode . python-ts-mode)
                   (css-mode . css-ts-mode)
                   (js-mode . js-ts-mode)
                   (typescript-mode . tsx-ts-mode)
                   (json-mode . json-ts-mode)
                   (yaml-mode . yaml-ts-mode)))
  (add-to-list 'major-mode-remap-alist mapping))

;; FIRST: git clone https://github.com/casouri/tree-sitter-module
;;        bash batch.sh
;; THEN : sudo cp dist/* /usr/local/lib
;; FINALLY:
;;(setq treesit-extra-load-path '("/usr/local/lib"))
;;;; Treesit ;; Eglot
;;(setq treesit-eglot-modes
;;      '((:ts (bash-mode . bash-ts-mode) :pacman "bash-language-server")
;;        (:ts (c++-mode . c++-ts-mode) :pacman "ccls")
;;        (:ts (c-mode . c-ts-mode) :pacman "ccls")
;;        (:ts (cpp-mode . cpp-ts-mode) :pacman "ccls")
;;        (:ts (c-sharp-mode . sharp-ts-mode))
;;        (:ts (cmake-mode . cmake-ts-mode))
;;        (:ts (css-mode . css-ts-mode) :pacman "vscode-css-languageserver")
;;        (:ts (dockerfile-mode . dockerfile-ts-mode))
;;        (:ts (elixir-mode . elixir-ts-mode))
;;        (:ts (glsl-mode . glsl-ts-mode))
;;        (:ts (go-mode . go-ts-mode) :pacman "gopls")
;;        (:ts (heex-mode . heex-ts-mode))
;;        (:ts (html-mode . html-ts-mode) :pacman "vscode-html-languageserver")
;;        (:ts (java-mode . java-ts-mode))
;;        (:ts (javascript-mode . js-ts-mode) :pacman "typescript-language-server")
;;        (:ts (js-json-mode . json-ts-mode) :pacman "vscode-json-languageserver")
;;        (:ts (julia-mode . julia-ts-mode))
;;        (:ts (make-mode . make-ts-mode))
;;        (:ts (markdown-mode . markdown-ts-mode))
;;        (:ts (python-mode . python-ts-mode) :pacman "jedi-language-server")
;;        (:ts (typescript-mode . typescript-ts-mode) :pacman "typescript-language-server")
;;        (:ts (proto-mode . proto-ts-mode))
;;        (:ts (ruby-mode . ruby-ts-mode))
;;        (:ts (rust-mode . rust-ts-mode) :pacman "rust-analyzer")
;;        (:ts (sql-mode . sql-ts-mode))
;;        (:ts (toml-mode . toml-ts-mode))
;;        (:ts (tsx-mode . tsx-ts-mode))
;;        (:ts (verilog-mode . verilog-ts-mode))
;;        (:ts (vhdl-mode . vhdl-ts-mode))
;;        (:ts (wgsl-mode . wgsl-ts-mode))
;;        (:ts (yaml-mode . yaml-ts-mode) :pacman "yaml-language-server")))
;;;; Not mature yet:
;;;; (push '(org-mode . org-ts-mode) major-mode-remap-alist)
;;;; (push '(perl-mode . perl-ts-mode) major-mode-remap-alist)              ;; cpan Perl::LanguageServer
;;(require 'treesit)
;;
;;;; Function to parse the above and make an install command
;;(if (treesit-available-p)
;;    (let ((pacman-install-list (list )))
;;      (dolist (ts-pm treesit-eglot-modes)
;;        (let ((majmode-remap (plist-get ts-pm :ts))
;;              (pacman-cmd (plist-get ts-pm :pacman)))
;;          ;; bind default major-mode to ts-mode
;;          (push majmode-remap major-mode-remap-alist)
;;          ;; populate install cmd
;;          (if pacman-cmd
;;              (unless (member pacman-cmd pacman-install-list)
;;                (push pacman-cmd pacman-install-list)))))
;;      (let ((install-cmd (concat
;;                          "pacman -S --needed "
;;                          (--reduce (concat acc " " it) pacman-install-list))))
;;        (message install-cmd)))
;;  (user-error "Treesitter not available"))
;;

;; (setq treesit-language-source-alist
;;   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
;;     (c "https://github.com/tree-sitter/tree-sitter-c")
;;     (cmake "https://github.com/uyha/tree-sitter-cmake")
;;     (common-lisp "https://github.com/theHamsta/tree-sitter-commonlisp")
;;     (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
;;     (css "https://github.com/tree-sitter/tree-sitter-css")
;;     (csharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
;;     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
;;     (go "https://github.com/tree-sitter/tree-sitter-go")
;;     (go-mod "https://github.com/camdencheek/tree-sitter-go-mod")
;;     (html "https://github.com/tree-sitter/tree-sitter-html")
;;     (js . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
;;     (json "https://github.com/tree-sitter/tree-sitter-json")
;;     (lua "https://github.com/Azganoth/tree-sitter-lua")
;;     (make "https://github.com/alemuller/tree-sitter-make")
;;     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
;;     (python "https://github.com/tree-sitter/tree-sitter-python")
;;     (r "https://github.com/r-lib/tree-sitter-r")
;;     (rust "https://github.com/tree-sitter/tree-sitter-rust")
;;     (toml "https://github.com/tree-sitter/tree-sitter-toml")
;;     (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
;;     (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
;;     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
;;

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
(add-hook 'inferior-haskell-mode-hook 'turn-on-comint-history)
(add-hook 'inferior-python-mode-hook 'turn-on-comint-history)
(add-hook 'shell-mode-hook 'turn-on-comint-history)
(add-hook 'kill-buffer-hook 'comint-write-input-ring)
(defun mapc-buffers (fn)
  (mapc (lambda (buffer)
          (with-current-buffer buffer
            (funcall fn)))
        (buffer-list)))
(defun comint-write-input-ring-all-buffers ()
  (mapc-buffers 'comint-write-input-ring))
(add-hook 'kill-emacs-hook 'comint-write-input-ring-all-buffers)

;; combobulate, see https://www.masteringemacs.org/article/combobulate-structured-movement-editing-treesitter
;; here’s some example code that navigates to the next dictionary, list or set:
(defun move-to-next-container ()
  (interactive)
  (with-navigation-nodes (:nodes '("dictionary" "set" "list"))
    (combobulate-visual-move-to-node
     (combobulate-nav-logical-next) t)))

(defvar combobulate-edit-map
  (let ((map (make-sparse-keymap)))
    (pcase-dolist (`(,k . ,f)
                   '(("u" . combobulate-navigate-up-list-maybe)
                     ("f" . combobulate-navigate-forward)
                     ("b" . combobulate-navigate-backward)
                     ("d" . combobulate-navigate-down-list-maybe)
                     ("k" . combobulate-kill-node-dwim)
                     ("n" . combobulate-navigate-next)
                     ("p" . combobulate-navigate-previous)
                     ("J" . combobulate-splice)
                     ("a" . combobulate-navigate-beginning-of-defun)
                     ("e" . combobulate-navigate-end-of-defun)
                     ("\\" . indent-region)
                     ("/" . undo)
                     ("t" . combobulate-transpose-sexps)
                     ("x" . eval-defun)))
      (define-key map (kbd k) f))
    map))
(map-keymap
 (lambda (_ cmd)
   (put cmd 'repeat-map 'combobulate-edit-map))
 combobulate-edit-map)

;; dont deactivate the mark on non-shift commands
(setq shift-select-mode 'permanent)

;; from https://emacs.stackexchange.com/questions/58073/how-to-find-inheritance-of-modes
(defun derived-modes (mode)
  "Return a list of the ancestor modes that MODE is derived from."
  (let ((modes   ())
        (parent  nil))
    (while (setq parent (get mode 'derived-mode-parent))
      (push parent modes)
      (setq mode parent))
    (setq modes  (nreverse modes))))

;; default shell for M-x term
(setq explicit-shell-file-name "zsh")

;; from https://emacs.stackexchange.com/questions/16688/how-can-i-escape-the-in-org-mode-to-prevent-bold-fontification
;; (defun my-bold (contents backend info)
;;   (when (org-export-derived-backend-p backend 'latex)
;;     (replace-regexp-in-string "\\`\\\\textbf{\\(.+\\)}"
;;                               "\\\\ast{}\\1\\\\ast{}" contents)))
;; (add-to-list 'org-export-filter-bold-functions 'my-bold)

;; for now, this is the one with the packages on my system
;; (setq python-shell-interpreter "python3.10")
;; (setq python-interpreter "python3.10")

;; add ~/.emacs.d to load-path
(push (concat user-emacs-directory "/lisp") load-path)
(require 'setup-packages) ;; load setup-packages.el
(require 'setup-org) ;; load setup-org.el
(require 'setup-evil) ;; load setup-evil.el
(require 'setup-keys) ;; load setup-keys.el
(require 'setup-theme) ;; load setup-theme.el
(require 'setup-dired) ;; load setup-dired.el

;; open agenda on startup
(add-hook 'after-init-hook
          (lambda ()
;;            (org-roam-db-sync)
            ;; (org-agenda-list)
            ;; (delete-other-windows)
            (when (file-exists-p persp-state-default-file)
              (persp-state-load persp-state-default-file)
              (persp-switch "main"))
            ;; (switch-to-light-theme)
            ;; (switch-to-theme 'minimal-light)
            ;; (switch-to-theme 'darktooth-darker)
            ;; (switch-to-theme 'ample-flat)
            ;; (switch-to-theme 'ample-light)
            ;; (switch-to-theme 'ample)
            ;; (switch-to-theme 'acme)
            ;; (switch-to-theme 'doom-gruvbox-light)
            ;; (switch-to-theme 'stimmung-themes-light)
            ;; (switch-to-theme 'stimmung-themes-dark)
            ;; (switch-to-theme 'ef-melissa-light)
            ;; (switch-to-theme 'ef-tritanopia-dark)
            ;; (switch-to-theme 'ef-melissa-dark)
            (switch-to-theme 'ef-autumn)
            ;; (switch-to-theme 'gruvbox-light-soft)
            ;; (switch-to-theme 'gruvbox-dark-hard)
            ;; (switch-to-theme 'modus-operandi)
            ;; (switch-to-theme 'modus-vivendi)
            ;; (switch-to-tango-theme)
            ;; (set-face-background hl-line-face "PeachPuff3")
            ;; (switch-to-theme 'doom-sourcerer)
            ;;(switch-to-darktooth-theme)
            ))
;; enable flyspell (spell checking)
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
;; ;; flyspell buffer when its opened
;; (add-hook 'flyspell-mode-hook #'flyspell-buffer)

;; sometimes useful for refactoring old tex notes
(defun replace-dollar-signs ()
  (interactive)
  (replace-regexp "\\$\\(.*?\\)\\$" "\\\\(\\1\\\\)"))

;; disable some modes for large files (otherwise emacs will hang..)
;; (add-hook 'find-file-hook 'my-find-file-check-make-large-file-read-only-hook)
;; there's also find-file-literally i guess
(defun conditional-disable-modes ()
  (unless (eq major-mode 'pdf-view-mode)
    (when (> (buffer-size) (* 1024 512))
      (flycheck-mode -1)
      (flyspell-mode -1)
      (font-lock-mode -1)
      (fundamental-mode)
      (which-function-mode -1)
      (linum-mode 0)
      (lsp-mode 0)
      )))
(add-hook 'find-file-hook 'conditional-disable-modes)