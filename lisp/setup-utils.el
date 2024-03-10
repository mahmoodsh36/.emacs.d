(defun buffer-contains-substring (string)
  "check if the current buffer contains a specific string"
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (not (eq nil (search-forward string nil t))))))

;; (defun buffer-contains-math ()
;;   "check if the current buffer contains any math equations (latex blocks)"
;;   (or
;;    (buffer-contains-substring "$")
;;    (buffer-contains-substring "\\(")
;;    (buffer-contains-substring "\\[")))
(defun buffer-contains-math ()
  "check if the current buffer contains any math equations (latex blocks), isnt smart about it obviously"
  (buffer-contains-substring "\\"))

(defmacro save-buffer-modified-p (&rest body)
  "eval BODY without affected buffer modification status"
  `(let ((buffer-modified (buffer-modified-p))
         (buffer-undo-list t))
     (unwind-protect
         ,@body
       (set-buffer-modified-p buffer-modified))))

(defun my-process-file (file func)
  (let ((present-buffer (find-buffer-visiting file)))
    (with-current-buffer (or present-buffer (find-file-noselect file))
      (funcall func)
      (when (not present-buffer)
        (kill-buffer (current-buffer))))))

(defun map-files (files func)
  "run the functions `funcs' on each file in `files'"
  (mapcar (lambda (file) (my-process-file file func)) files))

(defun map-dir-files (dir func &optional regex)
  "run the functions `funcs' on each file on the directory `dir', if `regex' is supplied, reduce the list of files to the ones matched by the regex"
  (map-files (directory-files dir t regex) func))

;; sometimes useful for refactoring old tex notes
(defun replace-dollar-signs ()
  (interactive)
  (replace-regexp "\\$\\(.*?\\)\\$" "\\\\(\\1\\\\)"))

;; from https://emacs.stackexchange.com/questions/58073/how-to-find-inheritance-of-modes
(defun derived-modes (mode)
  "Return a list of the ancestor modes that MODE is derived from."
  (let ((modes   ())
        (parent  nil))
    (while (setq parent (get mode 'derived-mode-parent))
      (push parent modes)
      (setq mode parent))
    (setq modes  (nreverse modes))))

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

(defun temp-file (EXT)
  (format "%stmp_%s.%s" (from-brain "out/") (generate-random-string 7) EXT))

(defun any (pred list)
  "return `t' if `pred' returns `t' for any items in `list'"
  (while (and list (not (funcall pred (car list))))
    (pop list))
  (car list))

(defun current-filename-no-ext ()
  "current filename without extension"
  (file-name-sans-extension
   (file-name-nondirectory (buffer-file-name))))

(defun from-brain (filename)
  "return `filename', prefixed by the path to the brain dir"
  (join-path *brain-dir* filename))

(defun from-cache (filename)
  "return 'filename' prefixed with cache dir path"
  (from-brain (concat "out/" filename)))
(defalias 'cached-file 'from-cache)

;; clean up ob-jupyter source block output
;; from henrik lissner
(defun my/org-babel-jupyter-strip-ansi-escapes-block ()
  (when (string-match-p "^jupyter-"
                        (nth 0 (org-babel-get-src-block-info)))
    (unless (or
             ;; ...but not while Emacs is exporting an org buffer (where
             ;; `org-display-inline-images' can be awfully slow).
             (bound-and-true-p org-export-current-backend)
             ;; ...and not while tangling org buffers (which happens in a temp
             ;; buffer where `buffer-file-name' is nil).
             (string-match-p "^ \\*temp" (buffer-name)))
      (save-excursion
        (when-let* ((beg (org-babel-where-is-src-block-result))
                    (end (progn (goto-char beg)
                                (forward-line)
                                (org-babel-result-end))))
          (ansi-color-apply-on-region (min beg end) (max beg end)))))))
(add-hook 'org-babel-after-execute-hook
          #'my/org-babel-jupyter-strip-ansi-escapes-block)

(defun led (key-str)
  "return the keybinding `key-str', which is taken as a string, prefixed by the leader key defined in '*leader-key*'"
  (concat *leader-key* " " key-str))

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

(defun join-path (&rest paths)
  "join file paths using a forward slash"
  (let ((mypath (car paths)))
    (dolist (path (cdr paths))
      (setq mypath (concat mypath "/" path)))
    (file-truename mypath)))

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

(defun run-command-show-output (cmd)
  "run shell command and show continuous output in new buffer"
  (interactive)
  (progn
    (start-process-shell-command cmd cmd cmd)
    (display-buffer cmd)
    (end-of-buffer-other-window nil)))

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

(defun message-no-format (msg)
  "invoke 'message' without it invoking 'format' (not really)"
  (message "%s" msg))

(defun open-wezterm-here ()
  (interactive)
  (async-shell-command "wezterm ."))

(defun is-android-system ()
  (or (string-equal system-type "android")
      (eq (shell-command "command -v termux-setup-storage") 0)))

(defun is-substring (small big)
  "whether `small' is a substring of `big'"
  (string-match-p (regexp-quote small) big))

(provide 'setup-utils)