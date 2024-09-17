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

(defun shell-command-to-string-no-stderr (cmd)
  (with-output-to-string
    (with-current-buffer
        standard-output
      (process-file shell-file-name nil '(t nil) nil shell-command-switch cmd))))

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
  (file-truename (join-path *brain-dir* filename)))

(defun from-emacsd (filename)
  "return `filename', prefixed by the path to the emacs config dir"
  (file-truename (join-path user-emacs-directory filename)))

(defun from-cache (filename)
  "return 'filename' prefixed with cache dir path"
  (from-brain (concat "out/" filename)))
(defalias 'cached-file 'from-cache)

(defun from-notes (filename)
  "return `filename', prefixed by the path to the notes dir `*notes-dir*'"
  (file-truename (join-path *notes-dir* filename)))

(defun from-template (filename)
  (file-truename (join-path *template-html-dir* filename)))

(defun from-data (filename)
  "return `filename', prefixed by the path to the brain dir"
  (file-truename (join-path *data-dir* filename)))

;; clean up ob-jupyter source block output
;; from henrik lissner
;; (defun my/org-babel-jupyter-strip-ansi-escapes-block ()
;;   (when (string-match-p "^jupyter-"
;;                         (nth 0 (org-babel-get-src-block-info)))
;;     (unless (or
;;              ;; ...but not while Emacs is exporting an org buffer (where
;;              ;; `org-display-inline-images' can be awfully slow).
;;              (bound-and-true-p org-export-current-backend)
;;              ;; ...and not while tangling org buffers (which happens in a temp
;;              ;; buffer where `buffer-file-name' is nil).
;;              (string-match-p "^ \\*temp" (buffer-name)))
;;       (save-excursion
;;         (when-let* ((beg (org-babel-where-is-src-block-result))
;;                     (end (progn (goto-char beg)
;;                                 (forward-line)
;;                                 (org-babel-result-end))))
;;           (ansi-color-apply-on-region (min beg end) (max beg end)))))))
;; (add-hook 'org-babel-after-execute-hook
;;           #'my/org-babel-jupyter-strip-ansi-escapes-block)

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
  (let ((main (car paths)))
    (dolist (other (cdr paths))
      (setq main
            (concat
             main
             (if (or (string-prefix-p "/" other)
                     (string-suffix-p "/" main))
                 ""
               "/")
             other)))
    (string-replace "//" "/" main)))

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

(defun is-substring (small big)
  "whether `small' is a substring of `big'"
  (string-match-p (regexp-quote small) big))

;; this cannot work with duplicates
;; propertize cannot handle empty strings
(defun completing-read-cons (prompt collection)
  (let ((new-collection)
        (id-prop 'myid))
    (dotimes (i (length collection))
      (let ((entry (elt collection i)))
        (push (propertize (car entry) id-prop i) new-collection)))
    (let* ((key (completing-read prompt new-collection))
           (idx (get-text-property 0 id-prop key)))
      (elt collection idx))))

;; this can work with duplicates
(defun completing-read-cons-ivy (prompt collection)
  "an alternative to `completing-read' that returns the whole cons from the alist `collection' instead of just the key, and handles duplicates \"properly\". assumes `minibuffer-allow-text-properties' is set to `t'. this depends on `ivy' and doesnt work with the builtin `completing-read'."
  (let ((new-collection)
        (id-prop 'myid))
    (dotimes (i (length collection))
      (let ((entry (elt collection i)))
        (push (propertize (car entry) id-prop i) new-collection)))
    (let* ((key (ivy-read prompt new-collection))
           (idx (get-text-property 0 id-prop key)))
      (elt collection idx))))

(defun completing-read-cons-consult (prompt collection)
  "an alternative to `completing-read' that returns the whole cons from the alist `collection' instead of just the key, and handles duplicates \"properly\". assumes `minibuffer-allow-text-properties' is set to `t'. this depends on consult (package)"
  (let ((new-collection)
        (id-prop 'myid))
    (dotimes (i (length collection))
      (let ((entry (elt collection i)))
        (push (consult--tofu-append (car entry) i) new-collection)))
    (let* ((key (completing-read prompt new-collection))
           (idx (consult--tofu-get key)))
      (elt collection idx))))

;; https://emacs.stackexchange.com/questions/2298/how-do-i-force-re-evaluation-of-a-defvar
(defun my/eval-buffer ()
  "Execute the current buffer as Lisp code.
Top-level forms are evaluated with `eval-defun' so that `defvar'
and `defcustom' forms reset their default values."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (forward-sexp)
      (eval-defun nil))))

(defmacro with-eval-after-load-all (my-features &rest body)
  "Run BODY after all MY-FEATURES are loaded.
example usage: (with-eval-after-load-all '(org) (message \"hi\"))"
  (if (eval my-features)
      `(with-eval-after-load (quote ,(car (eval my-features)))
         (with-eval-after-load-all (quote ,(cdr (eval my-features))) ,@body))
    `(progn ,@body)))

(defun open-devdocs-entry (entry)
  "simple function to open the devdocs reference for a specific manual/library/whatever the package accepts, by its name"
  (let ((devdocs-current-docs (list entry)))
    (devdocs-lookup)))

(defun buffer-name-incremented (name)
  "generate a name for a buffer named NAME, if such a buffer already exists use NAME but append
1 at the end, so that it becomes NAME-1, if buffer NAME-1 exists, use NAME-2 and so on.
this function doesnt create the buffer itself, it merely generates the proper name, use `get-buffer-create'"
  (let ((index 1)
        (original-name name))
    (while (get-buffer name)
      (setq name (format "%s-%s" original-name index))
      (cl-incf index))
    name))

(defun term-create-name-incremented ()
  "start a terminal and rename buffer."
  (interactive)
  (term *shell-program*)
  (let ((buffer-name (buffer-name-incremented "myterm")))
    (rename-buffer buffer-name t)
    buffer-name))

(defun shell-create-name-incremented ()
  (interactive)
  (let ((buffer-name (buffer-name-incremented "shell")))
    (shell buffer-name)
    buffer-name))

(defun new-term-with-cmd (cmd)
  (with-current-buffer (term-create-name-incremented)
    (term-send-raw-string (format "%s\n" cmd))))

(defun new-shell-with-cmd (cmd)
  (with-current-buffer (shell-create-name-incremented)
    (comint-send-string (current-buffer) (format "%s\n" cmd))))

(defun kill-this-buffer-volatile ()
  "kill current buffer, even if it has been modified."
  (interactive)
  (set-buffer-modified-p nil)
  (kill-this-buffer))

;; from xah's website or whatever
(defun copy-file-path (&optional DirPathOnlyQ)
  "copy current buffer file path or dired path.
result is full path.
if `universal-argument' is called first, copy only the dir path.
if in dired, copy the current or marked files.
if a buffer is not file and not dired, copy value of `default-directory'."
  (interactive "P")
  (let (($fpath
         (if (string-equal major-mode 'dired-mode)
             (progn
               (let (($result (mapconcat 'identity (dired-get-marked-files) "\n")))
                 (if (equal (length $result) 0)
                     (progn default-directory )
                   (progn $result))))
           (if (equal major-mode 'notmuch-show-mode)
               (org-notmuch-store-link)
             (if (buffer-file-name)
                 (buffer-file-name)
               (expand-file-name default-directory))))))
    (kill-new
     (if DirPathOnlyQ
         (progn
           (message "directory copied: %s" (file-name-directory $fpath))
           (file-name-directory $fpath))
       (progn
         (message "file path copied: %s" $fpath)
         $fpath)))))

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

(defun sudo-find-file (file-name)
  "like find file, but opens the file as root using tramp"
  (interactive (list (read-file-name "file: " "/sudo::/")))
  (let ((tramp-file-name (expand-file-name file-name)))
    (find-file tramp-file-name)))

(defun current-mpv-artist ()
  (shell-command-to-string "sh -c 'echo \"{ \\\"command\\\": [\\\"get_property\\\", \\\"metadata\\\"] }\" | socat - /tmp/mpv_socket | jq -j .data.artist' 2>/dev/null"))

(defun insert-random-string (&optional num)
  (interactive)
  (or num (setq num 7))
  (insert (generate-random-string num)))
(global-set-key (kbd "C-c R") #'insert-random-string)

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

(defun my-insert-newline-same-indentation ()
  "Insert a newline with the same indentation as the current line."
  (interactive)
  (end-of-line)
  (let ((current-indentation
         (save-excursion
           (beginning-of-line)
           (skip-chars-forward " \t")
           (- (point) (line-beginning-position)))))
    (newline)
    (insert (make-string current-indentation ? ))
    (evil-insert 0)))

(defun is-point-at-some-bol (pos)
  "whether POS points at the beginning of some line"
  (save-excursion
    (goto-char pos)
    (equal (point) (pos-bol))))

;; unadvice something https://emacs.stackexchange.com/questions/24657/unadvise-a-function-remove-all-advice-from-it
(defun my-unadvice (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

(defun file-for-blk-id (blk-id)
  (plist-get (car (blk-find-by-id blk-id)) :filepath))

(defun blk-open-by-id (blk-id)
  (find-file (plist-get (car (blk-find-by-id blk-id)) :filepath)))

(defun parse-bibtex-buffer ()
  (let ((entries))
    (bibtex-map-entries
     (lambda (key beg end)
       (let ((text (buffer-substring-no-properties beg end)))
         (goto-char beg)
         (let ((entry (bibtex-parse-entry)))
           (push entry entries)))))
    entries))

(defmacro memoized (seconds &rest body)
  )

(defun replace-file-extension (filepath new-ext)
  "return FILEPATH with the extension changed to NEW-EXT"
  (concat (file-name-sans-extension filepath) "." new-ext))

(defun libxml-parse-html-string (html)
  (with-temp-buffer
    (insert html)
    (libxml-parse-html-region)))

(defun libxml-render-html-string (libxml-format)
  (with-temp-buffer
    (dom-print libxml-format)
    (buffer-string)))

(defun libxml-map-nodes (libxml-node fn)
  (when libxml-node
    (funcall fn libxml-node)
    (when (consp libxml-node) ;; sometimes the node itself is just a string
      (dolist (node-child (cddr libxml-node)) ;; dom.el also provides dom-children
        (libxml-map-nodes node-child fn)))))

(defun libxml-parse-html-string-no-body-or-html (html)
  (libxml-parse-html-string html))

(provide 'setup-utils)