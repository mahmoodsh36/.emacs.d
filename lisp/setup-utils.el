(defun buffer-contains-substring (string)
  "check if the current buffer contains a specific string"
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (not (eq nil (search-forward string nil t))))))

(defun buffer-contains-math ()
  "check if the current buffer contains any math equations (latex blocks)"
  (or
   (buffer-contains-substring "$")
   (buffer-contains-substring "\\(")
   (buffer-contains-substring "\\[")))

(defmacro save-buffer-modified-p (&rest body)
  "eval BODY without affected buffer modification status"
  `(let ((buffer-modified (buffer-modified-p))
         (buffer-undo-list t))
     (unwind-protect
         ,@body
       (set-buffer-modified-p buffer-modified))))

(defun find-math-files (basedir)
  "find all org files in a directory that are math files and tag them with 'math' tag"
  (interactive)
  (dolist (file (directory-files-recursively basedir ".*\\.org$"))
    (ignore-errors (with-current-buffer (or (find-buffer-visiting file) (find-file-noselect file))
                     (beginning-of-buffer)
                     (org-id-get-create)
                     (ignore-errors (update-math-file))
                     (save-buffer)))))

(defun update-math-files ()
  "go through all roam files and check each for math formulas and update the math tag"
  (dolist (file (all-roam-files))
    (with-current-buffer (or (find-buffer-visiting file) (find-file-noselect file))
      (update-math-file)
      (save-buffer))))

;; (defun buffer-contains-code ()
;;   "check if the buffer contains an org-babel source block"
;;   (interactive)
;;   (org-element-map
;;       (org-element-parse-buffer 'element)
;;       'src-block
;;     (lambda (h) t)
;;     ;; (or (eq (org-element-property :todo-type h)
;;     ;;         'todo)
;;     ;;     (eq (org-element-property :todo-type h)
;;     ;;         'done)))
;;     nil 'first-match))

(defun my-process-file (file funcs)
  (let ((present-buffer (find-buffer-visiting file)))
    (with-current-buffer (or present-buffer (find-file-noselect file))
      (mapcar #'funcall funcs)
      (when (not present-buffer)
        (kill-buffer (current-buffer))))))

(defun map-files (files funcs)
  "run the functions `funcs' on each file in `files'"
  (mapcar (lambda (file) (my-process-file file funcs)) files))

(defun map-dir-files (dir funcs &optional regex)
  "run the functions `funcs' on each file on the directory `dir', if `regex' is supplied, reduce the list of files to the ones matched by the regex"
  (map-files (directory-files dir t regex) funcs))

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

(provide 'setup-utils)