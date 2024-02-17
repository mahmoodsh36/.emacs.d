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
  "Eval BODY without affected buffer modification status"
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

(provide 'setup-utils)