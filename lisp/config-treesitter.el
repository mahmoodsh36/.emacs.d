(defun grab-commands-from-latex-file (latex-file)
  (with-file-as-current-buffer
   latex-file
   (let ((root (treesit-parse-string (buffer-string) 'latex))
         (results))
     (map-treesitter-node
      root
      (lambda (node)
        (when (equal (treesit-node-field-name node) "command")
          (let ((text (treesit-node-text node)))
            (push text results)))))
     results)))

(defun map-treesitter-node (node func)
  (funcall func node)
  (dolist (child (treesit-node-children node))
    (map-treesitter-node child func)))

(defun grab-all-latex-commands ()
  (append
   (mapcar
    'grab-commands-from-latex-file
    (list-note-files))))

(defun shorten-strings (strings length)
  "Shorten strings to unique prefixes of the specified length.
If a prefix is repeated, append a number to make it unique."
  (let ((prefix-count (make-hash-table :test 'equal))
        (unique-prefixes '()))
    (dolist (s strings unique-prefixes)
      ;; only process strings that are at least as long as the desired prefix length
      (if (>= (length s) length)
          (let ((prefix (substring s 0 length)))
            ;; check if this prefix has already been used
            (if (gethash prefix prefix-count)
                (progn
                  ;; increment the count for this prefix and append the number
                  (puthash prefix (1+ (gethash prefix prefix-count)) prefix-count)
                  (push (concat prefix (number-to-string (gethash prefix prefix-count))) unique-prefixes))
              (progn
                ;; if it's the first time, use the prefix without a number
                (puthash prefix 0 prefix-count)
                (push prefix unique-prefixes))))))
    ;; return the unique prefixes in the order they were processed
    (nreverse unique-prefixes)))

(let ((strings '("apple" "apppp" "banana" "banan" "btatt" "ball" "ball" "ball")))
  (shorten-strings strings 4))

(provide 'config-treesitter)