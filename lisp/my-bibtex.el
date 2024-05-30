;; some utilities for bibtex

(defun parse-bibtex-buffer ()
  (let ((entries))
    (bibtex-map-entries
     (lambda (key beg end)
       (let ((text (buffer-substring-no-properties beg end)))
         (goto-char beg)
         (let ((entry (bibtex-parse-entry)))
           (push entry entries)))))
    entries))

(provide 'my-bibtex)