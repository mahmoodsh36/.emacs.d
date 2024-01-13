;; simple storage
;; https://stackoverflow.com/questions/2321904/elisp-how-to-save-data-in-a-file
(defun dump-vars-to-file (varlist filename)
  "simplistic dumping of variables in VARLIST to a file FILENAME"
  (save-excursion
    (let ((buf (find-file-noselect filename)))
      (set-buffer buf)
      (erase-buffer)
      (dump varlist buf)
      (save-buffer)
      (kill-buffer))))
(defun dump (varlist buffer)
  "insert into buffer the setq statement to recreate the variables in VARLIST"
  (cl-loop for var in varlist do
        (print (list 'setq var (list 'quote (symbol-value var)))
               buffer)))
(defun checkit ()
  (let ((a '(1 2 3 (4 5)))
        (b '(a b c))
        (c (make-vector 3 'a)))
    (dump-vars-to-file '(a b c) "/some/path/to/file.el")))
;; (setq a (quote (1 2 3 (4 5))))
;; (setq b (quote (a b c)))
;; (setq c (quote [a a a]))