(defcustom blk-directories
  (list (expand-file-name "~/notes/"))
  "directories to look for files in"
  )

(defcustom blk-list-directories-recursively
  nil
  "whether to look for files recursively in `blk-directories', if set to `t' may have severe consequences on speed"
  )

(defcustom blk-emacs-patterns
  (list (list :title "titled files" ;; name of the pattern
              :filename-regex ".*\.org"
              :anchor-regex "^#\\+title: [^:\n]+"
              :title-function 'blk-value-after-space
              :extract-id-function (lambda (matched-text) ()))
        ;; (list :title "org denote link"
        ;;         :filename-regex "*.org"
        ;;         :pattern "[[denote:%n]]"
        ;;         :with-id-pattern "[[denote:%n][%i]]" ;; %n for name, %i for identifier
        ;;         :identifier-pattern "[^\n\s\\[\\]]*"
        ;;         :name-pattern "[^\n\s\\[\\]]*")
        ;; (list :file-regex "*.tex" :line-regex "" :name-regex "" :link-regex "" :title-function (lambda () ()))
        ;; (list :title "org mode link")
        ;; (list :title "markdown link")
        ;; (list :title "latex link")
        )
  ":title is the title/type of the pattern, :filename-regex is the regex to match files to be grepped :anchor-regex is the regex for matching blocks of text that contain the target value which is then passed to :title-function to be turned into the final desired value to be passed to completing-read and that identifies the target, :link-function is the function that gets the id to be used when creating links to the target, the need for :link-function over :title-function is that an id and a name for the target can be different, as an id can be a random sequence but a name could be a more memorable sequence of characters. if the user wants the id to be the name itself, they may only supply :title-function"
  )

(defcustom blk-rg-patterns
  (list (list :title "titled org files or blocks"
              :filename-regex ".*\\.org"
              :anchor-regex "(:title|:alias|#\\+title:|#\\+alias:|#\\+name:)\\s+[^:\\n]+"
              :title-function 'blk-value-after-space
              :extract-id-function
              (lambda (grep-data)
                (let ((elm (org-element-at-point)))
                  (when elm
                    (let* ((elm-type (org-element-type elm))
                           (id (cl-case elm-type
                                 ('special-block (org-element-property :name elm))
                                 ('keyword (or
                                            (cdar (org-collect-keywords '("identifier")))
                                            (org-id-get))))))
                      (or id (plist-get grep-data :value)))))))
        (list :title "org headers"
              :filename-regex ".*\\.org"
              :anchor-regex "^\\*+\\s.*"
              :title-function 'blk-value-after-space
              :extract-id-function (lambda (grep-data)
                                     (or (org-id-get) (plist-get grep-result :value))))
        (list :title "org mode link"
              :filename-regex ".*\\.org"
              :anchor-regex "\\[\\[[a-z]+:%i\\]\\]|\\[\\[blk:%i\\]\\[%t\\]\\]"
              :id-regex "[^\\]\\[]+"
              :title-regex "[^\\]\\[]+"
              :extract-id-function (lambda (grep-data)
                                     ))
        (list :title "latex link"
              :filename-regex ".*\\.org|.*\\.tex"
              :anchor-regex "\\label{%i}"
              :id-regex "[^\\{\\}]+"
              :extract-id-function (lambda (grep-data)
                                     ))
        )
  "the pattern table for ripgrep")

(defcustom blk-patterns
  blk-rg-patterns
  "the list of patterns to invoke the grepper with")

(defun blk-value-after-space (str)
  (string-trim (string-join (cdr (split-string str " ")) " ")))

(defconst
  blk-grepper-rg
  '(:command "rg --field-match-separator '\t' --regexp '%r' %f --no-heading --line-number --ignore-case --byte-offset --only-matching --with-filename"
    :delimiter "\t"))

(defcustom blk-grepper
  ;; 'blk-emacs-grep
  blk-grepper-rg
  "the program to use for grepping files, could be 'emacs or a string representing a shell command to be formatted with the regex to grep for and the file list")

(defconst
  blk-grepper-grep
  '(:command "grep -e '%s' '%s' --no-heading --line-number --ignore-case --byte-offset --only-matching"
             :separator ":"
             :regex-or "|"))

(defmacro blk-with-file-as-current-buffer (file &rest body)
  (let ((present-buffer (gensym))
        (result (gensym)))
    `(let ((,present-buffer (find-buffer-visiting ,file)))
       (save-excursion
         (with-current-buffer (or (find-buffer-visiting ,file) (find-file-noselect ,file t t))
           (setq ,result (progn ,@body))
           (when (not ,present-buffer)
             (kill-buffer (current-buffer)))
           ,result)))))

(defun blk-emacs-grep (pattern-table files)
  (let ((results))
    (dolist (pattern pattern-table)
      (when (and (plist-get pattern :title-function) (string-match-p "%i" (plist-get pattern :anchor-regex)))
        (let ((matched-buffers
               (blk-str-list-matches (plist-get pattern :filename-regex)
                                     (mapcar 'buffer-file-name (blk-list-buffers))))
              (matched-files
               (blk-str-list-matches (plist-get pattern :filename-regex)
                                     (blk-list-files))))
          (dolist (filename (cl-union matched-buffers matched-files))
            (blk-with-file-as-current-buffer
             filename
             (let ((matches (blk-string-search-regex (plist-get pattern :anchor-regex)
                                                     (substring-no-properties (buffer-string)))))
               (dolist (match matches)
                 (push (list :position (cdr match)
                             :filepath buffer-file-name
                             :value (funcall (plist-get pattern :title-function) (car match))
                             :matched-pattern pattern)
                       results))))))))
    results))

(defun blk-string-search-regex (regex str)
  "returns matches of `regex' found in the string `str', a list with conses of the form (match . position) is returned"
  (let ((pos 0)
        (matches))
    (cl-loop for match-pos = (string-match regex str pos)
             while match-pos do
             (push (cons (match-string 0 str) match-pos) matches)
             (setq pos (1+ match-pos)))
    matches))

(defun blk-list-files ()
  "list the directories `blk-directories' to use for grepping links/references"
  (let ((files))
    (dolist (dir blk-directories)
      (setq files (append files (if blk-list-directories-recursively
                                    (mapcar (lambda (filename) (concat dir "/" filename))
                                            (directory-files-recursively dir ""))
                                  (mapcar (lambda (filename)
                                            (concat dir "/" filename))
                                          (directory-files dir))))))
    files))

(defun blk-list-buffers ()
  (buffer-list))

(defun blk-list-entries ()
  (let* ((grep-results (pcase (type-of blk-grepper)
                         ('cons (blk-run-grep-cmd blk-grepper blk-patterns (blk-list-files)))
                         (_ (funcall blk-grepper blk-patterns (blk-list-files)))))
         (entries (mapcar (lambda (grep-result)
                            (propertize (plist-get grep-result :value) 'grep-data grep-result))
                          grep-results)))
    entries))

(defun blk-str-list-matches (str-list regex)
  "get the strings from `str-list' matching the regex `regex'"
  (cl-remove-if-not
   (lambda (str)
     (string-match regex str))
   str-list))

(defun blk-run-grep-cmd (cmd patterns files)
  (let ((matches))
    (dolist (pattern patterns)
      (when (and (plist-get pattern :title-function) (not (string-match-p "%i" (plist-get pattern :anchor-regex))))
        (let* ((matching-files (blk-str-list-matches files (plist-get pattern :filename-regex)))
               (files-str (string-join (mapcar 'shell-quote-argument matching-files) " "))
               (full-cmd (format-spec (plist-get cmd :command)
                                      `((?f . ,files-str)
                                        (?r . ,(plist-get pattern :anchor-regex)))))
               (out (shell-command-to-string full-cmd))
               (sep (plist-get cmd :delimiter)))
          (dolist (line (split-string out "\n"))
            (when (not (string-empty-p line))
              (let* ((line-entries (split-string line sep))
                     (filepath (car line-entries))
                     (line-number (string-to-number (cadr line-entries)))
                     (position (string-to-number (caddr line-entries)))
                     (match-text (string-join (cdddr line-entries) sep)))
                (push (list :value (funcall (plist-get pattern :title-function) match-text)
                            :position (1+ position) ;; grep starts at position 0, while emacs doesnt
                            :line-number line-number
                            :matched-pattern pattern
                            :filepath filepath)
                      matches)))))))
    matches))

(defun blk-find (text)
  "find entries defined by patterns in `blk-patterns' using the grepper `blk-grepper', when found, visit it"
  (interactive
   (list (let ((minibuffer-allow-text-properties t))
           (completing-read "entry " (blk-list-entries)))))
  (if (get-text-property 0 'grep-data text)
      (let* ((grep-data (get-text-property 0 'grep-data text))
             (filepath (plist-get grep-data :filepath))
             (position (plist-get grep-data :position)))
        (find-file filepath)
        (goto-char position))
    (message "%s not found" text)))

(defun blk-insert-link (text)
  "insert a link to entries defined by patterns in `blk-patterns' using the grepper `blk-grepper', when found, visit it, the pattern of the link is also defined by entries in `blk-patterns'"
  (interactive
   (list (let ((minibuffer-allow-text-properties t))
           (completing-read "entry " (blk-list-entries)))))
  (if (get-text-property 0 'grep-data text)
      (let* ((grep-data (get-text-property 0 'grep-data text))
             (id (plist-get grep-data :filepath)))
        )
    (message "%s not found" text)))

(provide 'blk)