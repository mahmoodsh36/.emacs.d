(if (file-exists-p "/home/mahmooz/work/blk/")
    ;; (push "/home/mahmooz/work/blk/" load-path)
    (use-package blk
      :after (org org-transclusion)
      :load-path "/home/mahmooz/work/blk/"
      :config
      (my-blk-config))
  (use-package blk
    :after (org org-transclusion)
    :ensure ( :host github :repo "mahmoodsheikh36/blk")
    :config
    (my-blk-config)))

(defun my-blk-config ()
  ;; (defvar blk-find-history nil)

  ;; (setq blk-grepper blk-grepper-grep)
  ;; (setq blk-patterns blk-grep-patterns)
  ;; (setq blk-grepper 'blk-grepper-emacs)
  ;; (setq blk-patterns blk-emacs-patterns)

  (setq blk-directories
        (list (from-brain "notes")
              (from-brain "daily")))

  ;; org-transclusion integration
  (with-eval-after-load 'org-transclusion
    (blk-configure-org-transclusion)
    (defun org-transclusion-content-insert-advice (fn keyword-values type content sbuf sbeg send copy)
      (when (not (string-suffix-p "\n" content))
        (setq content (format "%s\n" content)))
      (funcall fn keyword-values type content sbuf sbeg send copy))
    (advice-add #'org-transclusion-content-insert :around #'org-transclusion-content-insert-advice))

  ;; use auctex
  (setq blk-tex-env-at-point-function 'blk-auctex-env-at-point-bounds)

  ;; allow for recursive grep
  ;; (setq blk-search-recursively t)

  ;; enable group search (group things together), for now its somewhat slow :(
  ;; (setq blk-enable-groups t)

  ;; add the :defines pattern
  (dolist (pattern-table '(blk-rg-patterns blk-grep-patterns))
    (add-to-list pattern-table (list :title "definition"
                                     :glob "*.org"
                                     :anchor-regex "(:defines|:mentions)\\s+[^:]+"
                                     :title-function 'blk-value-after-space-upto-colon
                                     :extract-id-function 'blk-org-id-at-point)))
  (setq blk-patterns blk-rg-patterns)
  ;; (setq blk-grepper blk-grepper-rg)
  ;; (setq blk-grepper blk-grepper-grep)

  (setq blk-ignored-files
        (list
         ;; "/home/mahmooz/brain/notes/1713104680.org" ;; this affects exporting :/ (blk.html doesnt get written..)
         "org-tex"))

  ;; add the :defines pattern for the emacs grepper
  ;; (dolist (pattern-table '(blk-emacs-patterns))
  ;;   (add-to-list pattern-table (list :title "definition"
  ;;                                    :glob "*.org"
  ;;                                    :anchor-regex "\\(:defines\\)\s+[^:\n]+"
  ;;                                    :title-function 'blk-value-after-space-upto-colon
  ;;                                    :extract-id-function 'blk-org-id-at-point)))
  ;; (setq blk-patterns blk-emacs-patterns)
  ;; (setq blk-grepper 'blk-grepper-emacs)

  (add-hook 'text-mode-hook #'blk-enable-completion)

  ;; enable cache for more responsivity
  (setq blk-use-cache t)
  ;; increase the update interval so we dont get frequent lags (should be fixed in the future)
  ;; (setq blk-cache-update-interval 20)
  (setq blk-cache-update-interval 1000000) ;; dont ever update it, i'll update it manually when i need
  (blk-update-cache)

  (with-eval-after-load
   'org-agenda
   (setq org-capture-templates (list))
   (add-to-list 'org-capture-templates
                `("t"
                  "todo"
                  entry
                  (file ,(file-for-blk-id "agenda"))
                  "* TODO %?\nentered on %U\n %i\n %a"))
   (add-to-list 'org-capture-templates
                `("i"
                  "idea"
                  entry
                  (file ,(file-for-blk-id "ideas"))
                  "* IDEA %(my-time-format (current-time)) %?\nentered on %U\n %i\n %a"))
   ;; (add-to-list 'org-capture-templates
   ;;              `("q"
   ;;                "question"
   ;;                entry
   ;;                (file ,(file-for-blk-id "questions"))
   ;;                "* QUESTION %(my-time-format (current-time)) %?\nentered on %U\n %i\n %a"))
   (add-to-list 'org-capture-templates
                `("q"
                  "question"
                  entry
                  (file ,(file-for-blk-id "questions"))
                  "#+begin_question %(my-time-format (current-time))\n\n#+end_question"))
   (add-to-list 'org-capture-templates
                `("f"
                  "feeling"
                  entry
                  (file ,(file-for-blk-id "feelings"))
                  "* FEELING %(my-time-format (current-time)) %?\nentered on %U\n %i\n %a"))
   (add-to-list 'org-capture-templates
                `("o"
                  "thought"
                  entry
                  (file ,(file-for-blk-id "thoughts"))
                  "* THOUGHT %(my-time-format (current-time)) %?\nentered on %U\n %i\n %a"))
   (add-to-list 'org-capture-templates
                `("n"
                  "note"
                  entry
                  (file ,(file-for-blk-id "notes!"))
                  "* NOTE %(my-time-format (current-time)) %?\nentered on %U\n %i\n %a"))
   )

  ;; redefine
  ;; overwrite it to handle attachments inserted by org-transclusion
  (defun org-attach-expand-links (_)
    "Expand links in current buffer.
It is meant to be added to `org-export-before-parsing-hook'."
    (save-excursion
      (while (search-forward "attachment:" nil t)
        (let* ((link (org-element-context))
               ;; (val (get-text-property
               ;;       0
               ;;       'org-transclusion-type
               ;;       (buffer-substring
               ;;        (org-element-begin link)
               ;;        (org-element-end link))))
               (orig-link
                (plist-get
                 (get-text-property
                  0
                  'org-transclusion-orig-keyword
                  (buffer-substring
                   (org-element-begin link)
                   (org-element-end link)))
                 :link))
               (src-file (if (and orig-link (string-prefix-p "[[blk:" orig-link))
                             (plist-get (car (blk-find-by-id (substring orig-link 6 -2))) :filepath)
                           buffer-file-name)))
          (when (and (org-element-type-p link 'link)
                     src-file
                     (string-equal "attachment"
                                   (org-element-property :type link)))
            (let* ((description (and (org-element-contents-begin link)
                                     (buffer-substring-no-properties
                                      (org-element-contents-begin link)
                                      (org-element-contents-end link))))
                   (file (org-element-property :path link))
                   (new-link (org-link-make-string
                              (concat "file:" (with-file-as-current-buffer src-file (org-attach-expand file)))
                              description)))
              (goto-char (org-element-end link))
              (skip-chars-backward " \t")
              (delete-region (org-element-begin link) (point))
              (insert new-link)))))))

  ;; redefine
  ;; redefine it to use `map-org-files' instead of `blk-with-file-as-current-buffer' for faster results
  (defun blk-extract-id (grep-result)
    "open the file and run the :extract-id-function of the grep rule that was matched to
obtain the id"
    (let* ((grep-pattern (plist-get grep-result :matched-pattern))
           (extract-id-func (plist-get grep-pattern :extract-id-function)))
      ;; if :extract-id-function isnt provided, we could try making our own that simply
      ;; returns the "src id" of the target to be linked to, although notice that if this is to
      ;; happen, the file might be later loaded into memory for no reason by `blk-with-file-as-current-buffer'
      (when (not extract-id-func)
        (let ((src-id-func (plist-get (plist-get grep-result :matched-pattern) :src-id-function)))
          (when src-id-func
            (setq extract-id-func
                  (lambda (grep-data-local)
                    ;; `grep-data-local' would be the same as `grep-result' anyway
                    (funcall (plist-get (plist-get grep-result :matched-pattern) :src-id-function) (plist-get grep-data-local :matched-value)))))))
      (if extract-id-func
          (let* ((id (car (map-org-files
                           (plist-get grep-result :filepath)
                           (lambda ()
                             (goto-char (plist-get grep-result :position))
                             (funcall extract-id-func grep-result))))))
            id)
        (progn
          (message "Pattern has no `extract-id-function' or `src-id-function'")
          nil))))

  ;; redefine
  ;; redefine to insert a header with a title when transcluding a file
  (defun blk-org-transclusion-at-point (grep-data)
    "Function that return a DWIM org-transclusion plist.
the plist returned represents an org-transclusion object which is then passed to
org-transclusion to be handled for transclusion in an org buffer."
    (let ((elm (org-element-at-point)))
      (when elm
        (let* ((elm-type (org-element-type elm)))
          (cond
           ;; handler for custom/src org-blocks
           ((or (eq elm-type 'special-block) (eq elm-type 'src-block))
            (list :src-content (buffer-substring (org-element-property :begin elm)
                                                 (org-element-property :end elm))
                  :src-buf (current-buffer)
                  :src-beg (org-element-property :begin elm)
                  :src-end (org-element-property :end elm)))
           ;; handler for latex blocks identified by #+name
           ((eq elm-type 'latex-environment)
            (progn
              (forward-line)
              (blk-tex-transclusion-env-at-point grep-data)))
           ((and (equal elm-type 'keyword)
                 (equal (org-element-property :key elm) "IDENTIFIER"))
            ;; skip over the file keywords
            (save-excursion
              (let ((no-text))
                (while (and (equal (org-element-type (org-element-at-point)) 'keyword)
                            (not no-text))
                  ;; check if we are at the last line
                  (if (eq (line-number-at-pos)
                          (line-number-at-pos (point-max)))
                      (setq no-text t)
                    (forward-line)))
                ;; file contains no text except the keywords, dont transclude
                (when (not no-text)
                  (list :src-content (concat (format "* %s\n" (org-get-title))
                                             (buffer-substring (point)
                                                               (point-max))
                                             )
                        :src-buf (current-buffer)
                        :src-beg (point)
                        :src-end (point-max)))))))))))

  )

(with-eval-after-load-all
 '(blk ox)
 ;; redefine
 ;; resolve links in their original buffers when they're transcluded (by org-transclusion)
 (defun org-export-custom-protocol-maybe (link desc backend &optional info)
   "Try exporting LINK object with a dedicated function.

DESC is its description, as a string, or nil.  BACKEND is the
backend used for export, as a symbol.

Return output as a string, or nil if no protocol handles LINK.

A custom protocol has precedence over regular backend export.
The function ignores links with an implicit type (e.g.,
\"custom-id\")."
   (let* ((type (org-element-property :type link))
          (overlay
           (get-text-property
            0
            'org-transclusion-pair
            (buffer-substring
             (org-element-begin link)
             (org-element-end link))))
          (orig-buffer
           (if overlay
               (overlay-buffer overlay)
             (current-buffer))))
     (with-current-buffer orig-buffer
       (unless (or (member type '("coderef" "custom-id" "fuzzy" "radio" nil))
		   (not backend))
         (let ((protocol (org-link-get-parameter type :export))
	       (path (org-element-property :path link)))
           (and (functionp protocol)
                (condition-case nil
                    (funcall protocol path desc backend info)
                  ;; XXX: The function used (< Org 9.4) to accept only
                  ;; three mandatory arguments.  Type-specific `:export'
                  ;; functions in the wild may not handle current
                  ;; signature.  Provide backward compatibility support
                  ;; for them.
                  (wrong-number-of-arguments
		   (funcall protocol path desc backend))))))))))

;; transclusions (including text from other documents) for org mode, causes problems when inserting ids to blocks that have a name using blk..
(use-package org-transclusion
  :after (org)
  ;; :config
  ;; (add-hook 'org-mode-hook #'org-transclusion-mode)
  ;; (add-to-list 'org-transclusion-after-add-functions 'org-latex-preview)
  )

(defun blk-find-with-consult (text)
  "Find entries defined by patterns in `blk-patterns' using the grepper `blk-grepper'.
Select one and visit it."
  (interactive
   (list (let* ((minibuffer-allow-text-properties t)
                (entries (blk-list-titles))
                (completion-extra-properties
                 '(:annotation-function
                   (lambda (key)
                     (let ((grep-result (get-text-property 0 'grep-data key)))
                       (when (plist-get grep-result :matched-pattern)
                         (propertize
                          (format "\t%s"
                                  (plist-get (plist-get grep-result :matched-pattern)
                                             :title))
                          'face 'font-lock-keyword-face)))))))
           (when entries (consult--read entries :prompt "entry " :history 'blk-hist)))))
  (when text
    (if (get-text-property 0 'grep-data text)
        (let* ((grep-data (get-text-property 0 'grep-data text))
               (filepath (plist-get grep-data :filepath))
               (position (plist-get grep-data :position)))
          (find-file filepath)
          (goto-char position))
      (message "%s not found" text))))

(defun blk-find-with-ivy ()
  (interactive)
  (let ((completing-read-function 'ivy-completing-read)
        (blk-enable-groups t))
    (call-interactively 'blk-find)))

;; i dont think this is useful
(defun my-lob-reload ()
  (interactive)
  (let ((added))
    (map-org-dir-elements
     *notes-dir*
     " :lob"
     'src-block
     (lambda (_)
       (let ((filename (buffer-file-name)))
         (org-babel-lob-ingest filename))))))

(defun construct-blk-hashtable (data)
  (let ((ht (make-hash-table :test 'equal)))
    (mapc
     (lambda (entry)
       (puthash (plist-get entry :id) entry ht))
     data)
    ht))

(defun blk-find-by-id-using-hashtable (id)
  (gethash id blk-hashtable))

(provide 'config-blk)