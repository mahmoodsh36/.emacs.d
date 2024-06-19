(if (file-exists-p "/home/mahmooz/work/blk/")
    ;; (push "/home/mahmooz/work/blk/" load-path)
    (use-package blk
      :after (org org-transclusion)
      :load-path "/home/mahmooz/work/blk/")
  (use-package blk
    :after (org org-transclusion)
    :ensure ( :host github :repo "mahmoodsheikh36/blk")))

;; transclusions (including text from other documents) for org mode, causes problems when inserting ids to blocks that have a name using blk..
(use-package org-transclusion
  :after (org)
  ;; :config
  ;; (add-hook 'org-mode-hook #'org-transclusion-mode)
  ;; (add-to-list 'org-transclusion-after-add-functions 'org-latex-preview)
  )

(defun blk-find-with-consult ()
  (interactive)
  (let ((completing-read-function 'my-consult-completing-read))
    (call-interactively 'blk-find)))

(defun blk-find-with-ivy ()
  (interactive)
  (let ((completing-read-function 'ivy-completing-read))
    (call-interactively 'blk-find)))

(with-eval-after-load 'blk
  ;; (defvar blk-find-history nil)

  ;; (setq blk-grepper blk-grepper-grep)
  ;; (setq blk-patterns blk-grep-patterns)
  ;; (setq blk-grepper 'blk-grepper-emacs)
  ;; (setq blk-patterns blk-emacs-patterns)

  (setq blk-directories
        (list (from-brain "notes")))

  ;; org-transclusion integration
  (blk-configure-org-transclusion)

  ;; use auctex
  (setq blk-tex-env-at-point-function 'blk-auctex-env-at-point-bounds)

  ;; allow for recursive grep
  ;; (setq blk-search-recursively t)

  ;; enable group search (group things together)
  (setq blk-enable-groups t)

  ;; add the :defines pattern
  ;; (dolist (pattern-table '(blk-rg-patterns blk-grep-patterns))
  ;;   (add-to-list pattern-table (list :title "definition"
  ;;                                    :glob "*.org"
  ;;                                    :anchor-regex "(:defines)\\s+[^:]+"
  ;;                                    :title-function 'blk-value-after-space-upto-colon
  ;;                                    :extract-id-function 'blk-org-id-at-point))
  ;;   ;; (add-to-list pattern-table (list :title "definition"
  ;;   ;;                                  :glob "*.org"
  ;;   ;;                                  :anchor-regex "(:defines)\\s+[^:]+"
  ;;   ;;                                  :title-function 'blk-value-after-space-upto-colon
  ;;   ;;                                  :extract-id-function 'blk-org-id-at-point))
  ;;   )
  ;; (setq blk-patterns blk-rg-patterns)

  ;; add the :defines pattern
  (dolist (pattern-table '(blk-emacs-patterns))
    (add-to-list pattern-table (list :title "definition"
                                     :glob "*.org"
                                     :anchor-regex "\\(:defines\\)\s+[^:\n]+"
                                     :title-function 'blk-value-after-space-upto-colon
                                     :extract-id-function 'blk-org-id-at-point)))
  (setq blk-patterns blk-emacs-patterns)
  (setq blk-grepper 'blk-grepper-emacs)

  (add-hook 'text-mode-hook #'blk-enable-completion)

  ;; enable cache for more responsivity
  (setq blk-use-cache t)
  ;; increase the update interval so we dont get frequent lags (should be fixed in the future)
  ;; (setq blk-cache-update-interval 20)
  (setq blk-cache-update-interval 1000000) ;; dont ever update it, i'll update it manually when i need
  (blk-update-cache)
  )

;; i dont think this is useful
(defun my-lob-reload ()
  (interactive)
  (let ((added))
    (map-org-dir-elements
     *notes-dir*
     "name:"
     'src-block
     (lambda (_)
       (let ((filename (buffer-file-name)))
         (when (not (cl-member filename added :test 'equal))
           (org-babel-lob-ingest filename)
           (push filename added)))))))

;; this is a bad idea, but i copied the function and modified it to work with blk, since there's really no other sane way to go about it (for now atleast..)
(defun org-babel-expand-noweb-references (&optional info parent-buffer)
  "Expand Noweb references in the body of the current source code block.

When optional argument INFO is non-nil, use the block defined by INFO
instead.

The block is assumed to be located in PARENT-BUFFER or current buffer
\(when PARENT-BUFFER is nil).

For example the following reference would be replaced with the
body of the source-code block named `example-block'.

<<example-block>>

Note that any text preceding the <<foo>> construct on a line will
be interposed between the lines of the replacement text.  So for
example if <<foo>> is placed behind a comment, then the entire
replacement text will also be commented.

This function must be called from inside of the buffer containing
the source-code block which holds BODY.

In addition the following syntax can be used to insert the
results of evaluating the source-code block named `example-block'.

<<example-block()>>

Any optional arguments can be passed to example-block by placing
the arguments inside the parenthesis following the convention
defined by `org-babel-lob'.  For example

<<example-block(a=9)>>

would set the value of argument \"a\" equal to \"9\".  Note that
these arguments are not evaluated in the current source-code
block but are passed literally to the \"example-block\"."
  (let* ((parent-buffer (or parent-buffer (current-buffer)))
	 (info (or info (org-babel-get-src-block-info 'no-eval)))
         (lang (nth 0 info))
         (body (nth 1 info))
	 (comment (string= "noweb" (cdr (assq :comments (nth 2 info)))))
         (noweb-prefix (let ((v (assq :noweb-prefix (nth 2 info))))
                         (or (not v)
                             (and (org-not-nil (cdr v))
                                  (not (equal (cdr v) "no"))))))
	 (noweb-re (format "\\(.*?\\)\\(%s\\)"
			   (with-current-buffer parent-buffer
			     (org-babel-noweb-wrap)))))
    (unless (equal (cons parent-buffer
                         (with-current-buffer parent-buffer
                           (buffer-chars-modified-tick)))
                   org-babel-expand-noweb-references--cache-buffer)
      (setq org-babel-expand-noweb-references--cache nil
            org-babel-expand-noweb-references--cache-buffer
            (cons parent-buffer
                  (with-current-buffer parent-buffer
                    (buffer-chars-modified-tick)))))
    (cl-macrolet ((c-wrap
	           (s)
	           ;; Comment string S, according to LANG mode.  Return new
	           ;; string.
	           `(unless org-babel-tangle-uncomment-comments
	              (with-temp-buffer
		        (funcall (org-src-get-lang-mode lang))
		        (comment-region (point)
				        (progn (insert ,s) (point)))
		        (org-trim (buffer-string)))))
	          (expand-body
	           (i)
	           ;; Expand body of code represented by block info I.
	           `(let ((b (if (org-babel-noweb-p (nth 2 ,i) :eval)
			         (org-babel-expand-noweb-references ,i)
		               (nth 1 ,i))))
	              (if (not comment) b
		        (let ((cs (org-babel-tangle-comment-links ,i)))
		          (concat (c-wrap (car cs)) "\n"
			          b "\n"
			          (c-wrap (cadr cs)))))))
	          (expand-references
	           (ref)
	           `(pcase (gethash ,ref org-babel-expand-noweb-references--cache)
	              (`(,last . ,previous)
	               ;; Ignore separator for last block.
	               (let ((strings (list (expand-body last))))
		         (dolist (i previous)
		           (let ((parameters (nth 2 i)))
		             ;; Since we're operating in reverse order, first
		             ;; push separator, then body.
		             (push (or (cdr (assq :noweb-sep parameters)) "\n")
			           strings)
		             (push (expand-body i) strings)))
		         (mapconcat #'identity strings "")))
	              ;; Raise an error about missing reference, or return the
	              ;; empty string.
	              ((guard (or org-babel-noweb-error-all-langs
			          (member lang org-babel-noweb-error-langs)))
	               (error "Cannot resolve %s (see `org-babel-noweb-error-langs')"
		              (org-babel-noweb-wrap ,ref)))
	              (_ ""))))
      (replace-regexp-in-string
       noweb-re
       (lambda (m)
         (with-current-buffer parent-buffer
	   (save-match-data
	     (let* ((prefix (match-string 1 m))
		    (id (match-string 3 m))
		    (evaluate (string-match-p "(.*)" id))
		    (expansion
		     (cond
		      (evaluate
                       (prog1
		           (let ((raw (org-babel-ref-resolve id)))
		             (if (stringp raw) raw (format "%S" raw)))
                         ;; Evaluation can potentially modify the buffer
		         ;; and invalidate the cache: reset it.
                         (unless (equal org-babel-expand-noweb-references--cache-buffer
                                        (cons parent-buffer
                                              (buffer-chars-modified-tick)))
		           (setq org-babel-expand-noweb-references--cache nil
                                 org-babel-expand-noweb-references--cache-buffer
                                 (cons parent-buffer
                                       (with-current-buffer parent-buffer
                                         (buffer-chars-modified-tick)))))))
                      ;; Already cached.
                      ((and (hash-table-p org-babel-expand-noweb-references--cache)
                            (gethash id org-babel-expand-noweb-references--cache))
                       (expand-references id))
		      ;; Return the contents of headlines literally.
		      ((org-babel-ref-goto-headline-id id)
		       (org-babel-ref-headline-body))
		      ;; Look for a source block named SOURCE-NAME.  If
		      ;; found, assume it is unique; do not look after
		      ;; `:noweb-ref' header argument.
		      ((org-with-point-at 1
		         (let ((r (org-babel-named-src-block-regexp-for-name id)))
			   (and (re-search-forward r nil t)
			        (not (org-in-commented-heading-p))
                                (let ((info (org-babel-get-src-block-info t)))
                                  (unless (hash-table-p org-babel-expand-noweb-references--cache)
                                    (setq org-babel-expand-noweb-references--cache (make-hash-table :test #'equal)))
                                  (push info (gethash id  org-babel-expand-noweb-references--cache))
			          (expand-body info))))))
		      ;; Retrieve from the Library of Babel.
		      ((nth 2 (assoc-string id org-babel-library-of-babel)))
                      (let ((result (car (blk-find-by-id id))))
                        (when result
                          (progn
                            (message "hi")
                            (find-file (plist-get result :filepath))
                            (goto-char (plist-get result :position))
                            (let ((info (org-babel-get-src-block-info t)))
                              (expand-body info)))))
		      ;; All Noweb references were cached in a previous
		      ;; run.  Yet, ID is not in cache (see the above
		      ;; condition).  Process missing reference in
		      ;; `expand-references'.
		      ((and (hash-table-p org-babel-expand-noweb-references--cache)
                            (gethash 'buffer-processed org-babel-expand-noweb-references--cache))
		       (expand-references id))
		      ;; Though luck.  We go into the long process of
		      ;; checking each source block and expand those
		      ;; with a matching Noweb reference.  Since we're
		      ;; going to visit all source blocks in the
		      ;; document, cache information about them as well.
		      (t
		       (setq org-babel-expand-noweb-references--cache (make-hash-table :test #'equal))
		       (org-with-wide-buffer
		        (org-babel-map-src-blocks nil
			  (if (org-in-commented-heading-p)
			      (org-forward-heading-same-level nil t)
			    (let* ((info (org-babel-get-src-block-info t))
				   (ref (cdr (assq :noweb-ref (nth 2 info)))))
			      (push info (gethash ref org-babel-expand-noweb-references--cache))))))
                       (puthash 'buffer-processed t org-babel-expand-noweb-references--cache)
		       (expand-references id)))))
	       ;; Interpose PREFIX between every line.
               (if noweb-prefix
		   (mapconcat #'identity
			      (split-string expansion "[\n\r]")
			      (concat "\n" prefix))
                 expansion)))))
       body t t 2))))

(provide 'setup-blk)