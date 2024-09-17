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
                                     :anchor-regex "(:defines)\\s+[^:]+"
                                     :title-function 'blk-value-after-space-upto-colon
                                     :extract-id-function 'blk-org-id-at-point))
    )
  (setq blk-patterns blk-rg-patterns)
  ;; (setq blk-grepper blk-grepper-rg)
  ;; (setq blk-grepper blk-grepper-grep)

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

  ;; overwrite it to handle attachments inserted by org-transclusion
  ;; unfinished
  (defun org-attach-expand-links (_)
    "Expand links in current buffer.
It is meant to be added to `org-export-before-parsing-hook'."
    (save-excursion
      (while (re-search-forward "attachment:" nil t)
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
                   (org-element-end link))) :link))
               (src-file (if (and orig-link (string-prefix-p "[[blk:" orig-link))
                             (plist-get (car (blk-find-by-id (substring orig-link 6 -2))) :filepath)
                           buffer-file-name)))
          (when (and (org-element-type-p link 'link)
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
  )

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

(provide 'setup-blk)