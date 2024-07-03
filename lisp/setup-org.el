;; org mode configuration and setup

;; tecosaur's org-mode version
(use-package org
  :ensure ( :remotes ("tecosaur"
                      :repo "https://git.tecosaur.net/tec/org-mode.git"
                      :branch "dev")
            :files (:defaults "etc")))

(use-package org-contrib
  :ensure ( :host github :repo "emacsmirror/org-contrib"))

(defvar *latex-previews-enabled-p*
  (not (is-android-system))
  "whether latex previews for org mode are enabled for the current session")

;; whether to export an org mode file
(setq should-export-org-file-function #'should-export-org-file)

(defun enable-latex-previews ()
  "enable org mode latex previews for current emacs session"
  (interactive)
  (setq *enable-latex-previews* t)
  (add-hook 'org-mode-hook 'org-latex-preview-auto-mode)
  (setq org-startup-with-latex-preview t))

(defun disable-latex-previews ()
  "disable org mode latex previews for current emacs session"
  (interactive)
  (setq *latex-previews-enabled-p* nil)
  (remove-hook 'org-mode-hook 'org-latex-preview-auto-mode)
  (setq org-startup-with-latex-preview nil))

(defun toggle-latex-previews ()
  "toggle org mode latex previews for current emacs session"
  (interactive)
  (if *latex-previews-enabled-p*
      (disable-latex-previews)
    (enable-latex-previews)))

(defun toggle-latex-previews-and-render-current-buffer ()
  "toggle org mode latex previews for current emacs session and render previews in current buffer (if the request is to enable them)"
  (interactive)
  (if *latex-previews-enabled-p*
      (progn
        (org-latex-preview-clear-overlays)
        (org-latex-preview-auto-mode -1)
        (disable-latex-previews))
    (progn
      (org-latex-preview)
      (org-latex-preview-auto-mode 1)
      (enable-latex-previews))))

(when *latex-previews-enabled-p*
  (enable-latex-previews))

;; bibliography file (i use one global one for everything)
(setq org-cite-global-bibliography '("~/brain/bib.bib"))

(defun get-latex-cache-dir-path ()
  "return the path for the directory that contains the compiled pdf latex documents"
  (interactive)
  (from-brain "out/"))

;; compile org docs to pdfs and put them in cache dir
(defun latex-out-file ()
  (concat (file-truename (get-latex-cache-dir-path)) (current-filename-no-ext) ".tex"))
(defun pdf-out-file ()
  (concat (file-truename (get-latex-cache-dir-path)) (current-filename-no-ext) ".pdf"))
(defun my-org-to-pdf ()
  (interactive)
  (let ((outfile (latex-out-file))
        (is-beamer (car (cdar (org-collect-keywords '("latex_class"))))))
    (call-process-shell-command (format "rm %s*%s*" (file-truename (get-latex-cache-dir-path)) (current-filename-no-ext)))
    (if is-beamer
        (org-export-to-file 'beamer outfile
          nil nil nil nil nil nil)
      (org-export-to-file 'latex outfile
        nil nil nil nil nil nil))
    (compile-latex-file outfile)))

(defun compile-latex-file (path)
  ;; for biber we need a different set of commands, for cross-references we need to compile twice
  (let ((cmd (format "%s -shell-escape -output-directory=%s %s"
                     org-latex-compiler
                     (file-truename (get-latex-cache-dir-path))
                     path)))
    (start-process-shell-command
     "latex"
     "latex"
     (format "%s && %s" cmd cmd))))

(defun compile-current-document ()
  "compile the current latex document being edited"
  (interactive)
  (compile-latex-file (buffer-file-name)))

(defun open-current-document ()
  "open the pdf of the current latex document that was generated"
  (interactive)
  (find-file-other-window (concat (get-latex-cache-dir-path) (current-filename-no-ext) ".pdf")))
(defun open-current-document-this-window ()
  (interactive)
  (let ((pdf-file (concat (get-latex-cache-dir-path) (current-filename-no-ext) ".pdf")))
    (if (file-exists-p pdf-file)
        (find-file pdf-file)
      (message "pdf file hasnt been generated"))))

(with-eval-after-load 'org
  (require 'org-attach)
  (require 'ox-beamer)
  ;; save the clock history across sessions
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)
  ;; log state/schedule/deadline changes
  (setq org-log-done 'time)
  (setq org-log-reschedule 'time)
  (setq org-log-redeadline 'time)
  ;; show images when opening a file.
  (setq org-startup-with-inline-images t)
  ;; show images after evaluating code blocks.
  ;; causes an error with (org-babel-ref-resolve "src-rm-quotes") in [[blk:1716750134][blk julia]].
  ;; (add-hook 'org-babel-after-execute-hook (lambda ()
  ;;                                           (clear-image-cache)
  ;;                                           (org-redisplay-inline-images)
  ;;                                           (org-latex-preview)
  ;;                                           ))
  ;; disable prompt when executing code block in org mode
  (setq org-confirm-babel-evaluate nil)
  (setq org-link-elisp-confirm-function nil)
  ;; enable more code block languages for org mode
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (js . t)
     (lisp . t)
     (java . t)
     (latex . t)
     (C . t)
     (shell . t)
     (sql . t)
     (julia . t)
     ;; (mathematica . t)
     ;; (wolfram . t)
     (lua . t)))
  ;; make g++ compile with std=c++17 flag
  (setq org-babel-C++-compiler "g++ -std=c++17")
  ;; make org babel default to python3
  (setq org-babel-python-command "python3")
  ;; increase org table max lines
  (setq org-table-convert-region-max-lines 10000)
  ;; to increase depth of the imenu in treemacs
  (setq org-imenu-depth 4)
  ;; annoying broken links..
  (setq org-export-with-broken-links 'mark)
  ;; dont cache latex preview images
  ;; (setq org-latex-preview-cache 'temp)
  ;; (setq org-element-cache-persistent nil)
  ;; (setq org-element-use-cache nil)
  (setq org-latex-default-packages-alist nil)

  ;; enter insert state after invoking org-capture
  (add-hook 'org-capture-mode-hook 'evil-insert-state)

  ;; allow usage of #+BIND in latex exports
  (setq org-export-allow-bind-keywords t)
  ;; preserve all line breaks when exporting
  (setq org-export-preserve-breaks t)
  ;; indent headings properly
  ;; (add-hook 'org-mode-hook 'org-indent-mode)
  (setq org-todo-keywords
        '("TODO(t!)"
          "GO(g@)";
          "WAIT(w@)"
          "REVIEW(r!)"
          "|" ; remaining entries close tasks
          "DONE(d@)"
          "CANCELED(c@)"
          "MISSED(m@)"
          "CANCELLED(C@)" ;; for backward compatibility
          ))
  ;; filter out entries with tag "ignore"
  (setq org-agenda-tag-filter-preset '("-ignore"))
  ;; use listings package for latex code blocks
  (setq org-latex-src-block-backend 'listings)
  ;; timestamp with seconds
  (setq org-time-stamp-formats '("<%Y-%m-%d %a>" . "<%Y-%m-%d %a %H:%M:%S>"))

  ;; make org-babel java act like other langs
  (setq org-babel-default-header-args:java
        '((:dir . nil)
          (:results . "value")))
  ;; use unique id's to identify headers, better than using names cuz names could change
  (setq org-id-link-to-org-use-id 'use-existing)
  ;; src block indentation / editing / syntax highlighting
  (setq org-src-window-setup 'current-window
        org-src-strip-leading-and-trailing-blank-lines t)
  ;; make org babel use dvisvgm instead of inkscape for pdf->svg, way faster and has many more advtanges over inkscape
  (setq org-babel-latex-pdf-svg-process "dvisvgm --pdf %f -o %O")
  (setq org-babel-latex-preamble (lambda (_) "\\documentclass[preview]{article}"))
  ;; latex syntax highlighting in org mode (and more)
  ;; (setq org-highlight-latex-and-related nil)
  (setq org-highlight-latex-and-related '(latex))
  ;; (setq org-highlight-latex-and-related '(native latex script entities))
  ;; disable org-mode's mathjax because my blog's code uses another version
  (setq org-html-mathjax-template "")
  (setq org-html-mathjax-options '())
  (setq org-babel-default-header-args:latex
        '((:results . "file graphics")
          (:exports . "results")
          ;; (:fit . t)
          ;; (:imagemagick . t)
          ;; (:eval . "no-export")
          (:headers . ("\\usepackage{\\string~/.emacs.d/common}"))
          ))
  ;; make org export deeply nested headlines as headlines still
  (setq org-export-headline-levels 20)
  ;; workaround to make yasnippet expand after dollar sign in org mode
  (add-hook 'org-mode-hook (lambda () (modify-syntax-entry ?$ "_" org-mode-syntax-table)))
  ;; also treat ' as a separator or whatever
  (add-hook 'org-mode-hook (lambda () (modify-syntax-entry ?' "_" org-mode-syntax-table)))
  ;; startup with headlines and blocks folded or not
  (setq org-startup-folded 'showall)
  ;; org-hide-block-startup t)
  ;; try to get the width from an #+ATTR.* keyword and fall back on the original width if none is found.
  (setq org-image-actual-width nil)
  ;; dont center images/tables in latex
  ;; (setq org-latex-images-centered nil)
  ;; (setq org-latex-tables-centered nil)
  ;; get rid of background colors of block lines bleeding all over folded headlines
  (setq org-fontify-whole-block-delimiter-line nil)
  (setq org-fold-catch-invisible-edits 'smart
        org-agenda-span 20)
  ;; stop org mode from moving tags far after headers
  (setq org-tags-column 0)
  ;; inherit attach folders
  (setq org-attach-use-inheritance t)
  ;; use html5 for org exports
  (setq org-html-html5-fancy t)

  ;; stop showing deadlines in today
  (setq org-deadline-warning-days 0)
  ;; remove done items
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-timestamp-if-done t)
  ;; show only the first occurrence of a recurring task
  (setq org-agenda-show-future-repeats 'next)
  ;; make org-open-at-point open link in the same buffer
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)
  ;; enable latex previews everywhere possible and use a custom preamble
  (setq org-latex-preview-live '(block inline edit-special))
  ;; for previews use private.sty
  ;; (setq org-latex-preview-preamble "\\documentclass[ignorerest=true,varwidth=true,float=true,crop=true,preview=true,multi=true]{standalone}[PACKAGES]\\usepackage{\\string~/.emacs.d/private}")
  (setq org-latex-preview-preamble "\\documentclass{standalone}[PACKAGES]\\usepackage{\\string~/.emacs.d/private}")
  (setq org-latex-packages-alist (list "\\usepackage{\\string~/.emacs.d/common}")) ;; use my ~/.emacs.d/common.sty
  ;; export to html using dvisvgm
  (setq org-html-with-latex 'dvisvgm)
  ;; dont export headlines with tags
  (setq org-export-with-tags nil)
  ;; allow characters as list modifiers in org mode
  (setq org-list-allow-alphabetical t)
  ;; also number equations
  (setq org-latex-preview-numbered t)
  ;; ;; tell org latex previews to use lualatex, its better (i need it for some tikz functionalities)
  (setq org-latex-compiler "lualatex")
  ;; make org-agenda open up in the current window
  (setq org-agenda-window-setup 'current-window)
  ;; dont prompt for downloading remote files on export
  (setq org-resource-download-policy nil)
  ;; enable eval: keyword in local variables
  ;; (setq enable-local-eval t)
  ;; dont number headers on exports
  (setq org-export-with-section-numbers nil)
  (setq org-use-property-inheritance t)
  ;; dont override my labels
  (setq org-latex-prefer-user-labels t)
  (setq org-html-prefer-user-labels t)
  ;; dont let org handle subscripts lol
  (setq org-export-with-sub-superscripts nil)
  ;; why truncate lines?
  (setq org-startup-truncated nil)
  (require 'ox-html)
  ;; set to 1.0 to avoid some images being cut off, although that still happens, but less often
  ;; (plist-put org-html-latex-image-options :page-width nil)
  ;; (plist-put org-latex-preview-appearance-options :page-width 0.8)
  ;; lower the debounce value
  ;; (setq org-latex-preview-live-debounce 0.25)
  ;; display inline tramp images in org mode (and other remote image links)
  (setq org-display-remote-inline-images t)
  ;; display full text of links
  ;; (setq org-link-descriptive nil)
  ;; (setq org-pretty-entities t)
  (setq org-ellipsis "⤵")

  ;; make org not evaluate code blocks on exporting
  ;; (add-to-list 'org-babel-default-header-args '(:eval . "no-export"))
  ;; (add-to-list 'org-babel-default-inline-header-args '(:eval . "no-export"))
  (setq org-babel-default-header-args '((:exports . "both")
                                        (:eval . "no-export")
                                        (:session . "none")
                                        (:results . "replace")
                                        (:cache . "no")
                                        (:noweb . "no")
                                        (:hlines . "no")
                                        (:tangle . "no")))
  (setq org-babel-default-inline-header-args '(((:exports . "both")
                                                (:eval . "no-export")
                                                (:session . "none")
                                                (:results . "replace")
                                                (:hlines . "yes"))))

  ;; also make org-special-edit respect tree-sitter modes
  (dolist (mapping major-mode-remap-alist)
    (let ((lang-name (car (split-string (symbol-name (car mapping)) "\\-"))))
      (add-to-list 'org-src-lang-modes (cons lang-name (concat lang-name "-ts")))))

  (setq ;; org-agenda-time-grid
        ;; org-agenda-include-diary t
        org-agenda-compact-blocks t
        org-agenda-start-with-log-mode t)

  (setq org-agenda-custom-commands
        '(("c" "my custom agenda"
           ((org-ql-block
             '(or (and (not (done))
                       (or (habit)
                           (deadline auto)
                           (scheduled :to today)
                           (ts-active :on today)))
                  (closed :on today))
             ;; :sort '(todo priority date)
             )))
          ))

  ;; advice to only render links to files that fit the criterion defined by 'should-export-org-file' so as to not generate links to pages that dont exist
  (defun my-org-link-advice (fn link desc info)
    "when exporting a file, it may contain links to other org files via id's, if a file being exported links to a note that is not tagged 'public', dont transcode the link to that note, just insert its description 'desc'. also we need to handle links to static files, copy those over to the html dir and link to them properly."
    (let* ((link-path (org-element-property :path link))
           (link-type (org-element-property :type link))
           (denote-filepath (pcase link-type
                              ("blk" (plist-get (car (blk-find-by-id link-path)) :filepath))
                              ("denote" (denote-get-path-by-id link-path))
                              (_ nil))))
      (if denote-filepath ;; if indeed a blk/denote link
          (if (funcall should-export-org-file-function denote-filepath)
              (let ((blk-result (car (blk-find-by-id link-path))))
                (if blk-result
                    (let* ((blk-filepath (plist-get blk-result :filepath))
                           (html-filename (file-name-nondirectory (org-file-html-out-file blk-filepath)))
                           (html-link (format "<a href=\"%s%s#%s\">%s</a>"
                                              *html-static-route*
                                              html-filename
                                              link-path
                                              (or desc link-path))))
                      html-link)
                  (format "%s" (or desc link-path))))
            (format "%s" (or desc link-path)))
        (if (equal link-type "file") ;; if its a link to a static file
            (let* ((filename (file-name-nondirectory link-path))
                   (ext (file-name-extension filename)))
              (message "copying linked static file %s" filename)
              (condition-case nil
                  (copy-file link-path (join-path *static-html-dir* filename) t)
                (error (message "failed to copy file %s" filename)))
              (if (cl-member ext (list "png" "jpg" "jpeg" "webp" "svg" "gif") :test #'equal)
                  (format "<img src=\"%s%s\" />"
                          *html-static-route*
                          filename)
                (format "<a href=\"%s%s\">%s</a>"
                          *html-static-route*
                          filename
                          (or desc link-path))))
          (funcall fn link desc info)))))
  (advice-add #'org-html-link :around #'my-org-link-advice)

  (defun my-org-replace-citations (&optional export-backend)
    "blocks whose last line is a citation, remove that citation to the block's :source keyword"
    (interactive)
    (message "got export-backend %s" export-backend)
    (let ((position 1)
          (citations (org-element-map (org-element-parse-buffer) 'citation 'identity)))
      (while citations
        (let* ((citation (car citations))
               (citation-start (1- (+ position (org-element-begin citation))))
               (citation-end (1- (+ position (org-element-end citation))))
               (parent (progn
                         (save-excursion
                           (goto-char citation-end)
                           (when (not (equal (point-max) (point)))
                             (forward-char)
                             (org-element-at-point)))))
               (citation-contents (buffer-substring-no-properties citation-start
                                                                  citation-end))
               ;; if we dont place the double quotes org fails to parse the citation properly in the header
               (to-insert (format " :source \"%s\"" citation-contents)))
          (when parent
            (let* ((parent-end (org-element-end parent))
                   (parent-start (org-element-begin parent))
                   (position parent-end))
              (when (and (equal (org-element-type parent) 'special-block)
                         (is-point-at-some-bol citation-start))
                (goto-char parent-start)
                (when (string-match-p "#\\+name:" (thing-at-point 'line 'no-properties)) ;; if we are at #+name, we need to move forward one line
                  (forward-line))
                (end-of-line)
                (insert to-insert)
                (setq citation-start (+ citation-start (length to-insert)))
                (setq citation-end (+ citation-end (length to-insert)))
                (kill-region citation-start citation-end))))
          (setq position citation-end))
        (if (<= position (buffer-size))
            (let ((original-buffer-substring
                   (buffer-substring position
                                     (point-max))))
              (setq citations (with-temp-buffer
                                (insert original-buffer-substring)
                                (org-element-map (org-element-parse-buffer) 'citation 'identity))))
          (setq citations nil)))))
  (add-to-list 'org-export-before-processing-functions 'my-org-replace-citations)

  ;; remove the title that org inserts into exports by default
  (defun my-org-html--build-meta-info-hook (out)
    (let ((inhibit-message t))
      (replace-regexp-in-string "<title>.*?</title>" "" out)))
  (advice-add #'org-html--build-meta-info :filter-return #'my-org-html--build-meta-info-hook)

  (defvar special-blocks-not-for-handling
    (list "dummy"
          "literal"))

  ;; export some blocks with class=math-block so they get styled accordingly
  (defun my-org-export-read-attribute-hook (fn attribute element &optional property)
    (when (equal attribute :attr_html)
      (let ((block-type (org-element-property :type element))
            (block-title (my-block-title element))
            (citation (get-block-source element)))
        (if (or (not (equal (org-element-type element) 'special-block))
                (cl-member block-type special-blocks-not-for-handling :test #'equal))
            (funcall fn attribute element property)
          (list :class (format "math-block %s" block-type)
                :data-before (concat (pcase block-type
                                       ("my_example" "example")
                                       ("my_comment" "comment")
                                       (_ block-type))
                                     (if (string-empty-p block-title)
                                         ""
                                       (concat ": " block-title)))
                :data-after citation)))))
  (advice-add #'org-export-read-attribute :around #'my-org-export-read-attribute-hook)

  ;; handle some custom blocks i've defined
  (defun my-org-latex-special-block-advice (fn special-block contents info)
    (let ((block-type (org-element-property :type special-block)))
      (if (member block-type special-blocks-not-for-handling)
          (funcall fn special-block contents info)
        (progn
            (setq block-type (pcase block-type
                               ("my_example" "example")
                               ("my_comment" "comment")
                               (_ block-type)))
            (let ((title (my-block-title special-block))
                  (dependency (org-block-property :on special-block))
                  (citation (get-block-source special-block))
                  (label (org-block-property :name special-block)))
              (when (not label)
                (setq label (generate-random-string 7)))
              (if dependency
                  (progn
                    (when (not (string-search "[" dependency)) ;; if its a link without the brackets
                      (setq dependency (format "\\(\\to\\) [[%s]]" dependency)))

                    (setq title (format "%s %s" title (org-export-string-as dependency 'latex t))))
                (when (org-block-property :on-prev special-block)
                  (setq title (format "%s \\(\\to\\) %s" title "previous block"))))
              (concat (format "\\begin{myenv}{%s}{%s}[%s]%s\n" block-type label title ;; note that title can be broken into multiple lines with \\ which may also allow for multiple titles i guess
                              (if (string-empty-p citation) "" (format "[%s]" citation)))
                      contents
                      (format "\\end{myenv}")))))))
  (advice-add #'org-latex-special-block :around #'my-org-latex-special-block-advice)

  ;; enforce some default keywords for all org buffers (in a hacky way)
  (defun my-org-collect-keywords-advice (orig-func &rest args)
    (let ((old-buffer (current-buffer)))
      (with-temp-buffer
        (insert-buffer-substring old-buffer)
        (insert "\n#+setupfile: ~/.emacs.d/setup.org\n#+include: ~/brain/private.org\n#+setupfile: ~/brain/private.org\n")
        (apply orig-func args))))
  (advice-add #'org-collect-keywords :around #'my-org-collect-keywords-advice)

  ;; temporary workaround for captions breaking latex export
  ;; (advice-add 'org-export-get-caption :filter-return (lambda (_) nil))

  )

;; insert a blank line between everything in exports
(defun my-export-newlines (_)
  (let ((inhibit-message t))
    (goto-char (point-min))
    ;; (replace-regexp "\\(^[^#: \\\\]\\{1\\}.*\\)\n" "\\1.\n\n") ;; newline after all lines that dont start with # or \, this is impractical as it affects code blocks
    (replace-regexp "\\(^[^#: \\\\]\\{1\\}.*\\)\\.\n" "\\1.\n\n") ;; insert newlines after lines ending with dot
    (replace-regexp "\\(^[^#: \\\\]\\{1\\}.*\\)\\.)\n" "\\1.\n\n") ;; insert newlines after lines ending with ".)"
    ;; (goto-char (point-min))
    ;; (replace-regexp "\\(^[^#: \\\\]\\{1\\}.*\\):\n" "\\1:\n\n") ;; insert newlines after lines ending with colon
    ))
;; (with-eval-after-load 'org
;;   (add-to-list 'org-export-before-processing-functions 'my-export-newlines))

;; dont insert \\usepackage[inkscapelatex=false]{svg} when exporting docs with svg's, i do that myself
(defun ox-latex-disable-svg-handling ()
  (interactive)
  (setf (org-export-backend-feature-implementations (org-export-get-backend 'latex))
        (cl-remove-if (lambda (entry)
                        (equal (car entry) 'svg))
                      (org-export-backend-feature-implementations (org-export-get-backend 'latex)))))

(defun org-babel-fold-all-latex-src-blocks ()
  "toggle visibility of org-babel latex src blocks"
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    ;; (re-search-forward org-babel-src-block-regexp)
    (ignore-errors (org-babel-next-src-block))
    (let ((old-point nil)
          (current-point (point)))
      (while (not (eq old-point current-point))
        (progn
          (if (string= (org-element-property :language (org-element-at-point)) "latex")
              (org-cycle))
          (ignore-errors (org-babel-next-src-block))
          (setf old-point current-point)
          (setf current-point (point))))))
  (org-content))
;; (add-hook 'org-mode-hook 'org-babel-fold-all-latex-src-blocks)

(defun org-fold-all-answer-blocks ()
  "toggle visibility of answer special blocks, i.e. #+begin_answer"
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (let ((old-point nil)
          (current-point (point)))
      (while (not (eq old-point current-point))
        (progn
          (if (string= (org-element-property :type (org-element-at-point)) "answer")
              (org-cycle))
          (org-next-block 1)
          (setf old-point current-point)
          (setf current-point (point)))))))
;; (add-hook 'org-mode-hook 'org-fold-all-answer-blocks)

(defun org-current-headline-name ()
  "get the name of the current headline"
  (interactive)
  (save-excursion
    (org-previous-visible-heading 1)
    (org-element-property :raw-value (org-element-at-point))))

(defun org-parent-headline-name ()
  "get the name of the parent headline"
  (interactive)
  (save-excursion
    (org-up-heading-safe)
    (org-element-property :raw-value (org-element-at-point))))

(defun open-todays-file ()
  "open todays org file"
  (interactive)
  (let ((todays-file (format-time-string (from-brain "/daily/%Y-%m-%d.org"))))
    (if (not (file-exists-p todays-file))
        (progn
          (find-file todays-file)
          (end-of-buffer)
          (insert (format-time-string "#+filetags: :daily:\n#+title: %Y-%m-%d"))))
    (find-file todays-file)))

(defun today-entry (&optional todo-keyword)
  "insert an entry for today, an action/todo/whatever and clock in"
  (interactive)
  (open-todays-file)
  (org-insert-heading-respect-content)
  (when todo-keyword
    (insert todo-keyword)
    (insert " "))
  (org-insert-time-stamp (current-time) t)
  (org-clock-in)
  (org-end-of-subtree)
  (next-line)
  (evil-insert 0))

(defun today-entry-text-simple ()
  "insert a heading with a timestamp for simple journalling"
  (interactive)
  (open-todays-file)
  (org-insert-heading-respect-content)
  (org-insert-time-stamp (current-time) t)
  (insert " ")
  (evil-insert 0))

;; org mode navigation map
(defvar org-nav-map
  (let ((map (make-sparse-keymap)))
    (pcase-dolist (`(,k . ,f)
                   '(("c" . org-babel-next-src-block)
                     ("C" . org-babel-previous-src-block)
                     ("h" . org-next-visible-heading)
                     ("H" . org-previous-visible-heading)
                     ("o" . org-next-block)
                     ("O" . org-previous-block)
                     ("i" . org-next-item)
                     ("I" . org-previous-item)
                     ("l" . org-next-link)
                     ("L" . org-previous-link)
                     ("e" . org-forward-element)
                     ("E" . org-backward-element)
                     ("s" . scroll-up-command)
                     ("S" . scroll-down-command)))
      (define-key map (kbd k) f))
    map))
(with-eval-after-load 'org
  (map-keymap
   (lambda (_ cmd)
     (put cmd 'repeat-map 'org-nav-map))
   org-nav-map)
  (define-key org-mode-map (kbd "C-l") org-nav-map))

;; set org-mode date's export according to file creation date
(defun file-modif-time (filepath)
  "the time the file was last modified"
  (let ((atr (file-attributes filepath)))
    (file-attribute-modification-time atr)))
;; (defun file-status-change-time (filepath)
;;   (let ((atr (file-attributes filepath)))
;;     (file-attribute-status-change-time atr)))
(defun file-creation-time (filepath)
  "get file creation timestamp, only works on ext4 (and other fs's that support 'crtime'),"
  (string-to-number
   (shell-command-to-string (format "stat --format='%%W' '%s'" filepath))))
(defun file-creation-time-using-git (filepath)
  "get the most distant timestamp in the git repo for the file modification/creation"
  (string-to-number
   (shell-command-to-string
    (format
     "~/work/scripts/git_creation_date.sh \"%s\""
     filepath))))
(defun my-org-date-advice (fn info &optional fmt)
  (let ((myfile (plist-get info :input-file)))
    (format-time-string "<%Y-%m-%d>" (file-modif-time myfile))))
    ;;(format-time-string "<%Y-%m-%d>" (file-creation-time myfile))))
    ;; (format-time-string "<%Y-%m-%d>"
    ;;                     (file-creation-time-using-git myfile))))
(advice-add #'org-export-get-date :around #'my-org-date-advice)

;; org-special-edit with lsp?, laggy af
;; (defun org-babel-edit-prep:python (babel-info)
;;   (setq-local buffer-file-name (->> babel-info caddr (alist-get :tangle)))
;;   (lsp))

(defun org-files-with-tag (tag)
  (grep-org-dir-get-files *notes-dir* (format "#\\+filetags:.*%s.*" tag)))
(defun my-org-agenda-files ()
  (org-files-with-tag "todo"))

;; https://github.com/alphapapa/org-ql/blob/master/examples.org
;; https://github.com/alphapapa/org-super-agenda
;; https://github.com/alphapapa/org-super-agenda/blob/master/examples.org
(defun my-org-agenda ()
  (interactive)
  (setq org-agenda-files (my-org-agenda-files))

  (let ((org-super-agenda-groups
         '(;; Each group has an implicit boolean OR operator between its selectors.
           (:name "Today"
                  :time-grid t
                  :date today
                  :todo "TODAY"
                  :scheduled today
                  :order 1)
           (:priority<= "B"
                        ;; Show this section after "Today" and "Important", because
                        ;; their order is unspecified, defaulting to 0. Sections
                        ;; are displayed lowest-number-first.
                        :order 1)
           ;; After the last group, the agenda will display items that didn't
           ;; match any of these groups, with the default order position of 99
           )))
    (org-agenda nil "a")))

(defmacro with-file-as-current-buffer (file &rest body)
  (let ((present-buffer (gensym))
        (result (gensym)))
    `(let ((,present-buffer (find-buffer-visiting ,file)))
       (save-excursion
         (with-current-buffer (or (find-buffer-visiting ,file) (find-file-noselect ,file))
           (setq ,result (progn ,@body))
           (when (not ,present-buffer)
             (kill-buffer (current-buffer)))
           ,result)))))

(defun export-org-file (file &rest kw)
  "export a node's file to both html and pdf, if pdf-p is true, export to pdf, if html-p is true, export to html"
  (with-file-as-current-buffer
   file
   (when (plist-get kw :pdf-p)
     (my-org-to-pdf))
   (when (plist-get kw :html-p)
     (my-org-to-html))))

(defun all-org-files ()
  "return all known org files"
  (directory-files (from-brain "notes/") t ".*\\.org$"))

(defun files-linked-from-org-file (filepath)
  (with-file-as-current-buffer
   filepath
   (remove
    nil
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (mylink)
        (let ((filepath (pcase (org-element-property :type mylink)
                          ("blk" (plist-get (car (blk-find-by-id (org-element-property :path mylink)))
                                            :filepath))
                          ("denote" (denote-get-path-by-id (org-element-property :path mylink)))
                          (_ nil))))
          filepath))))))

(defun export-node-recursively (node exceptions &rest kw)
  "export node, export all nodes/files it links to, and all files linked from those and so on, basically we're exporting the connected subgraph the node exists in, `exceptions' is used for recursion to keep a record of exported nodes"
  (if (and node (not (cl-find node exceptions :test #'string=)))
      (progn
        (push node exceptions)
        (when (and node (funcall should-export-org-file-function node))
          (message (format "exporting: %s" node))
          (condition-case nil
              (apply #'export-org-file node kw)
            (error (message "failed to export %s" node)))
          (let ((nodes (files-linked-from-org-file node)))
            (dolist (other-node nodes)
              (when (funcall should-export-org-file-function other-node) ;; to avoid jumping to nodes that arent for exporting anyway
                (when other-node (message (format "exporter jumping to: %s" other-node)))
                (setf exceptions (apply #'export-node-recursively (nconc (list other-node exceptions) kw)))))))
        exceptions)
    exceptions))

(defun export-all-org-files (&rest kw)
  "export all org mode files using `export-org-file', use `should-export-org-file-function' to check whether a file should be exported"
  (let ((exceptions))
    (let ((files-to-export (list-org-files-to-export))
          ;; i need my transclusions present when exporting
          (org-mode-hook (cons 'org-transclusion-mode org-mode-hook)))
      (dolist (file files-to-export)
        (setq exceptions (apply #'export-node-recursively (nconc (list file exceptions) kw)))
        (setq exceptions (push file exceptions))))))

(defun list-org-files-to-export ()
  (list-note-files))

(defun export-all-org-files-to-html-and-pdf ()
  (interactive)
  (let ((should-export-org-file-function #'should-export-org-file))
    (export-all-org-files :html-p t :pdf-p t)))

(defun export-all-org-files-to-html ()
  (interactive)
  (map-org-dir-elements *notes-dir* ":forexport:" 'headline
                        (lambda (_) (org-export-heading-html)))
  (let ((should-export-org-file-function #'should-export-org-file))
    (export-all-org-files :html-p t)))

(defun export-all-math-org-files-to-html ()
  (interactive)
  (let ((should-export-org-file-function
         (lambda (orgfile)
           (with-file-as-current-buffer orgfile
            (or (cl-find "math" (org-get-tags) :test 'equal)
                (cl-find "cs" (org-get-tags) :test 'equal)))))
        (*static-html-dir* (expand-file-name "~/work/test/")))
    (map-org-dir-elements *notes-dir* ":forexport:" 'headline
                          (lambda (_) (org-export-heading-html)))
    (export-all-org-files :html-p t)))

(defun export-current-buffer (&rest kw)
  "gets the node associated with the current buffer, exports it"
  (interactive)
  (apply #'export-org-file buffer-file-name kw))

(defun org-get-keyword (kw)
  (cadar (org-collect-keywords (list kw))))
(defun should-export-org-file (file)
  (org-export-dir-name file)) ;; why am i returning dir name?
(defun org-export-dir-name (file)
  "whether the current org buffer should be exported"
  (car
   (cdar
    (with-file-as-current-buffer
     file
     (org-collect-keywords '("export_section"))))))
(defun org-file-grab-time (orgfile)
  (timestamp-to-time (string-to-number (file-name-nondirectory orgfile))))
(defun org-file-grab-keyword (orgfile kw)
  (with-file-as-current-buffer
   orgfile
   (org-get-keyword kw)))

(defun list-section-export-candidates (section)
  (let* ((grep-results (grep-org-dir *notes-dir* (format "#\\+export_section: %s" section)))
         (files-to-export (mapcar (lambda (result) (plist-get result :filepath)) grep-results)))
    files-to-export))

(defun sort-org-files-by-time (candidate-files)
  (cl-sort
   candidate-files
   (lambda (file1 file2)
     (not (time-less-p (org-file-grab-time file1)
                       (org-file-grab-time file2))))))

(defun map-org-dir-elements (dir regex elm-type fn)
  "look for lines containing `regex' that contain an org element of type `elm-type', run `fn' at the point where the element is"
  (interactive)
  (let ((grep-results (grep-org-dir dir regex)))
    (dolist (result grep-results)
      (let ((file (plist-get result :filepath))
            (position (plist-get result :position)))
        (with-file-as-current-buffer
         file
         (goto-char position)
         (let ((elm (org-element-at-point)))
           (when (eq (org-element-type elm) elm-type)
             (funcall fn elm))))))))

(defun notes-execute-marked-src-block (rgx)
  (map-org-dir-elements *notes-dir* rgx 'src-block
                        (lambda (_)
                          (message "running code block in file %s" (buffer-file-name))
                          (org-ctrl-c-ctrl-c))))

(defun grep-org-dir (dir regex)
  (blk-grep blk-grepper
            (list (list :anchor-regex regex :src-id-function 'identity :glob "*.org"))
            (list dir)))
(defun grep-org-dir-get-files (dir regex)
  (mapcar (lambda (entry) (plist-get entry :filepath))
          (grep-org-dir dir regex)))

(defun list-note-files ()
  (append (directory-files *notes-dir* t ".*\\.org")
          (directory-files (from-brain "daily") t ".*\\.org")))

(defun my-org-ql-agenda ()
  (interactive)
  (org-ql-search (my-org-agenda-files)
    '(or (and (not (done))
             (or (habit)
                 (deadline auto)
                 (scheduled :to today)
                 (ts-active :on today)))
         (closed :on today)
         (ts :from -7 :to today)
         (deadline :from -60 :to +60)
         (and (todo) (not (done))))
    ;; :sort '(todo priority date)
    :super-groups '((:name "habits" :habit)
                    (:name "today" :time-grid t)
                    (:name "college" :tag "college1")
                    (:deadline t :name "deadlines")
                    (:auto-planning t)
                    (:todo t :name "other"))))

(defun org-block-property (property block)
  "example: #+begin_myblock :myproperty whatever-value"
  (let ((result
         (or (org-element-property property block)
             ;; use format because sometimes org-babel-parse-header-arguments parses links as a vector, not as a string
             (alist-get
              property
              (org-babel-parse-header-arguments
               (org-element-property :parameters block)))
             (member property
                     (mapcar 'car (org-babel-parse-header-arguments
                                   (org-element-property :parameters block)))))))
    (when result
      (format "%s" result))))

(defun my-block-title (block)
  (or (org-block-property :defines block)
      (org-block-property :title block)
      ""))

(defun html-out-file (title)
  (let ((inhibit-message t))
    (join-path *static-html-dir*
               (format "%s.html"
                       (or
                        (replace-regexp-in-string " \\|/" "_" title)
                        (current-filename-no-ext))))))

(defun org-file-html-out-file (org-filepath)
  (with-file-as-current-buffer
   org-filepath
   (html-out-file (org-get-title))))

(defun my-org-to-html (&optional heading)
  (interactive)
  ;; so that org mode places the latex previews in the specified dir
  (plist-put org-html-latex-image-options :image-dir "ltx")
  (plist-put org-html-latex-image-options :page-width nil)

  ;; disable some stuff that is enabled by default in html exporting
  (let* ((title (if heading (car (last (org-get-outline-path t))) (org-get-title)))
         (desc (when (not heading) (org-get-keyword "description")))
         (outfile (html-out-file title))
         (org-export-with-title nil)
         (org-html-postamble nil)
         (org-html-head-include-default-style nil)
         (org-html-head (concat
                         (format "<title>%s</title>" title)
                         (with-temp-buffer
                          (insert-file-contents (from-emacsd "head.html"))
                          (buffer-string))))
         (my-preamble
          (concat
           (with-temp-buffer
             (insert-file-contents (from-emacsd "preamble.html"))
             (buffer-string))
           (if (or (not heading)
                   (and heading (not (cl-member "notitle" (org-get-tags) :test 'equal))))
               (format "<h1 class=\"main-title\">%s</h1>%s"
                       title
                       (if desc (format "<span class=\"desc\">%s</span>" desc) ""))
             "")))
         (org-html-preamble-format (list (list "en" my-preamble))))
    (when heading
      (message "got %s %s" (and heading (not (cl-member "notitle" (org-get-tags) :test 'equal))) title))
    (message "writing to %s" outfile)
    (when heading
      (org-narrow-to-subtree))
    (org-export-to-file 'html outfile
      nil nil nil nil nil nil)
    (when heading
      (widen))
    (copy-file (from-emacsd "main.css") *static-html-dir* t)
    (copy-file (from-emacsd "darkmode.js") *static-html-dir* t)
    (copy-directory "ltx" *static-html-dir* t)))

(defun org-remove-forexport-headlines (backend)
  "Remove headlines with :forexport: tag."
  (org-map-entries (lambda () (delete-region (pos-bol) (pos-eol)))
                   "forexport"))
(defun org-export-heading-html ()
  (interactive)
  ;; temoporarily add org-remove-headlines because otherwise it causes some issues.
  ;; notice that we have to remove the headline otherwise we'd get two titles since
  ;; the exporting function inserts the contents of the headline as a title,
  ;; although the way we're doing it is somewhat hacky..
  (let ((org-export-before-processing-functions
         (cons 'org-remove-forexport-headlines
               org-export-before-processing-functions)))
    (my-org-to-html t)))

(defun get-block-source (block)
  ;; the use of `format` is because org-babel parses [this link] as a vector because it sees
  ;; it that way because of the brackets around it
  (let ((original-citation-str (format "%s" (or (org-block-property :source block) ""))))
    ;; the newline is there (maybe temporarily) that prevents an error that happens otherwise
    (org-export-string-as (concat original-citation-str "\n") 'latex t)))

(defun current-unix-timestamp ()
  (time-to-seconds (current-time)))

(defun my-time-format (mydate)
  (format-time-string (cdr org-time-stamp-formats) mydate))

(defun timestamp-to-time (timestamp)
  (seconds-to-time (truncate timestamp)))

(defun time-to-timestamp (mydate)
  (time-to-seconds mydate))

;; functions to create new files (like org-capture)
(defun new-note-file ()
  (interactive)
  (let ((my-timestamp (current-unix-timestamp)))
    (find-file (from-notes (format "%s.org" my-timestamp)))
    (yas-expand-snippet
     (format "#+title: $1\n#+filetags: $2\n#+date: %s\n#+identifier: %s\n$0"
             (my-time-format (timestamp-to-time my-timestamp)) my-timestamp))
    (evil-insert 0)))
(defun new-book-note-file ()
  (interactive)
  (let ((my-timestamp (current-unix-timestamp)))
    (find-file (from-notes (format "%s.org" my-timestamp)))
    (yas-expand-snippet
     (format "#+title: $1\n#+filetags: $2\n#+date: %s\n#+identifier: %s\n#+book_title: \n#+book_author: \n#+book_year: \n#+book_main_file: \n#+book_source_url: \n$0"
             (my-time-format (timestamp-to-time my-timestamp)) my-timestamp))
    (evil-insert 0)))

(defun org-set-keyword (option value)
  "Set or update a file-level OPTION to VALUE after the :PROPERTIES: drawer if it exists, otherwise at the top of the file."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t)) ;; ensure case-insensitive search
      ;; check for the :PROPERTIES: drawer
      (if (and (is-substring (substring-no-properties (buffer-string) 0 9) ":PROPERTIES:")
               (re-search-forward "^:PROPERTIES:" nil t))
          (progn
            (re-search-forward "^:END:" nil t)
            (forward-line)
            ;; insert the option after the :PROPERTIES: drawer
            (if (re-search-forward (concat "^#\\+" option ":") nil t)
                (progn
                  (beginning-of-line)
                  (kill-line)
                  (insert (concat "#+" option ": " value)))
              (insert (concat "#+" option ": " value "\n"))))
        ;; if no :PROPERTIES: drawer, insert at the beginning
        (goto-char (point-min))
        (if (re-search-forward (concat "^#\\+" option ":") nil t)
            (progn
              (beginning-of-line)
              (kill-line)
              (insert (concat "#+" option ": " value)))
          ;; insert at the top after initial comments or blank lines
          (goto-char (point-min))
          (while (or (looking-at "^#") (looking-at "^$"))
            (forward-line))
          (insert (concat "#+" option ": " value "\n")))))))

;; execute some python blocks when a python repl starts
(add-hook 'inferior-python-mode-hook
          (lambda () (notes-execute-marked-src-block (regexp-quote ":python-repl"))))

(defun blk-grep-list-files (regex)
  (mapcar
   (lambda (entry) (plist-get entry :filepath))
   (blk-grep blk-grepper
             (list (list :anchor-regex regex :src-id-function 'identity :glob "*.org"))
             blk-directories)))

(defun list-book-org-files ()
  ;; (org-files-with-tag "book")
  (blk-grep-list-files "book_title:"))

(defun list-books ()
  (let ((book-org-files (list-book-org-files))
        (books))
    (dolist (book-org-file book-org-files)
      (blk-with-file-as-current-buffer
       book-org-file
       (let ((book-title (org-get-keyword "book_title"))
             (file-identifier (org-get-keyword "identifier"))
             (book-year (org-get-keyword "book_year"))
             (book-author (org-get-keyword "book_author"))
             (book-main-file (org-get-keyword "book_main_file")))
         (push (list :title book-title
                     :identifier file-identifier
                     :author book-author
                     :year book-year
                     :file book-main-file)
               books))))
    books))
(defun book-prompt ()
  (interactive)
  (let ((option (completing-read-cons
                 "book"
                 (mapcar
                  (lambda (book)
                    (cons (format "%s - %s - %s"
                                  (plist-get book :title)
                                  (plist-get book :author)
                                  (plist-get book :year))
                          book))
                  (list-books)))))
    (find-file (plist-get (cdr option) :file))))

(provide 'setup-org)