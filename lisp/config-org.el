;; -*- lexical-binding: t -*-
;; org mode configuration and setup

;; tecosaur's org-mode version
(use-package org
  :ensure ( :remotes ("tecosaur"
                      :repo "https://git.tecosaur.net/tec/org-mode.git"
                      :branch "dev"
                      ;; :depth nil ;; full depth clone
                      ;; :pin t
                      ;; :ref "b9637ef14273f35e77ca9efe82fbab178c24993f"
                      )
            :files (:defaults "etc"))
  )

(use-package org-contrib
  :after (org)
  :ensure ( :type git :host sourcehut :repo "bzg/org-contrib"))

;; org-notmuch.el from an old org-contrib
(with-eval-after-load 'org
  (require 'org-notmuch))

(defvar *latex-previews-enabled-p*
  (not (is-android-system))
  "whether latex previews for org mode are enabled for the current session")

;; whether to export an org mode file
(setq should-export-org-file-function #'should-export-org-file)

;; 0 means export only the nodes themselves, i means nodes with a distance of at most 'i' links from each node we have
(defconst export-graph-depth 2)

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

;; bibliography file (i use one global one for everything)
(setq org-cite-global-bibliography (list (from-brain "bib.bib") (from-brain "auto.bib")))

(defun get-latex-cache-dir-path ()
  "return the path for the directory that contains the compiled pdf latex documents"
  (interactive)
  (from-brain "out/"))

;; compile org docs to pdfs and put them in cache dir
(defun latex-out-file ()
  (concat (file-truename (get-latex-cache-dir-path)) (current-filename-no-ext) ".tex"))
(defun pdf-out-file ()
  (concat (file-truename (get-latex-cache-dir-path)) (current-filename-no-ext) ".pdf"))
(cl-defun my-org-to-pdf (&optional (async t))
  (interactive)
  (let ((outfile (latex-out-file))
        (is-beamer (car (cdar (org-collect-keywords '("latex_class")))))
        (org-latex-packages-alist (list "\\usepackage{\\string~/.emacs.d/common}")))
    (clean-latex-files outfile)
    (if is-beamer
        (org-export-to-file 'beamer outfile
          nil nil nil nil nil nil)
      (org-export-to-file 'latex outfile
        nil nil nil nil nil nil))
    (compile-latex-file outfile async)))

(cl-defun compile-latex-file (path &optional (async t))
  ;; for biber we need a different set of commands, for cross-references we need to compile twice
  (let ((cmd (format "%s -shell-escape -output-directory=%s %s"
                     org-latex-compiler
                     (file-truename (get-latex-cache-dir-path))
                     path)))
    (if async
        (start-process-shell-command
         "latex"
         "latex"
         (format "%s && %s" cmd cmd)
         )
      (call-process-shell-command
       cmd))))

(cl-defun clean-latex-files (path)
  (call-process-shell-command
   (format "rm %s*%s*" (file-truename (get-latex-cache-dir-path))
           (file-name-base path))))
(cl-defun clean-latex-files-without-tex-file (path)
  (call-process-shell-command
   (format "find -name '%s*%s*' -not -name '*.tex' -delete"
           (file-truename (get-latex-cache-dir-path))
           (file-name-base path))))

(defun compile-current-document ()
  "compile the current latex document being edited"
  (interactive)
  (clean-latex-files-without-tex-file (buffer-file-name))
  (compile-latex-file (buffer-file-name)))

(defun open-current-document ()
  "open the pdf of the current latex document that was generated"
  (interactive)
  (find-alternate-file-other-window (concat (get-latex-cache-dir-path) (current-filename-no-ext) ".pdf")))
(defun open-current-document-this-window ()
  (interactive)
  (let ((pdf-file (concat (get-latex-cache-dir-path) (current-filename-no-ext) ".pdf")))
    (if (file-exists-p pdf-file)
        (find-file pdf-file)
      (message "pdf file hasnt been generated"))))

;; (with-eval-after-load-all '(org ox org-latex-preview org-agenda ox-latex)
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
     (js . t)
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
  ;; dont export with table of contents unless i want you to
  (setq org-export-with-toc nil)

  ;; enter insert state after invoking org-capture
  (add-hook 'org-capture-mode-hook 'evil-insert-state)

  ;; allow usage of #+BIND in latex exports
  (setq org-export-allow-bind-keywords t)
  ;; preserve all line breaks when exporting
  ;; (setq org-export-preserve-breaks t)
  ;; indent headings properly
  ;; (add-hook 'org-mode-hook 'org-indent-mode)
  (setq org-todo-keywords
        '("TODO(t!)"
          "TOD2(T!)"
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
  ;; (setq org-babel-default-header-args:latex
  ;;       '((:results . "file graphics")
  ;;         (:exports . "results")
  ;;         ;; (:fit . t)
  ;;         ;; (:imagemagick . t)
  ;;         ;; (:eval . "no-export")
  ;;         (:headers . ("\\usepackage{\\string~/.emacs.d/common}"))
  ;;         ))
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
  (setq org-latex-packages-alist (list "\\usepackage{\\string~/.emacs.d/common}"  "\\usepackage{\\string~/.emacs.d/private}")) ;; use my ~/.emacs.d/common.sty
  ;; (setq org-latex-preview-preamble "\\documentclass{article}\n\\usepackage{\\string~/.emacs.d/common}\\usepackage{\\string~/.emacs.d/private}")
  ;; export to html using dvisvgm
  (setq org-html-with-latex 'dvisvgm)
  ;; dont export headlines with tags
  (setq org-export-with-tags nil)
  ;; allow characters as list modifiers in org mode
  (setq org-list-allow-alphabetical t)
  ;; dont show equation numbers, they sometimes mess up the preview by causing it to be cut off
  (setq org-latex-preview-numbered nil)
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
  ;; (plist-put org-latex-preview-appearance-options :page-width 0.6)
  ;; lower the debounce value
  ;; (setq org-latex-preview-live-debounce 0.25)
  ;; display inline tramp images in org mode (and other remote image links)
  (setq org-display-remote-inline-images t)
  ;; display full text of links
  ;; (setq org-link-descriptive nil)
  ;; (setq org-pretty-entities t)
  (setq org-ellipsis "⤵")
  ;; so that a state modification date is inserted on a new heading automatically
  (setq org-treat-insert-todo-heading-as-state-change t)
  ;; show evaluation time in hash?
  (setq org-babel-hash-show-time t)

  ;; make org not evaluate code blocks on exporting
  ;; (add-to-list 'org-babel-default-header-args '(:eval . "no-export"))
  ;; (add-to-list 'org-babel-default-inline-header-args '(:eval . "no-export"))
  (setq org-babel-default-header-args
        '((:exports . "both")
          (:eval . "no-export")
          (:session . "none")
          (:results . "replace")
          (:cache . "no")
          (:noweb . "no")
          (:hlines . "no")
          (:tangle . "no")))
  (setq org-babel-default-inline-header-args
        '(((:exports . "results")
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
  (defun my-org-html-link-advice (fn link desc info)
    "when exporting a file, it may contain links to other org files via id's, if a file being exported links to a note that is not tagged 'public', dont transcode the link to that note, just insert its description 'desc'. also we need to handle links to static files, copy those over to the html dir and link to them properly."
    (let* ((link-path (org-element-property :path link))
           (link-type (org-element-property :type link))
           (filepath (pcase link-type
                       ("blk" (plist-get (car (blk-find-by-id link-path)) :filepath))
                       ("denote" (denote-get-path-by-id link-path))
                       (_ nil))))
      (if filepath ;; if indeed a blk/denote link
          (if (funcall should-export-org-file-function filepath)
              (let ((blk-result (car (blk-find-by-id link-path))))
                (if blk-result
                    (let* ((blk-filepath (plist-get blk-result :filepath))
                           (html-filename (file-name-nondirectory (org-file-html-out-file blk-filepath)))
                           (html-link (format "<a href=\"%s%s%s\">%s</a>"
                                              *html-static-route*
                                              html-filename
                                              ;; if its a link to a file, dont append #<anchor>
                                              (if (eq (plist-get (plist-get blk-result :matched-pattern) :shared-name)
                                                      'blk-org-file-rule)
                                                  ""
                                                (format "#%s" link-path))
                                              (or desc link-path))))
                      html-link)
                  (format "%s" (or desc link-path))))
            (format "%s" (or desc link-path)))
        ;; if its a link to a static file
        (if (or (equal link-type "file")
                (equal link-type "xopp-figure"))
            (progn
              (when (equal link-type "xopp-figure")
                (call-process org-xopp-figure-generation-script
                              nil
                              nil
                              nil
                              link-path
                              (org-xopp-temp-file link-path))
                (setq link-path (org-xopp-temp-file link-path)))
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
                  (if (cl-member ext (list "mp4" "mkv") :test #'equal)
                      (format "
<video controls muted>
  <source src='%s%s' type='video/%s'>
your browser does not support the video tag.
</video>"
                              *html-static-route*
                              filename
                              ext)
                    (format "<a href=\"%s%s\">%s</a>"
                            *html-static-route*
                            filename
                            (or desc link-path))))))
          (funcall fn link desc info)))))
  (advice-add #'org-html-link :around #'my-org-html-link-advice)

  ;; handle .xopp files properly
  ;; todo rewrite, this can be done without advising with `org-export-before-processing-functions`
  ;; (defun my-org-latex-link-advice (fn link desc info)
  ;;   (let* ((link-path (org-element-property :path link))
  ;;          (file-basename (file-name-base link-path))
  ;;          (link-type (org-element-property :type link)))
  ;;     (if (equal link-type "xopp-pages")
  ;;         (let ((pdf-filepath (format "/tmp/%s.pdf" file-basename))
  ;;               (shell-command-dont-erase-buffer t))
  ;;           (shell-command
  ;;            (format "xournalpp --create-pdf %s %s"
  ;;                    pdf-filepath
  ;;                    link-path))
  ;;           (format "\\includepdf[pages=-]{%s}" pdf-filepath))
  ;;       (if (equal link-type "xopp-figure")
  ;;           (let ((png-filepath
  ;;                  (s-trim
  ;;                   (shell-command-to-string-no-stderr
  ;;                    (format "generate_xopp_figure.sh '%s'"
  ;;                            link-path)))))
  ;;             (message "im here %s" png-filepath)
  ;;             (format "\\begin{center}\\includegraphics[max width=0.5\\linewidth]{%s}\\end{center}" png-filepath))
  ;;         (funcall fn link desc info)))))
  ;; (advice-add #'org-latex-link :around #'my-org-latex-link-advice)

  (defun my-org-replace-citations (&optional export-backend)
    "blocks whose last line is a citation, remove that citation to the block's :source keyword"
    (interactive)
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
                             (when (string-prefix-p
                                    "#+end_"
                                    (substring-no-properties (thing-at-point 'line)))
                               (org-element-at-point))))))
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
                (delete-region citation-start citation-end))))
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

  (defun my-org-preprocess (&optional export-backend)
    (save-excursion
      (goto-char (point-min))
      (replace-regexp "#\\+caption: \\(.*\\)" "\\1")))
  ;; (add-to-list 'org-export-before-processing-functions 'my-org-preprocess)

  (defmacro org-with-modifications (element-type &rest body)
    "Iterate over Org ELEMENT-TYPE and modify them, adjusting positions automatically.
   Handles both insertions and deletions."
    `(let ((offset 0))
       (org-element-map (org-element-parse-buffer) ,element-type
         (lambda (elm)
           (let* ((begin-pos (org-element-property :begin elm))
                  (modified-pos (+ begin-pos offset))
                  (initial-buffer-size (point-max)))
             (save-excursion
               (goto-char modified-pos)
               ,@body)
             ;; Calculate how much the buffer size has changed (inserted or deleted)
             (let ((buffer-size-delta (- (point-max) initial-buffer-size)))
               (setq offset (+ offset buffer-size-delta))))))))

  ;; remove blocks with :noexport
  (defun my-org-remove-noexport-blocks (&optional export-backend)
    (org-with-modifications
     'special-block
     (when (or (string= (org-block-property :noexport (org-element-at-point))
                        (symbol-name export-backend))
               (equal (org-block-property-1 :noexport (org-element-at-point))
                      '(:noexport)))
       (delete-region (org-element-begin (org-element-at-point))
                      (org-element-end (org-element-at-point))))))
  (add-to-list 'org-export-before-processing-functions 'my-org-remove-noexport-blocks)

  ;; hacky, detects backslash at beginning of line as latex, #+ as keyword/special block/whatever
  ;; insert whitespaces wherever appropriate when exporting
  (defun my-org-hook-insert-whitespaces (export-backend)
    (goto-char (point-min))
    ;; (replace-regexp "\\(\\end{[a-zA-Z0-9]*?}\\)$" "\\1\n") ;; this breaks the latex document when it matches \end{array} inside `equation' environment
    ;; newline after tikzpicture figures
    (let ((message-log-max nil)
          (inhibit-message t))
      (replace-regexp "\\(\\end{tikzpicture}\\)$" "\\1\n"))
    (goto-char (point-min))
    ;; newline after special blocks
    ;; (replace-regexp "\\(#\\+end_.*\\)" "\\1\n")
    ;; newline after sentences
    (goto-char (point-min))
    (let ((prev-line)
          (next-line)
          (this-line)
          (this-element)
          (next-line-begin)
          (stop)
          (inserted))
      (while (not stop)
        (setq this-line (buffer-substring (pos-bol) (pos-eol)))
        (setq this-element (org-element-at-point))
        (setq inserted nil)
        (when (not (point-on-last-line-p))
          (save-excursion
            (setq next-line-begin (1+ (pos-eol)))
            (goto-char next-line-begin)
            (setq next-line (buffer-substring (pos-bol) (pos-eol)))))
        ;; after each sentence
        ;; also if next line is tikzpicture we need to create a line break
        (when (or (and (not (point-on-last-line-p))
                       (string-suffix-p "." this-line) ;; dot to denote end of sentence and new line?
                       (equal (org-element-type (org-element-at-point)) 'paragraph) ;; or maybe it deserves a new line simply if its a text line?
                       (not (string-prefix-p "#+" next-line))
                       (not (string-prefix-p "\\" next-line)))
                  (and (not (point-on-last-line-p))
                       (not (string-prefix-p "#+" this-line))
                       (or (string-prefix-p "\\begin{tikzpicture}" next-line)
                           (string-prefix-p "\\begin{alg}" next-line))))
          (goto-char (pos-eol))
          (unless (get-text-property (point) 'read-only)
            (insert (case export-backend
                      (latex "\\\\")
                      (html "\n"))))
          (setq inserted t))
        ;; after block ends
        (when (and (not inserted)
                   (string-prefix-p "#+end_" this-line))
          (goto-char (pos-eol))
          (unless (get-text-property (point) 'read-only)
            (insert "\n")))
        (setq prev-line this-line)
        (if (point-on-last-line-p)
            (setq stop t)
          (goto-char (1+ (pos-eol)))))))
  (add-to-list 'org-export-before-processing-functions 'my-org-hook-insert-whitespaces)

  ;; remove the title that org inserts into exports by default
  (defun my-org-html--build-meta-info-hook (out)
    (let ((message-log-max nil)
          (inhibit-message t))
      (replace-regexp-in-string "<title>.*?</title>" "" out)))
  (advice-add #'org-html--build-meta-info :filter-return #'my-org-html--build-meta-info-hook)

  (defvar special-blocks-not-for-handling
    (list ;; "dummy"
          "any"
          "part"
          ;; "dummy2"
          "literal"))

  ;; export some blocks with class=fancy-block so they get styled accordingly
  (defun my-org-export-read-attribute-hook (fn attribute element &optional property)
    (when (equal attribute :attr_html)
      (let* ((block-type (org-element-property :type element))
             (block-title (my-block-title element))
             (element-name (org-block-property :name element))
             (citation (org-block-property :source element))
             ;; (caption (org-element-property :caption element))
             (block-type-str
              (pcase block-type
                ("dummy" "")
                ("dummy2" "")
                ("my_example" "example: ")
                ("my_comment" "comment: ")
                (_ (format "%s: " block-type)))))
        (if (or (not (equal (org-element-type element) 'special-block))
                (cl-member block-type special-blocks-not-for-handling :test #'equal))
            (funcall fn attribute element property)
          (let ((myplist (append (funcall fn attribute element property) '(:test "test"))))
            (when (equal (org-element-type element) 'special-block)
              (plist-put myplist :class "fancy-block")
              (plist-put myplist
                         :data-before (cheap-org-export-string-as
                                       (concat block-type-str
                                               block-title)
                                       'html
                                       t))
              ;; dont export :source if it is just a path to a local file (starts with forward slash)
              (plist-put myplist
                         :data-after (when (and citation (not (string-prefix-p "/" citation)))
                                       (cheap-org-export-string-as citation org-export-current-backend t))))
            ;; (when caption
            ;;   (plist-put myplist :data-caption caption))
            (when element-name
              (plist-put myplist :id element-name)
              (plist-put myplist :data-id element-name))
            myplist)))))
  (advice-add #'org-export-read-attribute :around #'my-org-export-read-attribute-hook)

  ;; redefine
  ;; overwrite the function to add the data-language attribute to the code blocks
  (defun org-html-src-block (src-block _contents info)
    "Transcode a SRC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
    (if (org-export-read-attribute :attr_html src-block :textarea)
        (org-html--textarea-block src-block)
      (let* ((lang (org-element-property :language src-block))
             (code (org-html-format-code src-block info))
             (label (let ((lbl (org-html--reference src-block info t)))
                      (if lbl (format " id=\"%s\"" lbl) "")))
             (klipsify  (and  (plist-get info :html-klipsify-src)
                              (member lang '("javascript" "js"
                                             "ruby" "scheme" "clojure" "php" "html")))))
        (format "<div class=\"org-src-container\" data-language=\"%s\">\n%s%s\n</div>"
                (org-element-property :language src-block)
                ;; Build caption.
                (let ((caption (org-export-get-caption src-block)))
                  (if (not caption) ""
                    (let ((listing-number
                           (format
                            "<span class=\"listing-number\">%s </span>"
                            (format
                             (org-html--translate "Listing %d:" info)
                             (org-export-get-ordinal
                              src-block info nil #'org-html--has-caption-p)))))
                      (format "<label class=\"org-src-name\">%s%s</label>"
                              listing-number
                              (org-trim (org-export-data caption info))))))
                ;; Contents.
                (if klipsify
                    (format "<pre><code class=\"src src-%s\" %s%s>%s</code></pre>"
                            lang ; lang being nil is OK.
                            label
                            (if (string= lang "html")
                                " data-editor-type=\"html\""
                              "")
                            code)
                  (format "<pre class=\"src src-%s\"%s>%s</pre>"
                          ;; Lang being nil is OK.
                          lang label code))))))

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
                (citation (org-block-property :source special-block))
                (label (org-block-property :name special-block)))
            (when (not label)
              ;; (setq label (generate-random-string 7)))
              (setq label ""))
            (if dependency
                (progn
                  (when (not (string-search "[" dependency)) ;; if its a link without the brackets
                    (setq dependency (format "\\(\\to\\) [[%s]]" dependency)))
                  (setq title (format "%s %s" title (cheap-org-export-string-as dependency 'latex t))))
              (when (org-block-property :on-prev special-block)
                (setq title (format "%s \\(\\to\\) %s" title "previous block"))))
            (concat (format "\\begin{myenv}{%s}{%s}[%s]%s\n" block-type label title ;; note that title can be broken into multiple lines with \\ which may also allow for multiple titles i guess
                            (if citation (format "[%s]" (cheap-org-export-string-as citation 'latex t)) ""))
                    contents
                    (format "\\end{myenv}")))))))
  ;; (advice-add #'org-latex-special-block :around #'my-org-latex-special-block-advice)

  ;; redefine
  ;; redefine to insert :name (by default org only recognizes #+name)
  (defun org-latex-special-block (special-block contents info)
    "Transcode a SPECIAL-BLOCK element from Org to LaTeX.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
    (let ((type (org-element-property :type special-block))
          (opt (org-export-read-attribute :attr_latex special-block :options))
          ;; (caption (org-latex--caption/label-string special-block info))
          (caption (when (org-block-property :name special-block)
                     (format "\\label{%s}"
                             (org-block-property :name special-block))))
          (caption-above-p (org-latex--caption-above-p special-block info)))
      (concat (format "\\begin{%s}%s\n" type (or opt ""))
              (and caption-above-p caption)
              contents
              (and (not caption-above-p) caption)
              (format "\\end{%s}" type))))

  ;; enforce some default keywords for all org buffers (in a hacky way)
  (defun my-org-collect-keywords-advice (orig-func &rest args)
    (let ((old-buffer (current-buffer)))
      (with-temp-buffer
        (insert-buffer-substring old-buffer)
        (insert "\n#+setupfile: ~/.emacs.d/setup.org\n#+include: ~/brain/private.org\n#+setupfile: ~/brain/private.org\n")
        (apply orig-func args))))
  (advice-add #'org-collect-keywords :around #'my-org-collect-keywords-advice)

  ;; this function sometimes tries to select a killed buffer and it causes an async error that cant be caught, so im modifying it to ignore errors
  (defun org-latex-preview--failure-callback-advice (orig-func &rest args)
    (ignore-errors (apply orig-func args)))
  (advice-add #'org-latex-preview--failure-callback :around #'org-latex-preview--failure-callback-advice)

  ;; modified it to remove --bbox=preview, to prevent long latex previews from getting cut off
  (plist-put (alist-get 'dvisvgm org-latex-preview-process-alist)
             :image-converter
             (list
              (concat "dvisvgm --page=1- --optimize --clipjoin --relative --no-fonts"
                      (if (>= org-latex-preview--dvisvgm3-minor-version 2)
                          " -v3 --message='processing page {?pageno}: output written to {?svgfile}'" "")
                      " -o %B-%%9p.svg %f")))

  ;; org inserts inline height value, override that, it causes problems with big latex previews on smaller screens where the previews take more height than they need because their width was decreased (using max-width: 100%) but their height wasnt
  (defun my-latex-preview-filter (transcoded-text backend channel)
    (if (equal backend 'html)
        (replace-regexp-in-string (regexp-quote "\"height: ")
                                  "\"max-height: "
                                  transcoded-text)
      transcoded-text))
  (add-to-list 'org-export-filter-latex-environment-functions 'my-latex-preview-filter)
  (add-to-list 'org-export-filter-latex-fragment-functions 'my-latex-preview-filter)

  ;; get rid of the metadata (<meta>) that org mode inserts in html exports
  (defun my-org-html-meta-tags-default-advice (_)
    )
  (advice-add #'org-html-meta-tags-default :filter-return #'my-org-html-meta-tags-default-advice)

  ;; this is a bad idea, but i copied the function and modified it to work with blk, since there's really no other sane way to go about it (for now atleast..)
  (defun org-babel-ref-resolve (ref)
    "Resolve the reference REF and return its value."
    (save-window-excursion
      (with-current-buffer (or org-babel-exp-reference-buffer (current-buffer))
        (save-excursion
          (let ((case-fold-search t)
                args new-refere new-header-args new-referent split-file split-ref
                index contents)
            ;; if ref is indexed grab the indices -- beware nested indices
            (when (and (string-match "\\[\\([^\\[]*\\)\\]$" ref)
                       (let ((str (substring ref 0 (match-beginning 0))))
                         (= (cl-count ?\( str) (cl-count ?\) str))))
              (if (> (length (match-string 1 ref)) 0)
                  (setq index (match-string 1 ref))
                (setq contents t))
              (setq ref (substring ref 0 (match-beginning 0))))
            ;; assign any arguments to pass to source block
            (when (string-match
                   "^\\(.+?\\)\\(\\[\\(.*\\)\\]\\|\\(\\)\\)(\\(.*\\))$" ref)
              (setq new-refere      (match-string 1 ref))
              (setq new-header-args (match-string 3 ref))
              (setq new-referent    (match-string 5 ref))
              (when (> (length new-refere) 0)
                (when (> (length new-referent) 0)
                  (setq args (mapcar (lambda (ref) (cons :var ref))
                                     (org-babel-ref-split-args new-referent))))
                (when (> (length new-header-args) 0)
                  (setq args (append (org-babel-parse-header-arguments
                                      new-header-args)
                                     args)))
                (setq ref new-refere)))
            (when (string-match "^\\(.+\\):\\(.+\\)$" ref)
              (setq split-file (match-string 1 ref))
              (setq split-ref (match-string 2 ref))
              (when (file-exists-p split-file)
                (find-file split-file)
                (setq ref split-ref)))
            (org-with-wide-buffer
             (goto-char (point-min))
             (let* ((params (append args '((:results . "none"))))
                    (regexp (org-babel-named-data-regexp-for-name ref))
                    (result
                     (catch :found
                       ;; Check for code blocks or named data.
                       (while (re-search-forward regexp nil t)
                         ;; Ignore COMMENTed headings and orphaned
                         ;; affiliated keywords.
                         (unless (org-in-commented-heading-p)
                           (let ((e (org-element-at-point)))
                             (when (equal (org-element-property :name e) ref)
                               (goto-char
                                (org-element-post-affiliated e))
                               (pcase (org-element-type e)
                                 (`babel-call
                                  (throw :found
                                         (org-babel-execute-src-block
                                          nil (org-babel-lob-get-info e) params)))
                                 ((and `src-block (guard (not contents)))
                                  (throw :found
                                         (org-babel-execute-src-block
                                          nil nil
                                          (and
                                           (not org-babel-update-intermediate)
                                           params))))
                                 ((and (let v (org-babel-read-element e))
                                       (guard v))
                                  (throw :found v))
                                 (_ (error "Reference not found")))))))
                       ;; Check for local or global headlines by ID.
                       (when (org-babel-ref-goto-headline-id ref)
                         (throw :found (org-babel-ref-headline-body)))
                       ;; blk
                       (when (blk-find-by-id ref)
                         (let ((result (car (blk-find-by-id ref))))
                           (blk-with-file-as-current-buffer
                            (plist-get result :filepath)
                            (goto-char (plist-get result :position))
                            (let ((info (org-babel-get-src-block-info t)))
                              (throw :found
                                     (org-babel-execute-src-block nil info params))))))
                       ;; Check the Library of Babel.
                       (let ((info (cdr (assq (intern ref)
                                              org-babel-library-of-babel))))
                         (when info
                           (throw :found
                                  (org-babel-execute-src-block nil info params))))
                       (error "Reference `%s' not found in this buffer" ref))))
               (cond
                ((and result (symbolp result)) (format "%S" result))
                ((and index (listp result))
                 (org-babel-ref-index-list index result))
                (t result)))))))))

  ;; dont insert \\usepackage[inkscapelatex=false]{svg} when exporting docs with svg's, i do that myself
  ;; (defun my-ox-latex-disable-svg-handling ()
  ;;   (interactive)
  ;;   (setf (org-export-backend-feature-implementations (org-export-get-backend 'latex))
  ;;         (cl-remove-if (lambda (entry)
  ;;                         (equal (car entry) 'svg))
  ;;                       (org-export-backend-feature-implementations (org-export-get-backend 'latex)))))
  ;; (my-ox-latex-disable-svg-handling)
  (defun my-org-latex-export-filter-remove-svg (data backend channel)
    ;; org-latex-preivew (for now, should be fixed i think by teco) inserts
    ;; the string <?xml version='1.0' encoding='UTF-8'?> into the html
    ;; which dom.el renders as nil, we need to get rid of those, otherwise
    ;; we're gonna get unwanted results in the rendered html output
    (replace-regexp-in-string
     (regexp-quote "\\usepackage[inkscapelatex=false]{svg}")
     ""
     data))
  (add-to-list 'org-export-filter-final-output-functions 'my-org-latex-export-filter-remove-svg)

  ;; redefine
  ;; to insert `figure` environment when #+caption is present
  (defun org-latex-latex-environment (latex-environment _contents info)
    "Transcode a LATEX-ENVIRONMENT element from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual information."
    (when (plist-get info :with-latex)
      (let* ((value (org-remove-indentation
                     (org-element-property :value latex-environment)))
             (type (org-latex--environment-type latex-environment))
             (caption (if (eq type 'math)
                          (org-latex--label latex-environment info nil t)
                        (org-latex--caption/label-string latex-environment info)))
             (caption-above-p
              (or (eq type 'math)
                  (org-latex--caption-above-p latex-environment info))))
        (if (not (or (org-element-property :name latex-environment)
                     (org-element-property :caption latex-environment)))
            value
          ;; Environment is labeled: label must be within the environment
          ;; (otherwise, a reference pointing to that element will count
          ;; the section instead).  Also insert caption if `latex-environment'
          ;; is not a math environment.
          (with-temp-buffer
            (insert "\\begin{figure}\\centering\n")
            (insert value)
            (goto-char (point-max))
            (insert "\n\\end{figure}")
            (if caption-above-p
                (progn
                  (goto-char (point-min))
                  (forward-line))
              (goto-char (point-max))
              (forward-line -1))
            (insert caption)
            (buffer-string))))))

  ;; add empty #+caption to any #+name'd block that doesnt have a caption
  (defun my-add-empty-caption (_)
    (let ((offset 0))
      (org-element-map
          (org-element-parse-buffer)
          'latex-environment
        (lambda (elm)
          (let ((pos (+ (org-element-property :begin elm) offset))
                (text-to-insert "#+caption: \n"))
            (when (and (org-element-property :name elm) (not (org-element-property :caption elm)))
              (beginning-of-line)
              (goto-char pos)
              (insert text-to-insert)
              (setq offset (+ offset (length text-to-insert)))))))))
  (add-to-list 'org-export-before-processing-functions 'my-add-empty-caption)

  ;; redefine
  ;; make it export links to my website and use \cref
  (with-eval-after-load-all
   '(org blk)
   (defun blk-org-export (link desc format)
     "Return the LINK with DESC converted into html or markdown FORMAT.
If LINK is not found, just return it as is."
     (if (blk-find-by-id link)
         (let* ((linked-file (plist-get (car (blk-find-by-id link)) :filepath))
                (desc (or desc link))
                (linked-file-no-ext (file-name-sans-extension (org-export-file-uri linked-file)))
                (latex-link
                 (if desc
                     (format "\\hyperref[%s]{%s}" link desc)
                   (format "\\cref{%s}" link))))
           (when (not (equal buffer-file-name linked-file))
             (setq latex-link (format "\\href{%s}{%s}" (url-for-blk-id link) (or desc link))))
           (cond
            ((eq format 'html) (format "<a href=\"%s.html#%s\">%s</a>" linked-file-no-ext link desc))
            ((eq format 'md) (format "[%s](%s.md)" desc linked-file-no-ext))
            ((eq format 'latex) latex-link)
            (t link)))
       link)))

  ;; redefine
  ;; make it insert the caption into the element, this is fragile as it doesnt encode the text before it inserts it into data-caption
  (defun org-html-latex-environment (latex-environment _contents info)
    "Transcode a LATEX-ENVIRONMENT element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
    (let ((processing-type (plist-get info :with-latex))
          (latex-frag (org-remove-indentation
                       (org-element-property :value latex-environment)))
          (label (org-html--reference latex-environment info t))
          (caption (org-element-property :caption latex-environment)))
      (if (memq processing-type '(t mathjax))
          (org-html--as-latex
           latex-environment info
           (if (org-string-nw-p label)
               (replace-regexp-in-string "\\`.*"
                                         (format "\\&\n\\\\label{%s}" label)
                                         latex-frag)
             latex-frag))
        (format "\n<div%s class=\"equation-container\">\n%s\n<span class=\"caption\">%s</span>\n</div>"
                ;; ID.
                (if (org-string-nw-p label) (format " id=\"%s\"" label) "")
                ;; Contents.
                (format "<span class=\"equation\">\n%s\n</span>" (org-html--as-latex latex-environment info latex-frag))
                (if caption
                    (cheap-org-export-string-as (org-element-interpret-data caption) 'html t)
                  "")))))

  ;; for xopp
  ;; (org-add-link-type "xopp-figure")
  ;; (org-add-link-type "xopp-pages")

  ;; (org-link-set-parameters "xopp-figure"
  ;;                          :preview #'org-xopp-figure-image
  ;;                          )

  ;; (defun org-xopp-figure-image (ov path link)
  ;;   "overlay .xopp file links in the current org buffer with the corresponding sketches."
  ;;   (let ((absolute-path (expand-file-name path))
  ;;         (output-file))
  ;;     ;; check if the .xopp file exists
  ;;     (if (not (file-exists-p absolute-path))
  ;;         (message "file not found: %s" absolute-path)
  ;;       ;; export the .xopp file to an image if not already done
  ;;       (setq output-file (s-trim (shell-command-to-string-no-stderr
  ;;                                  (format "generate_xopp_figure.sh %s"
  ;;                                          absolute-path))))
  ;;       (org-link-preview-file ov output-file link))))

  ;; (defun org-xopp-figure-image (ov path link)
  ;;   "overlay .xopp file links in the current org buffer with the corresponding sketches."
  ;;   (let ((absolute-path (expand-file-name path)))
  ;;     ;; check if the .xopp file exists
  ;;     (if (not (file-exists-p absolute-path))
  ;;         (message "file not found: %s" absolute-path)
  ;;       ;; export the .xopp file to an image if not already done
  ;;       (lexical-let ((ov ov)
  ;;                     (link link))
  ;;         (prog1 t
  ;;           (make-process
  ;;            :name "xopp-preview"
  ;;            :buffer (generate-new-buffer " *xopp-preview*")
  ;;            :command (list "sh"
  ;;                           "-c"
  ;;                           (format "generate_xopp_figure.sh '%s' 2>/dev/null"
  ;;                                   absolute-path))
  ;;            :sentinel (lambda (_proc _status)
  ;;                        (when-let* ((output-file
  ;;                                     (with-current-buffer
  ;;                                         (process-buffer _proc)
  ;;                                       (string-trim (buffer-string))))
  ;;                                    (org-buf (overlay-buffer ov))
  ;;                                    (buffer-live-p org-buf))
  ;;                          (with-current-buffer org-buf
  ;;                            (org-link-preview-file ov output-file link))))))))))

  ;; center images, limit max width of images
  (setq org-image-align 'center)
  (setq org-image-max-width 0.7)

  (with-eval-after-load 'org-latex-preview
    (when *latex-previews-enabled-p*
      (enable-latex-previews)))

  ;; redefine
  ;; redefine to always center images no matter what
  (defun org-image--align (link)
    "Determine the alignment of the image LINK.
LINK is a link object.

In decreasing order of priority, this is controlled:
- Per image by the value of `:center' or `:align' in the
affiliated keyword `#+attr_org'.
- By the `#+attr_html' or `#+attr_latex` keywords with valid
  `:center' or `:align' values.
- Globally by the user option `org-image-align'.

The result is either nil or one of the strings \"left\",
\"center\" or \"right\".

\"center\" will cause the image preview to be centered, \"right\"
will cause it to be right-aligned.  A value of \"left\" or nil
implies no special alignment."
    (let ((par (org-element-lineage link 'paragraph)))
      ;; Only align when image is not surrounded by paragraph text:
      (when (and par ; when image is not in paragraph, but in table/headline/etc, do not align
                 ;; (= (org-element-begin link)
                 ;;    (save-excursion
                 ;;      (goto-char (org-element-contents-begin par))
                 ;;      (skip-chars-forward "\t ")
                 ;;      (point)))           ;account for leading space
                 ;;                        ;before link
                 ;; (<= (- (org-element-contents-end par)
                 ;;        (org-element-end link))
                 ;;     1)
                 )                  ;account for trailing newline
                                        ;at end of paragraph
        (save-match-data
          ;; Look for a valid ":center t" or ":align left|center|right"
          ;; attribute.
          ;;
          ;; An attr_org keyword has the highest priority, with
          ;; any attr.* next.  Choosing between these is
          ;; unspecified.
          (let ((center-re ":\\(center\\)[[:space:]]+t\\b")
                (align-re ":align[[:space:]]+\\(left\\|center\\|right\\)\\b")
                attr-align)
            (catch 'exit
              (org-element-properties-mapc
               (lambda (propname propval)
                 (when (and propval
                            (string-match-p ":attr.*" (symbol-name propname)))
                   (setq propval (car-safe propval))
                   (when (or (string-match center-re propval)
                             (string-match align-re propval))
                     (setq attr-align (match-string 1 propval))
                     (when (eq propname :attr_org)
                       (throw 'exit t)))))
               par))
            (if attr-align
                (when (member attr-align '("center" "right")) attr-align)
              ;; No image-specific keyword, check global alignment property
              (when (memq org-image-align '(center right))
                (symbol-name org-image-align))))))))

  )

(defun url-for-blk-id (blk-id)
  (let* ((linked-file (plist-get (car (blk-find-by-id blk-id)) :filepath))
         (html-filename (file-name-nondirectory (org-file-html-out-file linked-file))))
    (format "https://mahmoodsh36.github.io/%s\\#%s" html-filename blk-id)))

;; code for centering LaTeX preview
(use-package org-latex-preview
  :ensure nil
  :after org-latex-preview
  :config
  (defun my/org-latex-preview-uncenter (ov)
    (overlay-put ov 'before-string nil))
  (defun my/org-latex-preview-recenter (ov)
    (overlay-put ov 'before-string (overlay-get ov 'justify)))
  (defun my/org-latex-preview-center (ov)
    (save-excursion
      (goto-char (overlay-start ov))
      (when-let* ((elem (org-element-context))
                  ((or (eq (org-element-type elem) 'latex-environment)
                       (string-match-p "^\\\\\\[" (org-element-property :value elem))))
                  (img (overlay-get ov 'display))
                  (prop `(space :align-to (- center (0.55 . ,img))))
                  (justify (propertize " " 'display prop 'face 'default)))
        (overlay-put ov 'justify justify)
        (overlay-put ov 'before-string (overlay-get ov 'justify)))))
  (define-minor-mode org-latex-preview-center-mode
    "Center equations previewed with `org-latex-preview'."
    :global nil
    (if org-latex-preview-center-mode
        (progn
          (add-hook 'org-latex-preview-overlay-open-functions
                    #'my/org-latex-preview-uncenter nil :local)
          (add-hook 'org-latex-preview-overlay-close-functions
                    #'my/org-latex-preview-recenter nil :local)
          (add-hook 'org-latex-preview-overlay-update-functions
                    #'my/org-latex-preview-center nil :local)
          )
      (remove-hook 'org-latex-preview-overlay-close-functions
                    #'my/org-latex-preview-recenter)
      (remove-hook 'org-latex-preview-overlay-update-functions
                    #'my/org-latex-preview-center)
      (remove-hook 'org-latex-preview-overlay-open-functions
                   #'my/org-latex-preview-uncenter)))
  (add-hook 'org-mode-hook 'org-latex-preview-center-mode)
  ;; do we really want this following line?
  (add-to-list 'org-latex-preview-overlay-update-functions 'my/org-latex-preview-center)
  )

;; insert a blank line between everything in exports
(defun my-export-newlines (_)
  (let ((inhibit-message t))
    (goto-char (point-min))
    (replace-regexp "\\(^[^#: \\\\]\\{1\\}.*\\)\\.\n" "\\1.\n\n") ;; insert newlines after lines ending with dot
    (replace-regexp "\\(^[^#: \\\\]\\{1\\}.*\\)\\.)\n" "\\1.\n\n") ;; insert newlines after lines ending with ".)"
    ))
;; (with-eval-after-load 'org
;;   (add-to-list 'org-export-before-processing-functions 'my-export-newlines))

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
          (goto-char (point-max))
          (insert (format-time-string "#+filetags: :daily:\n#+title: %Y-%m-%d")))
      (find-file todays-file))))

(defun open-prev-file ()
  "open the file of the day previous to the current file's day. if current file isnt a daily org file, open yesterday's daily file."
  (interactive)
  (let* ((current-files-date (condition-case nil
                                 (date-to-time (file-name-base buffer-file-name))
                               (error (current-time))))
         (prev-time-str (format-time-string "%Y-%m-%d" (time-subtract current-files-date (days-to-time 1))))
         (prev-file (format-time-string (from-brain (format "/daily/%s.org" prev-time-str)))))
    (find-file prev-file)))

(defun open-next-file ()
  "similar to `open-prev-file', except it opens the file for the day after."
  (interactive)
  (let* ((current-files-date (condition-case nil
                                 (date-to-time (file-name-base buffer-file-name))
                               (error (current-time))))
         (next-time-str (format-time-string "%Y-%m-%d" (time-add current-files-date (days-to-time 1))))
         (next-file (format-time-string (from-brain (format "/daily/%s.org" next-time-str)))))
    (find-file next-file)))

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
(defun org-files-with-property (prop)
  (grep-org-dir-get-files *notes-dir* (format "#\\+%s:.*" prop)))
(defun my-org-agenda-files ()
  (org-files-with-tag "todo"))

;; https://github.com/alphapapa/org-ql/blob/master/examples.org
;; https://github.com/alphapapa/org-super-agenda
;; https://github.com/alphapapa/org-super-agenda/blob/master/examples.org
(defun my-org-agenda ()
  (interactive)
  (setq org-agenda-files (my-org-agenda-files))

  (let ((org-inhibit-startup t)
        (org-startup-with-latex-preview nil)
        (org-super-agenda-groups
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
    `(let ((,present-buffer (find-buffer-visiting ,file))
           (org-inhibit-startup t))
       (save-excursion
         (with-current-buffer (or (find-buffer-visiting ,file)
                                  (find-file-noselect ,file))
           (setq ,result (progn ,@body))
           (when (not ,present-buffer)
             (kill-buffer (current-buffer)))
           ,result)))))

(defun export-org-file (file &rest kw)
  "export a node's file to both html and pdf, if pdf-p is true, export to pdf, if html-p is true, export to html"
  (with-file-as-current-buffer
   file
   (when (not org-transclusion-mode) (org-transclusion-mode))
   (when (plist-get kw :pdf-p)
     (my-org-to-pdf (plist-get kw :async)))
   (when (plist-get kw :html-p)
     (my-org-to-html nil (plist-get kw :force)))))

;; (defun files-linked-from-org-file (filepath)
;;   (with-org-file-faster filepath
;;    (remove
;;     nil
;;     (org-element-map (org-element-parse-buffer) 'link
;;       (lambda (mylink)
;;         (let ((filepath
;;                (pcase (org-element-property :type mylink)
;;                  ("blk" (plist-get
;;                          (car (blk-find-by-id (org-element-property :path mylink)))
;;                          :filepath))
;;                  ("denote" (denote-get-path-by-id (org-element-property :path mylink)))
;;                  (_ nil))))
;;           filepath))))))

(defun files-linked-from-org-file (filepath)
  (car
   (map-org-files
    filepath
    (lambda ()
      (org-element-map (org-element-parse-buffer) 'link
        (lambda (mylink)
          (let ((filepath
                 (pcase (org-element-property :type mylink)
                   ("blk" (plist-get
                           (blk-find-by-id-using-hashtable
                            (org-element-property :path mylink))
                           :filepath))
                   ("denote" (denote-get-path-by-id
                              (org-element-property :path mylink)))
                   (_ nil))))
            filepath)))))))

(defun export-node (node exceptions &rest kw)
  "export node, export all nodes/files it links to, and all files linked from those and so on, basically we're exporting the connected subgraph the node exists in, `exceptions' is used for recursion to keep a record of exported nodes"
  (if (and node (not (cl-find node exceptions :test #'string=)))
      (progn
        (push node exceptions)
        (when (and node (funcall should-export-org-file-function node))
          ;; (message (format "exporting: %s" node))
          (condition-case nil
              (apply #'export-org-file node kw)
            (error (message "failed to export %s" node)))
          ;; (when (< depth export-graph-depth)
          ;;   (let ((nodes (files-linked-from-org-file node)))
          ;;     (dolist (other-node nodes)
          ;;       (when (funcall should-export-org-file-function other-node) ;; to avoid jumping to nodes that arent for exporting anyway
          ;;         (when other-node (message (format "exporter jumping to: %s" other-node)))
          ;;         (setf exceptions (apply #'export-node (nconc (list other-node exceptions (1+ depth)) kw)))))))
          )
        exceptions)
    exceptions))

;; do we really need "recursive exporting"? i think i should keep it as a side feature for specific use cases
(defun export-all-org-files (&rest kw)
  "export all org mode files using `export-org-file', use `should-export-org-file-function' to check whether a file should be exported"
  (blk-update-cache)
  (let ((exceptions)
        (org-startup-with-latex-preview nil)
        (files-to-export-1 (list-org-files-to-export))
        ;; i need my transclusions present when exporting
        (org-mode-hook (cons 'org-transclusion-mode org-mode-hook)))
    (dolist (file files-to-export-1)
      (setq exceptions (apply #'export-node (nconc (list file exceptions) kw)))
      (setq exceptions (push file exceptions))
      (message "%s/%s" (length exceptions) (length files-to-export-1)))))

(defun list-org-files-to-export ()
  (reverse (list-note-files)))

;; this doesnt do all that is necessary
;; (defun export-all-org-files-to-html-and-pdf ()
;;   (interactive)
;;   (let ((should-export-org-file-function #'should-export-org-file))
;;     (export-all-org-files :html-p t :pdf-p t)))

(defvar files-to-export) ;; to make it dynamically bound
(defun export-all-org-files-to-html ()
  (interactive)
  (message "collecting files to export")
  (let* ((org-inhibit-startup t) ;; to make opening org files faster disable startup
         (should-export-org-file-function #'should-export-org-file)
         (files-to-export (collect-org-files-to-export)))
    (message "collected %s files to export" (length files-to-export))
    (map-org-dir-elements *notes-dir* ":forexport:" 'headline
                          (lambda (_) (org-export-heading-html)))
    ;; (export-entries-page)
    (export-all-org-files :html-p t)
    (generate-and-save-website-search-data)
    (export-html-as-org-file "search" (org-file-contents (from-template "search.html")))))

(defun export-all-org-files-to-pdf ()
  (interactive)
  (let* ((org-inhibit-startup t) ;; to make opening org files faster disable startup
         (should-export-org-file-function (lambda (_) t))
         (files-to-export (collect-org-files-to-export)))
    (export-all-org-files :pdf-p t :async nil)))

(defun export-all-math-org-files-to-html ()
  (interactive)
  (let ((should-export-org-file-function
         (lambda (orgfile)
           (with-file-as-current-buffer orgfile
            (or (cl-find "math" (org-get-tags) :test 'equal)
                (cl-find "cs" (org-get-tags) :test 'equal)))))
        (*static-html-dir* (expand-file-name "~/work/test/")))
    (map-org-dir-elements
     *notes-dir*
     ":forexport:"
     'headline
     (lambda (_) (org-export-heading-html)))
    (export-all-org-files :html-p t)))

(defun export-current-buffer (&rest kw)
  "gets the node associated with the current buffer, exports it"
  (interactive)
  (apply #'export-org-file buffer-file-name kw))

(defun custom-org-collect-keywords (keywords)
  "Collect values of KEYWORDS from the current Org buffer.
KEYWORDS is a list of keyword strings, like '(\"TITLE\" \"AUTHOR\")."
  (let (collected)
    (save-excursion
      (goto-char (point-min))
      ;; Iterate over each keyword
      (dolist (kw keywords)
        (when (re-search-forward (format "^#\\+%s: \\(.*\\)$" (regexp-quote kw)) nil t)
          (push (cons kw (match-string 1)) collected))))
    ;; Return the collected keyword-value pairs
    collected))

;; this seems to be slower, i think it may be parsing the whole buffer as an org buffer regardless
(defun org-get-keyword (kw)
  (let ((value (cadar (org-collect-keywords (list kw)))))
    (if (string-prefix-p "(" value)
        (eval (car (read-from-string value)))
      value)))
(defun org-get-keyword-faster (kw)
  (let ((value (cdar (custom-org-collect-keywords (list kw)))))
    (if (string-prefix-p "(" value)
        (eval (car (read-from-string value)))
      value)))
(defun should-export-org-file (file)
  (if (boundp 'files-to-export)
      (cl-member file files-to-export :test #'equal)
    (cl-member file (collect-org-files-to-export) :test #'equal)))
(defun org-export-dir-name (file)
  "whether the current org buffer should be exported"
  (car
   (cdar
    (with-file-as-current-buffer
     file
     (org-collect-keywords '("export_section"))))))
;; this is quicker, it grabs the date from the filename
;; (defun org-file-grab-time (orgfile)
;;   (timestamp-to-time (string-to-number (file-name-nondirectory orgfile))))
(defun org-file-grab-time (orgfile)
  (with-file-as-current-buffer-faster orgfile
   (let* ((date-string (or (org-get-keyword-faster "actual_date") (org-get-keyword-faster "date")))
          (time (when date-string (date-to-time date-string))))
     time)))
(defun org-file-grab-keyword (orgfile kw)
  (with-file-as-current-buffer
   orgfile
   (org-get-keyword kw)))
(defun org-file-grab-title (orgfile)
  (with-file-as-current-buffer-faster
   orgfile
   (org-get-title)))
(defun org-file-grab-keyword-faster (orgfile kw)
  (with-file-as-current-buffer-faster
   orgfile
   (let ((value (cdar (custom-org-collect-keywords (list kw)))))
     (if (string-prefix-p "(" value)
         (eval (car (read-from-string value)))
       value))))

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
  (map-org-dir-elements
   *notes-dir*
   rgx
   'src-block
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
  "example: #+begin_myblock :myproperty whatever-value, notice that this function disables evaluation when it parses the headers"
  (let ((result
         (or (org-element-property property block)
             (alist-get
              property
              (org-babel-parse-header-arguments
               (org-element-property :parameters block)
               t))
             (member property
                     (mapcar 'car (org-babel-parse-header-arguments
                                   (org-element-property :parameters block)
                                   t))))))
    (when result
      (format "%s" result))))

(defun org-block-property-1 (property block)
  (let ((result
         (or (org-element-property property block)
             (alist-get
              property
              (org-babel-parse-header-arguments
               (org-element-property :parameters block)
               t))
             (member property
                     (mapcar 'car (org-babel-parse-header-arguments
                                   (org-element-property :parameters block)
                                   t))))))
    result))

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

(cl-defun my-org-to-html (&optional heading (force nil))
  (interactive)
  ;; so that org mode places the latex previews in the specified dir
  ;; (plist-put org-html-latex-image-options :image-dir "ltx")
  (plist-put org-html-latex-image-options :inline '(svg svg-embed))
  (plist-put org-html-latex-image-options :page-width 0.6)

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
                           (insert-file-contents (from-template "head.html"))
                           (buffer-string))))
         (date-string-original (or (org-get-keyword "actual_date") (org-get-keyword "date")))
         (date-string
          (when date-string-original
            (format-time-string "%Y-%m-%d" (date-to-time date-string-original))))
         (my-preamble
          (concat
           (with-temp-buffer
             (insert-file-contents (from-template "preamble.html"))
             (buffer-string))
           (if (or (not heading)
                   (and heading (not (cl-member "notitle" (org-get-tags) :test 'equal))))
               (format "<h1 class=\"main-title\">%s</h1>%s%s"
                       title
                       (if desc (format "<span class=\"desc\">%s</span>" desc) "")
                       (if date-string (format "<span class='date'>%s</span>" date-string) ""))
             "")))
         (org-html-preamble-format (list (list "en" my-preamble)))
         (search-data))
    (when (not (and (not heading)
                    buffer-file-name
                    (file-newer-than-file-p outfile buffer-file-name)
                    (not force)))
      (when heading
        (org-narrow-to-subtree))
      (org-export-to-file 'html outfile
        nil nil nil nil nil nil)
      (when heading
        (widen))
      ;; (copy-directory "ltx" *static-html-dir* t)
      (dolist (filepath (directory-files (join-path *template-html-dir* "static") t "html\\|css\\|js"))
        (copy-file filepath *static-html-dir* t)))))

(defun generate-website-search-data ()
  (let ((data (blk-collect-all))
        (new-data)
        (org-file-to-html-file-alist))
    (cl-loop for num from 1
             for entry in data
             do (let* ((org-startup-with-latex-preview nil)
                       (org-file (plist-get entry :filepath))
                       (html-file (alist-get org-file org-file-to-html-file-alist)))
                  (message "%s/%s %s" num (length data) (plist-get entry :title))
                  (when (funcall should-export-org-file-function org-file)
                    (let ((entry-id (blk-extract-id entry)))
                      (plist-put entry :id entry-id)
                      (when (not html-file)
                        (setq html-file
                              (html-out-file
                               (org-file-grab-keyword-faster
                                org-file
                                "title")))
                        (push (cons org-file html-file) org-file-to-html-file-alist))
                      (plist-put entry :filepath (file-name-nondirectory html-file))
                      (plist-put entry
                                 :original-filename
                                 (file-name-nondirectory html-file))
                      (push entry new-data)))))
    new-data))
(defun generate-and-save-website-search-data ()
  (interactive)
  (with-temp-file
      (join-path *static-html-dir* "search.json")
    (insert (json-encode-array (generate-website-search-data)))))

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
      (with-file-as-current-buffer-faster
       book-org-file
       (push (list :title (org-get-keyword-faster "book_title")
                   :identifier (org-get-keyword-faster "identifier")
                   :author (org-get-keyword-faster "book_author")
                   :year (org-get-keyword-faster "book_year")
                   :file (org-get-keyword-faster "book_main_file")
                   :book-source-url (org-get-keyword-faster "book_source_url")
                   :bibtex-entry-name (org-get-keyword-faster "bibtex_entry_name"))
             books)))
    books))
(defun book-prompt ()
  (interactive)
  (let ((option (completing-read-cons
                 "book: "
                 (mapcar
                  (lambda (book)
                    (cons (format "%s - %s - %s"
                                  (plist-get book :title)
                                  (plist-get book :author)
                                  (plist-get book :year))
                          book))
                  (list-books)))))
    (plist-get (cdr option) :file)))

(defun generate-bib-file ()
  (interactive)
  (let ((books (list-books)))
    (with-temp-file (from-brain "auto.bib")
      (dolist (book books)
        (let ((title (plist-get book :title))
              (year (plist-get book :year))
              (first-author (string-trim (car (string-split (plist-get book :author) ",")))))
          (insert
           (format
            "@book{%s,
  author = {%s},
  title = {%s},
  year = %s,
  file = {%s},
  url = {%s}
}
"
            (or (plist-get book :bibtex-entry-name)
                (downcase (string-join (append (split-string title " ")
                                               (split-string first-author " ")
                                               (list year))
                                       "_")))
            (plist-get book :author)
            (plist-get book :title)
            (plist-get book :year)
            (plist-get book :file)
            (plist-get book :book-source-url))))))))

(defun entry-nodes-metadata ()
  (mapcar
   (lambda (orgfile)
     (list :title (org-file-grab-title orgfile)
           :books (entry-books orgfile)
           :nodes (entry-children orgfile)
           :time (org-file-grab-time orgfile)
           :image (org-file-grab-keyword orgfile "image")
           :id (org-file-grab-keyword orgfile "identifier")))
   (org-files-with-tag "entry")))

(defun point-on-last-line-p ()
  "Return t if the point is on the last line of the buffer, otherwise nil."
  (eq (line-number-at-pos) (line-number-at-pos (point-max))))

(defun parse-org-list ()
  (let ((mylist)
        (stop))
    (while (and (not (equal (org-element-type (org-element-at-point)) 'plain-list))
                (not stop))
      (forward-line)
      (when (point-on-last-line-p)
        (setq stop t)))
    (while (and (not stop)
                (cl-member (org-element-type (org-element-at-point)) '(plain-list item)))
      (forward-char 2)
      (push (buffer-substring-no-properties
             (org-element-contents-begin (org-element-at-point))
             (1- (org-element-contents-end (org-element-at-point))))
            mylist)
      (condition-case nil
          (org-forward-element)
        (error (setq stop t))))
    mylist))

(defun entry-books (orgfile)
  (with-file-as-current-buffer
   orgfile
   (let ((mybooks))
     (org-element-map (org-element-parse-buffer) 'headline
       (lambda (elm)
         (when (is-substring "books" (org-element-property :raw-value elm))
           (goto-char (org-element-begin elm))
           (setq mybooks (parse-org-list)))))
     mybooks)))

(defun get-blk-id-from-org-link-str (linkstr)
  (with-temp-buffer
    (insert linkstr)
    (org-mode)
    (org-element-property :path (org-element-context))))

(defun get-blk-ids-from-org-str (orgstr)
  (with-temp-buffer
    (insert orgstr)
    (org-mode)
    (org-element-map (org-element-parse-buffer)
        '(link)
      (lambda (link-elm)
        (org-element-property :path link-elm)))))

(defun entry-children (orgfile)
  (with-file-as-current-buffer
   orgfile
   (let ((mylist))
     (org-element-map (org-element-parse-buffer) 'headline
       (lambda (elm)
         (when (is-substring "nodes" (org-element-property :raw-value elm))
           (goto-char (org-element-begin elm))
           (setq mylist (parse-org-list)))))
     (apply
      #'append
      (mapcar
       'get-blk-ids-from-org-str
       mylist)))))

(defun generate-collage-html (entries)
  (let ((myhtml "<div class=\"collage\">"))
    (dolist (entry entries)
      (setq
       myhtml
       (format "%s
<div class='card fancy-button' data-ref='blk:%s'>
  <img src='%s' class='card-image' />
  <span class='card-title'>%s</span>
  <span class='card-subtitle'>%s</span>
  <span class='card-subtitle'>%s</span>
</div>"
               myhtml
               (plist-get entry :id)
               (when (plist-get entry :image)
                 (export-static-file (plist-get entry :image)))
               (plist-get entry :title)
               (or (plist-get entry :subtitle) "")
               (or (plist-get entry :subsubtitle) "")
               )))
    (concat myhtml "</div>")))

;; (defun export-entries-page ()
;;   (interactive)
;;   (let* ((metadata (entry-nodes-metadata))
;;          (myhtml (generate-collage-html metadata)))
;;     (export-html-as-org-file "index" myhtml)))

(defun export-html-as-org-file (title myhtml)
  (with-temp-buffer
    (insert (format "#+title: %s\n" title))
    (insert "#+begin_export html\n")
    (insert myhtml)
    (goto-char (point-max))
    (insert "\n#+end_export")
    (org-mode)
    (my-org-to-html)))

(defvar blk-hashtable)
(defun collect-org-files-to-export ()
  (let ((files-to-export)
        (entries (org-files-with-tag "entry"))
        (blk-hashtable (construct-blk-hashtable (blk-collect-all))))
    (dolist (orgfile entries)
      (push orgfile files-to-export)
      (let ((children (entry-children orgfile)))
        (dolist (child children)
          (let ((blk-entry (car (blk-find-by-id child))))
            (push (plist-get blk-entry :filepath) files-to-export)))))
    (setq files-to-export
          (cl-union files-to-export (org-files-with-property "export_section")))
    (dolist (file-to-export files-to-export)
      (setq files-to-export
            (cl-union
             files-to-export
             (org-files-connected-to-org-file
              file-to-export
              export-graph-depth)
             :test 'equal)))
    files-to-export))

(cl-defun org-files-connected-to-org-file (org-file &optional (depth 1))
  (when (and org-file (> depth 0))
    (let ((nodes (files-linked-from-org-file org-file)))
      (let ((connected-files))
        (dolist (node nodes)
          (when (not (cl-find node
                              (cl-union connected-files
                                        (list org-file)
                                        :test 'equal)
                              :test 'equal))
            (setq connected-files
                  (cl-union (cons node connected-files)
                            (org-files-connected-to-org-file node (1- depth))
                            :test #'equal))))
        connected-files))))

(defun get-latex-preview-svg-by-blk-id (blk-id)
  (let* ((blk-entry (car (blk-find-by-id blk-id)))
         (filepath (plist-get blk-entry :filepath))
         (position (plist-get blk-entry :position)))
    (with-file-as-current-buffer
     filepath
     (goto-char position)
     (beginning-of-line)
     (setq position (line-beginning-position))
     (when (s-prefix-p "#+name:"
                       (org-no-properties (thing-at-point 'line)))
       (forward-line))
     ;; (org-latex-preview)
     (let ((preview-table (org-latex-preview-cache-images (org-element-parse-buffer))))
       (cl-some
        (lambda (key)
          (when (equal position (org-element-begin key))
            (car (gethash key preview-table))))
        (hash-table-keys preview-table))))))

(defun export-static-file (filepath)
  (let* ((filename (file-name-nondirectory filepath))
         (new-filepath (join-path *static-html-dir* filename))
         (href (join-path *html-static-route* filename)))
    (copy-file filepath new-filepath t)
    href))

(defun book-collage (blk-ids)
  (generate-collage-html
   (mapcar
    (lambda (blk-id)
      (when (is-substring "blk" blk-id)
        (setq blk-id (get-blk-id-from-org-link-str blk-id)))
      (let* ((blk-filepath (plist-get (car (blk-find-by-id blk-id)) :filepath))
             (pdf-filepath (org-file-grab-keyword blk-filepath "book_main_file")))
        (if pdf-filepath
            (list :image (cover-from-pdf pdf-filepath)
                  :title (org-file-grab-keyword blk-filepath "book_title")
                  :subtitle (org-file-grab-keyword blk-filepath "book_author")
                  :subsubtitle (org-file-grab-keyword blk-filepath "book_year"))
          (message "no book_main_file defined for %s"
                   (org-file-grab-keyword blk-filepath "title")))))
    blk-ids)))

(defun cover-from-pdf (pdf-path)
  (let ((image-filepath (from-cache (file-name-nondirectory
                                     (replace-file-extension pdf-path "png")))))
    (shell-command (format "convert '%s[0]' '%s'" pdf-path image-filepath))
    image-filepath))

(defun parse-string-with-org-element (str)
  (with-temp-buffer
    (insert str)
    (org-mode)
    (org-element-context)))

(defun storage-path-for-current-org-buffer ()
  (let* ((myid (save-excursion (goto-char 0) (search-forward "identifier:") (blk-org-id-at-point nil)))
         (mypath (when myid (join-path (file-truename *data-dir*) "forbrain" myid))))
    (when myid
      (when (not (file-exists-p mypath))
        (mkdir mypath))
      mypath)))

(defun list-for-current-org-buffer ()
  "return `filename', prefixed by the path to the brain dir"
  (let ((mydir (storage-path-for-current-org-buffer)))
    (when mydir
      (directory-files mydir))))

(defun open-storage-dir-for-current-org-buffer ()
  (interactive)
  (find-file (storage-path-for-current-org-buffer)))

(defun my-new-todo ()
  (interactive)
  (let* ((todo-files (org-files-with-tag "todo"))
         (titles (map-org-files todo-files (lambda () (org-get-keyword "title"))))
         (selected-title (completing-read "file " titles))
         (selected-idx (cl-position selected-title titles :test 'equal))
         (selected-todo-file (when selected-idx (elt todo-files selected-idx)))
         (found nil))
    (when selected-idx
      (find-file selected-todo-file)
      (goto-char 0)
      (org-element-map
          (org-element-parse-buffer)
          'headline
        (lambda (head)
          (let ((head-title (save-excursion
                              (goto-char (org-element-begin head))
                              (elt (org-heading-components) 4))))
            (when (equal head-title "todos")
              (setq found t)
              (goto-char (org-element-begin head))
              (end-of-line)
              (call-interactively 'org-insert-todo-subheading)))))
      (when (not found)
        (goto-char (point-max))
        (end-of-line)
        (newline)
        (insert "* todos")
        (newline)
        (call-interactively 'org-insert-todo-subheading))
      (enter-append-if-evil))))

(defun enter-append-if-evil ()
  (when (and (boundp 'evil-mode) evil-mode)
    (call-interactively 'evil-append)))

;; (defmacro map-org-files (files &rest body)
;;   `(let ((org-inhibit-startup t)
;;          (org-element-cache-persistent)
;;          (org-element-use-cache)
;;          (org-mode-hook)
;;          (files (if (atom ,files) (list ,files) ,files))
;;          (gc-cons-threshold 100000000) ;; 100mb
;;          (coding-system-for-read 'utf-8))
;;      (with-temp-buffer
;;        (buffer-disable-undo)
;;        (mapcar
;;         (lambda (orgfile)
;;           (insert-file-contents orgfile nil nil nil t)
;;           (let ((major-mode 'org-mode))
;;             (progn ,@body)))
;;         files))))

(defun map-org-files (files func)
  (let ((org-inhibit-startup t)
        (org-element-cache-persistent)
        (org-element-use-cache)
        (org-mode-hook)
        (files (if (atom files) (list files) files))
        (gc-cons-threshold 100000000) ;; 100mb
        (coding-system-for-read 'utf-8))
    (with-temp-buffer
      (buffer-disable-undo)
      (mapcar
       (lambda (orgfile)
         (insert-file-contents orgfile nil nil nil t)
         (let ((major-mode 'org-mode))
           (funcall func)))
       files))))

(defun grab-all ()
  (map-org-files
   (list-note-files)
   'org-element-parse-buffer))

(defun new-xournalpp ()
  (interactive)
  (let* ((timestamp (current-unix-timestamp))
         (xopp-file (from-brain (format "pen/%s.xopp" timestamp))))
    (start-process "xournalpp" "xournalpp" "xournalpp" xopp-file)
    (insert (format "[[xopp-figure:%s]]" xopp-file))))

(defun my-open-at-point (&optional beg end)
  "open the file at point"
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (let ((path (if (and beg end)
                  (buffer-substring-no-properties beg end)
                (thing-at-point-file-at-point))))
    (my-xdg-open-file path)))

;; could be helpful, https://emacs.stackexchange.com/questions/52678/convert-org-table-to-ms-excel
(defun org-table-export-to-spreadsheet (arg)
  "export org table to spreadsheet formats, e.g. `ods', `xls', `xlsx'."
  (interactive "sFormat: ")
  (let* ((source-file  (file-name-sans-extension (buffer-file-name  (current-buffer))))
         (csv-file (concat source-file ".csv")))
    (org-table-export csv-file "orgtbl-to-csv")
    (org-odt-convert csv-file arg)))

;; advice to resize properly from 0-2500 to 0-600
(with-eval-after-load 'org-xopp
  (defun my-org-xopp-place-image-advice (fn buffer image-path link)
    (let* ((max-xopp-image-width (float 2500))
           (max-display-width 600)
           (org-image-actual-width
            (floor
             (* (/ (car (image-size
                         (create-image image-path)
                         t))
                   max-xopp-image-width)
                max-display-width))))
      (funcall fn buffer image-path link)))
  (advice-add #'org-xopp-place-image :around #'my-org-xopp-place-image-advice))

(defun cheap-org-export-string-as (str backend &optional body-only)
  (with-temp-buffer
    (insert str)
    (org-export-to-file backend "/tmp/test1" nil nil nil body-only))
  (org-file-contents "/tmp/test1"))

(provide 'config-org)