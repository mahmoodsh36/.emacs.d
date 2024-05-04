;; org mode configuration and setup

;; tecosaur's org-mode version
(use-package org
  :ensure ( :remotes ("tecosaur" :repo "https://git.tecosaur.net/tec/org-mode.git" :branch "dev")
            :files (:defaults "etc")))

(use-package org-contrib
  :ensure ( :host github :repo "emacsmirror/org-contrib"))

(defvar *latex-previews-enabled-p*
  (not (is-android-system))
  "whether latex previews for org mode are enabled for the current session")

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
  (start-process-shell-command
   "latex"
   "latex"
   (format "%s -shell-escape -output-directory=%s %s"
           org-latex-compiler
           (file-truename (get-latex-cache-dir-path))
           path)))

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

;; requires pre-isntallation of gcc/clang
;; (use-package org-roam
;;   :elpaca (org-roam :type git :repo "mahmoodsheikh36/org-roam")
;;   :custom
;;   (org-roam-directory (from-brain "notes"))
;;   ;; (org-roam-completion-everywhere t)
;;   :config
;;   ;; (setq org-roam-node-display-template "${title:*} ${tags:*}")
;;   ;; (org-roam-db-autosync-mode 1)
;;   ;; (require 'org-roam-export)
;;   ;; (require 'org-roam-protocol)
;;   ;; (global-set-key (kbd "C-c r f") 'org-roam-node-find)
;;   ;; (global-set-key (kbd "C-c r g") 'org-roam-graph)
;;   (setq org-roam-capture-templates
;;         '(("n" "note" plain "%?"
;;            :if-new (file+head "notes/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}") ;; "#+setupfile: ~/.emacs.d/setup.org\n#+include: ~/.emacs.d/common.org\n#+title: ${title}")
;;            :kill-buffer t :unnarrowed t :empty-lines-after 0)
;;           ("k" "quick note" plain "%?"
;;            :if-new (file+head "quick/%<%Y%m%d%H%M%S>.org" "#+filetags: :quick-note:")
;;            :kill-buffer t :unnarrowed t :empty-lines-after 0)
;;           ;;("d" "daily" plain "%?" ;;"* %T %?"
;;           ;;("d" "daily" plain "* %T %<%Y-%m-%d %H:%M:%S> %?"
;;           ("d" "daily" plain "* %T %?"
;;            :if-new (file+head "daily/%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n#+filetags: :daily:")
;;            :kill-buffer t :unnarrowed t :empty-lines-after 0 :immediate-finish t)
;;           ("t" "todo" plain "* TODO ${title}"
;;            :if-new (file+head "notes/agenda.org" "#+title: ${title}\n")))))

;; (defun org-roam-capture-no-title-prompt (&optional goto keys &key filter-fn templates info)
;;   (interactive "P")
;;   (org-roam-capture- :goto goto
;;                      :info info
;;                      :keys keys
;;                      :templates templates
;;                      :node (org-roam-node-create :title "")
;;                      :props '(:immediate-finish nil)))

(with-eval-after-load 'org
  (require 'org-attach)
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
  (add-hook 'org-babel-after-execute-hook (lambda ()
                                            (clear-image-cache)
                                            (org-redisplay-inline-images)
                                            (org-latex-preview)
                                            ))
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
  ;; decrease image size in latex exports
  ;; (setq org-latex-image-default-scale "2.0")
  ;; disable images from being scaled/their dimensions being changed
  ;; (setq org-latex-image-default-width "2.0")
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
  ;; creation dates for TODOs
  ;; (defun my/log-todo-creation-date (&rest ignore)
  ;;   "Log TODO creation time in the property drawer under the key 'CREATED'."
  ;;   (when (and (org-get-todo-state)
  ;;              (not (org-entry-get nil "CREATED")))
  ;;     (org-entry-put nil "CREATED" (format-time-string (cdr org-time-stamp-formats)))))
  ;; (add-hook 'org-after-todo-state-change-hook #'my/log-todo-creation-date)
  ;; src block indentation / editing / syntax highlighting
  (setq org-src-window-setup 'current-window
        org-src-strip-leading-and-trailing-blank-lines t)
  ;; to make gifs work
  ;; (setq org-format-latex-header (string-replace "{article}" "[tikz]{standalone}" org-format-latex-header))
  ;; (setq org-format-latex-header (string-replace "\\usepackage[usenames]{color}" "" org-format-latex-header))
  ;; (setq org-format-latex-header "\\documentclass[tikz]{standalone}")
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
  ;; [border=2pt] argument to documentclass may be desired but not for inline previews...
  (setq org-latex-preview-preamble "\\documentclass{standalone}")
  (setq org-latex-packages-alist (list "\\usepackage{\\string~/.emacs.d/common}")) ;; use my ~/.emacs.d/common.sty, its already in setup.org though
  ;; export to html using dvisvgm
  (setq org-html-with-latex 'dvisvgm)
  ;; not sure why org-mode 9.7-pre dev branch doesnt respect global visual line mode so imma add this for now
  (add-hook 'org-mode-hook 'visual-line-mode)
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
  ;; dont let org handle subscripts lol
  (setq org-export-with-sub-superscripts nil)
  ;; increase preview size
  ;; (plist-put org-latex-preview-appearance-options :scale 1.5)
  ;; (plist-put org-latex-preview-appearance-options :zoom 1.5)
  ;; dont limit the width of previews
  ;; (plist-put org-latex-preview-appearance-options :page-width nil)
  (require 'ox-html)
  ;; set to 1.0 to avoid some images being cut off, although that still happens, but less often
  ;; (plist-put org-html-latex-image-options :page-width nil)
  ;; (plist-put org-latex-preview-appearance-options :page-width nil)
  ;; lower the debounce value
  ;; (setq org-latex-preview-live-debounce 0.25)
  ;; (plist-put org-latex-preview-appearance-options :page-width 0.85)
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

  (setq org-publish-project-alist
        '(("orgfiles"
           :base-directory "~/brain/notes/"
           :base-extension "org"
           :publishing-directory "~/publish/"
           :publishing-function org-html-publish-to-html
           ;; :exclude "PrivatePage.org" ;; regexp
           :headline-levels 3
           :section-numbers nil
           :with-toc nil
           :recursive t
           :html-preamble t)
          ("images"
           :base-directory "~/brain/"
           :base-extension "jpg\\|gif\\|png\\|webp\\|jpeg\\|svg"
           :publishing-directory "~/publish/"
           :recursive t
           :publishing-function org-publish-attachment)
          ("website" :components ("orgfiles" "images"))))

  ;; also make org-special-edit respect tree-sitter modes
  (dolist (mapping major-mode-remap-alist)
    (let ((lang-name (car (split-string (symbol-name (car mapping)) "\\-"))))
      (add-to-list 'org-src-lang-modes (cons lang-name (concat lang-name "-ts")))))

  ;; centered latex previews in org https://www.reddit.com/r/emacs/comments/15vt0du/centering_latex_previews_in_org97/
  ;; (defun my/org-latex-preview-center (ov)
  ;;   (save-excursion
  ;;     (goto-char (overlay-start ov))
  ;;     (when-let* ((elem (org-element-context))
  ;;                 ((or (eq (org-element-type elem) 'latex-environment)
  ;;                      (string-match-p
  ;;                       "^\\\\\\[" (org-element-property :value elem))))
  ;;                 (img (overlay-get ov 'display))
  ;;                 (width (car-safe (image-display-size img)))
  ;;                 (offset (floor (- (window-max-chars-per-line) width) 2))
  ;;                 ((> offset 0)))
  ;;       (overlay-put ov 'before-string
  ;;                    (propertize
  ;;                     (make-string offset ?\ )
  ;;                     'face (org-latex-preview--face-around
  ;;                            (overlay-start ov) (overlay-end ov)))))))
  ;; (add-hook 'org-latex-preview-overlay-update-functions
  ;;           #'my/org-latex-preview-center)

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
  )

;; dont insert \\usepackage[inkscapelatex=false]{svg} when exporting docs with svg's
(defun ox-latex-disable-svg-handling ()
  (interactive)
  (setf (org-export-backend-feature-implementations (org-export-get-backend 'latex))
        (cl-remove-if (lambda (entry)
                        (equal (car entry) 'svg))
                      (org-export-backend-feature-implementations (org-export-get-backend 'latex)))))

;; run some python code from my org notes on shell startup
;; (add-hook 'python-shell-first-prompt-hook (lambda () (execute-files "python-code")))
;; i need those in library of babel on startup too
;; (lob-reload)

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

(defun today-entry ()
  "insert an entry for today, an action/todo/whatever and clock in"
  (interactive)
  (open-todays-file)
  (org-insert-heading-respect-content)
  (org-insert-time-stamp (current-time) t)
  (insert " ")
  (org-clock-in)
  (org-up-heading-safe)
  (end-of-line)
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

;; advice to only render links to files that fit the criterion defined by 'should-export-org-file' so as to not generate links to pages that dont exist
(defun my-org-link-advice (fn link desc info)
  "when exporting a file, it may contain links to other org files via id's, if a file being exported links to a note that is not tagged 'public', dont transcode the link to that note, just insert its description 'desc'"
  (let* ((filepath (pcase (org-element-property :type link)
                     ("blk" (plist-get (car (blk-find-by-id link)) :filepath))
                     ("denote" (denote-get-path-by-id (org-element-property :path link)))
                     (_ nil))))
    (if filepath
        (if (should-export-org-file filepath)
            (funcall fn link desc info)
          (format "%s" (or desc (org-element-property :path link))))
      (funcall fn link desc info))))
(advice-add #'org-html-link :around #'my-org-link-advice)
(advice-add #'org-hugo-link :around #'my-org-link-advice)

;; advice to make links in hugo markdown export properly
(defun my-blk-org-export-advice (fn link desc format)
  (if (blk-find-by-id link)
      (let* ((linked-file (plist-get (car (blk-find-by-id link)) :filepath))
             (desc (or desc link))
             (linked-file-no-ext (file-name-sans-extension (org-export-file-uri linked-file))))
        (message "testt %s" linked-file)
        (when (member format (list 'html 'md))
          (format "<a href=\"/%s/%s/\">%s</a>"
                  (org-export-dir-name path)
                  (downcase linked-file-no-ext)
                  desc))
        ;; ((eq format 'latex) (format "\\href{%s.tex}{%s}" linked-file-no-ext desc))
        )
    link))
(advice-add #'blk-org-export :override #'my-blk-org-export-advice)
;; overwrite the export function, for some slight modifications, to achieve the same effect as my-blk-org-export-advice
(with-eval-after-load 'denote
  (defun denote-link-ol-export (link description format)
    (let* ((path-id (denote-link--ol-resolve-link-to-target link :full-data))
           (path (file-relative-name (nth 0 path-id)))
           (id (nth 1 path-id))
           (search (nth 2 path-id))
           (anchor (file-name-sans-extension path))
           (path-no-ext (file-name-sans-extension (file-name-base path)))
           (desc (cond
                  (description)
                  (search (format "denote:%s::%s" id search))
                  (t (concat "denote:" id)))))
      (if (member format (list 'html 'md))
          (format "<a href=\"/%s/%s/\">%s</a>"
                  (org-export-dir-name path)
                  (downcase path-no-ext)
                  desc)
        link)
      ;; (cond
      ;;  ((eq format 'html)
      ;;   (if search
      ;;       (format "<a href=\"%s.html%s\">%s</a>" anchor search desc)
      ;;     (format "<a href=\"%s.html\">%s</a>" anchor desc)))
      ;;  ((eq format 'latex) (format "\\href{%s}{%s}" (replace-regexp-in-string "[\\{}$%&_#~^]" "\\\\\\&" path) desc))
      ;;  ((eq format 'texinfo) (format "@uref{%s,%s}" path desc))
      ;;  ((eq format 'ascii) (format "[%s] <denote:%s>" desc path))
      ;;  ((eq format 'md) (format "[%s](../%s.md)" desc path-no-ext))
      ;;  (t path))
      ))
  )

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

(defun my-org-agenda-files ()
  (denote-files-with-keyword "todo"))
(defun denote-files-with-keyword (keyword)
  (cl-remove-if-not
   (lambda (filepath)
     (member keyword (denote-extract-keywords-from-path filepath)))
   (denote-directory-files)))

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

(setq org-capture-templates (list))
(with-eval-after-load 'org
  (add-to-list 'org-capture-templates
               '("n"
                 "new note with prompts (with denote.el)"
                 plain
                 (file denote-last-path)
                 (function
                  (lambda ()
                    (denote-org-capture-with-prompts :title :keywords)))
                 :no-save t
                 :immediate-finish nil
                 :kill-buffer t
                 :jump-to-captured t))
  (add-to-list 'org-capture-templates
               '("t"
                 "todo"
                 entry
                 (file "/home/mahmooz/brain/notes/20240204T953231--agenda__todo.org")
                 "* TODO %?\nentered on %U\n %i\n %a"))
  (add-to-list 'org-capture-templates
               '("i"
                 "idea"
                 entry
                 (file "/home/mahmooz/brain/notes/20221106T180410--ideas.org")
                 "* %?\nentered on %U\n %i\n %a"))
  (add-to-list 'org-capture-templates
               '("q"
                 "question"
                 entry
                 (file "/home/mahmooz/brain/notes/20240226T224954--questions.org")
                 "* %?\nentered on %U\n %i\n %a"))
  )

;; enforce some default keywords for all org buffers (in a hacky way)
(defun my-org-collect-keywords-advice (orig-func &rest args)
  (let ((old-buffer (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring old-buffer)
      (insert "\n#+setupfile: ~/.emacs.d/setup.org\n#+include: ~/brain/private.org\n#+setupfile: ~/brain/private.org\n")
      (apply orig-func args))))
(advice-add #'org-collect-keywords :around #'my-org-collect-keywords-advice)

;; temporary workaround for captions breaking latex export
(advice-add 'org-export-get-caption :filter-return (lambda (_) nil))

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
  "export a node's file to both hugo md and pdf, if pdf-p is true, export to pdf, if html-p is true, export to html"
  (with-file-as-current-buffer
   file
   (when (plist-get kw :pdf-p)
     (my-org-to-pdf))
   (when (plist-get kw :html-p)
       (org-hugo-export-to-md))))

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
        (let ((nodes (files-linked-from-org-file node)))
          (dolist (other-node nodes)
            (when (should-export-org-file other-node) ;; to avoid jumping to nodes that arent for exporting anyway
              (when other-node (message (format "exporter jumping to: %s" other-node)))
              (setf exceptions (apply #'export-node-recursively (nconc (list other-node exceptions) kw))))))
        (when (and node (should-export-org-file node))
          (message (format "exporting: %s" node))
          (apply #'export-org-file node kw))
        exceptions)
    exceptions))

(defun export-all-org-files (&rest kw)
  "export all org mode files using `export-org-file', use `should-export-org-file' to check whether a file should be exported"
  (let ((exceptions))
    (let* ((grep-results (grep-org-dir *notes-dir* "#\\+hugo_section"))
           (files-to-export (mapcar (lambda (result) (plist-get result :filepath)) grep-results)))
      (dolist (file files-to-export)
        (setq exceptions (apply #'export-node-recursively (nconc (list file exceptions) kw)))
        (setq exceptions (push file exceptions))))))

(defun export-all-org-files-to-html-and-pdf ()
  (interactive)
  (export-all-org-files :html-p t :pdf-p t))

(defun export-current-buffer (&rest kw)
  "gets the node associated with the current buffer, exports it"
  (interactive)
  (apply #'export-org-file buffer-file-name kw))

(defun should-export-org-file (file)
  (org-export-dir-name file))
(defun org-export-dir-name (file)
  "whether the current org buffer should be exported"
  (car
   (cdar
    (with-file-as-current-buffer
     file
     (org-collect-keywords '("hugo_section"))))))
;; (member "public" (mapcar #'substring-no-properties (org-get-tags)))))

;; (defun map-org-dir-elements (regex dir elm-type fn)
;;   "look for lines containing `regex' that contain an org element of type `elm-type', run `fn' at the point where the element is"
;;   (interactive)
;;   (let ((positions (grep-org-dir regex dir)))
;;     (dolist (position positions)
;;       (let ((file (car position))
;;             (line (cadr position)))
;;         (with-file-as-current-buffer
;;          file
;;          (goto-line line)
;;          (let ((elm (org-element-at-point)))
;;            (when (eq (org-element-type elm) elm-type)
;;              (funcall fn))))))))

;; (defun notes-execute-marked-src-block (rgx)
;;   (map-org-dir-elements rgx *notes-dir* 'src-block
;;                         (lambda ()
;;                           (message "running code block in file %s" (buffer-file-name))
;;                           (org-ctrl-c-ctrl-c))))

(defun grep-org-dir (dir regex)
  (blk-grep blk-grepper
            (list (list :anchor-regex regex :src-id-function 'identity :glob "*.org"))
            (list dir)))

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
  (or (org-element-property property block)
      (alist-get
       property
       (org-babel-parse-header-arguments
        (org-element-property :parameters block)))))

;; advice to only render links to files that fit the criterion defined by 'should-export-org-file' so as to not generate links to pages that dont exist
(defun my-org-link-advice (fn link desc info)
  "when exporting a file, it may contain links to other org files via id's, if a file being exported links to a note that is not tagged 'public', dont transcode the link to that note, just insert its description 'desc'"
  (let* ((filepath (pcase (org-element-property :type link)
                     ("blk" (plist-get (car (blk-find-by-id link)) :filepath))
                     ("denote" (denote-get-path-by-id (org-element-property :path link)))
                     (_ nil))))
    (if filepath
        (if (should-export-org-file filepath)
            (funcall fn link desc info)
          (format "%s" (or desc (org-element-property :path link))))
      (funcall fn link desc info))))
(advice-add #'org-html-link :around #'my-org-link-advice)
(advice-add #'org-hugo-link :around #'my-org-link-advice)

;; handle some custom blocks i've defined
(defun my-org-latex-special-block-advice (fn special-block contents info)
  (let ((type (org-element-property :type special-block)))
    (when (string= type "my_example") (setq type "example"))
    (if (member type (list "definition"
                           "theorem"
                           "problem"
                           "proposition"
                           "notation"
                           "solution"
                           "example"
                           "corollary"
                           "task"
                           "thought"
                           "question"
                           "lemma"
                           "note"
                           "result"
                           "claim"))
        (progn
          (let ((title (or (org-block-property :defines special-block)
                           (org-block-property :title special-block)
                           ""))
                (citation (org-block-citation-string special-block)))
            (when citation
              ;; delete the citation, we insert it ourselves later
              (setq contents
                    (with-temp-buffer
                      (insert contents)
                      (goto-char (point-max))
                      (previous-line)
                      (goto-char (pos-bol))
                      (kill-line)
                      (buffer-string))))
            (concat (format "\\begin{myenv}{%s}[%s]%s\n" type title ;; note that title can be broken into multiple lines with \\ which may also allow for multiple titles i guess
                            (if citation (format "[%s]" citation) ""))
                    contents
                    (format "\\end{myenv}"))))
      (funcall fn special-block contents info))))
(advice-add #'org-latex-special-block :around #'my-org-latex-special-block-advice)

(defun org-block-citation-string (&optional block)
  (save-excursion
    (let* ((block (or block (org-element-at-point)))
           (end (org-element-property :end block)))
      (goto-char (1- end))
      (previous-line)
      (goto-char (pos-eol))
      (let* ((context
              (org-element-lineage
               (org-element-context)
               '(citation citation-reference)
               t))
             (type (org-element-type context))
             (value (org-element-property :value context)))
        (when context
          (let ((contents (buffer-substring (org-element-begin context)
                                            (org-element-end context))))
            (org-export-string-as contents 'latex t)))))))

(provide 'setup-org)