;; org mode configuration and setup

;; tecosaur's org-mode version
(use-package org
  :elpaca (org :remotes ("tecosaur" :repo "https://git.tecosaur.net/tec/org-mode.git" :branch "dev")
               :files (:defaults "etc")))

(defvar *latex-previews-enabled-p* t "whether latex previews for org mode are enabled for the current session")

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

(defun compile-latex-file (path)
  (start-process-shell-command "latex" "latex" (format "%s -shell-escape -output-directory=%s %s" org-latex-compiler (file-truename (get-latex-cache-dir-path)) path)))

(defun compile-current-document ()
  "compile the current latex document being edited"
  (interactive)
  (compile-latex-file (buffer-file-name)))

(defun open-current-document ()
  "open the pdf of the current latex document that was generated"
  (interactive)
  (find-file-other-window (concat (get-latex-cache-dir-path) (current-filename) ".pdf")))
(defun open-current-document-this-window ()
  (interactive)
  (let ((pdf-file (concat (get-latex-cache-dir-path) (current-filename) ".pdf")))
    (if (file-exists-p pdf-file)
        (find-file pdf-file)
      (message "pdf file hasnt been generated"))))

;; cache for orgmode links, requires pre-isntallation of gcc/clang
;; (use-package org-roam
;;   :elpaca (org-roam :type git :repo "mahmoodsheikh36/org-roam")
;;   :custom
;;   ;; (org-roam-directory *brain-dir*)
;;   (org-roam-directory (file-truename "~/brain2/notes/"))
;;   (org-roam-completion-everywhere t)
;;   :config
;;   ;; (setq org-roam-node-display-template "${title:*} ${tags:*}")
;;   (org-roam-db-autosync-mode 1)
;;   (require 'org-roam-export)
;;   (require 'org-roam-protocol)
;;   (global-set-key (kbd "C-c r f") 'org-roam-node-find)
;;   (global-set-key (kbd "C-c r g") 'org-roam-graph)
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
  ;; save the clock history across sessions
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)
  ;; log state/schedule/deadline changes
  (setq org-log-done 'time)
  (setq org-log-reschedule 'time)
  (setq org-log-redeadline 'time)
  ;; show images when opening a file.
  (setq org-startup-with-inline-images nil)
  ;; show images after evaluating code blocks.
  (add-hook 'org-babel-after-execute-hook (lambda ()
                                            (interactive)
                                            (clear-image-cache)
                                            (org-display-inline-images)))
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

  ;; enter insert state after invoking org-capture
  (add-hook 'org-capture-mode-hook 'evil-insert-state)

  ;; compile org docs to pdfs and put them in cache dir
  (defun latex-out-file ()
    (concat (file-truename (get-latex-cache-dir-path)) (current-filename) ".tex"))
  (defun pdf-out-file ()
    (concat (file-truename (get-latex-cache-dir-path)) (current-filename) ".pdf"))
  (defun org-to-pdf ()
    (interactive)
    (let ((outfile (latex-out-file)))
      (call-process-shell-command (format "rm %s*%s*" (file-truename (get-latex-cache-dir-path)) (current-filename)))
      (org-export-to-file 'latex outfile
        nil nil nil nil nil nil)
      (compile-latex-file outfile)))
  ;; allow usage of #+BIND in latex exports
  (setq org-export-allow-bind-keywords t)
  ;; decrease image size in latex exports
  ;; (setq org-latex-image-default-scale "2.0")
  ;; disable images from being scaled/their dimensions being changed
  ;; (setq org-latex-image-default-width "2.0")
  ;; enable latex snippets in org mode
  (defun my-org-latex-yas ()
    "Activate org and LaTeX yas expansion in org-mode buffers."
    (yas-minor-mode)
    (yas-activate-extra-mode 'latex-mode))
  (add-hook 'org-mode-hook #'my-org-latex-yas)
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
          "CANCELLED(C@)" ;; for backward compatibility
          ))
  ;; filter out entries with tag "ignore"
  (setq org-agenda-tag-filter-preset '("-ignore"))
  ;; use listings package for latex code blocks
  (setq org-latex-src-block-backend 'listings)
  ;; timestamp with seconds
  (setq org-time-stamp-formats '("<%Y-%m-%d %a>" . "<%Y-%m-%d %a %H:%M:%S>"))

  ;; give svg's a proper width when exporting with dvisvgm
  ;; (with-eval-after-load 'ox-html
  ;;   (setq org-html-head
  ;;         (replace-regexp-in-string
  ;;          ".org-svg { width: 90%; }"
  ;;          ".org-svg { width: auto; }"
  ;;          org-html-style-default)))
  ;; enable <s code block snippet
  ;; (require 'org-src)
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
  ;; dunno why \def\pgfsysdriver is needed (i think for htlatex)... gonna override the variable cuz that causes errors
  ;; (setq org-babel-latex-preamble
  ;;       (lambda (_)
  ;;         "\\documentclass[preview]{standalone}"))
  ;; to make gifs work
  ;; (setq org-format-latex-header (string-replace "{article}" "[tikz]{standalone}" org-format-latex-header))
  ;; (setq org-format-latex-header (string-replace "\\usepackage[usenames]{color}" "" org-format-latex-header))
  ;; (setq org-format-latex-header "\\documentclass[tikz]{standalone}")
  ;; i think this is irrelevant at this point
  (defun space-x-with-latex-header-hack ()
    (interactive)
    (let ((org-format-latex-header "\\documentclass[tikz]{standalone}"))
      (org-ctrl-c-ctrl-c)))
  ;; make org babel use dvisvgm instead of inkscape for pdf->svg, way faster and has many more advtanges over inkscape
  (setq org-babel-latex-pdf-svg-process "dvisvgm --pdf %f -o %O")
  ;; latex syntax highlighting in org mode (and more)
  ;; (setq org-highlight-latex-and-related nil)
  (setq org-highlight-latex-and-related '(latex))
  ;; (setq org-highlight-latex-and-related '(native latex script entities))
  ;; disable org-mode's mathjax because my blog's code uses another version
  (setq org-html-mathjax-template "")
  (setq org-html-mathjax-options '())
  (setq org-babel-default-header-args:latex
        '((:results . "file graphics")
          ;; (:exports . "results")
          ;; (:fit . t)
          ;; (:imagemagick . t)
          ;; (:eval . "no-export")
          ;; (:headers . ("\\usepackage{\\string~/.emacs.d/common}"))
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
  (setq org-latex-images-centered nil)
  (setq org-latex-tables-centered nil)
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

  ;; dynamic org-agenda
  ;; (defun buffer-contains-todo ()
  ;;   "check if the buffer contains a TODO entry"
  ;;   (org-element-map
  ;;       (org-element-parse-buffer 'headline)
  ;;       'headline
  ;;     (lambda (h)
  ;;       (eq (org-element-property :todo-type h) 'todo))
  ;;     nil 'first-match))
  ;; (add-hook 'before-save-hook #'update-todo-tag)
  ;; (defun update-todo-tag ()
  ;;   "remove/add the todo tag to the buffer by checking whether it contains a TODO entry"
  ;;   (let ((kill-ring)) ;; keep kill ring, dont modify it
  ;;     (when (and (not (active-minibuffer-window))
  ;;                (is-buffer-roam-note))
  ;;       (save-excursion
  ;;         (goto-char (point-min))
  ;;         (if (buffer-contains-todo)
  ;;             (org-roam-tag-add '("todo"))
  ;;           (ignore-errors (org-roam-tag-remove '("todo")))))))
  ;;   (agenda-files-update))
  ;; (defun is-buffer-roam-note ()
  ;;   "return non-nil if the currently visited buffer is a note."
  ;;   (and buffer-file-name
  ;;        (string-prefix-p
  ;;         (expand-file-name (file-name-as-directory org-roam-directory))
  ;;         (file-name-directory buffer-file-name))))
  ;; (setq org-agenda-files (roam-files-with-tag "todo"))
  ;; (defun agenda-files-update (&rest _)
  ;;   "Update the value of `org-agenda-files'."
  ;;   (setq org-agenda-files (roam-files-with-tag "todo")))
  ;; (advice-add 'org-agenda :before #'agenda-files-update)
  ;; (advice-add 'org-todo-list :before #'agenda-files-update)
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
  ;; enable it everywhere possible
  (setq org-latex-preview-live '(block inline edit-special))
  (setq org-latex-preview-preamble "\\documentclass{article}\n[DEFAULT-PACKAGES]\n[PACKAGES]\n\\usepackage{xcolor}\n\\usepackage{\\string\~/.emacs.d/common}") ;; use my ~/.emacs.d/common.sty
  ;; export to html using dvisvgm/mathjax/whatever
  (setq org-html-with-latex 'dvisvgm)
  ;; not sure why org-mode 9.7-pre dev branch doesnt respect global visual line mode so imma add this for now
  (add-hook 'org-mode-hook 'visual-line-mode)
  ;; use dvisvgm instead of dvipng
  ;; (setq org-latex-preview-process-default 'dvisvgm)
  ;; dont export headlines with tags
  (setq org-export-with-tags nil)
  ;; allow characters as list modifiers in org mode
  (setq org-list-allow-alphabetical t)
  ;; also number equations
  (setq org-latex-preview-numbered t)
  ;; ;; tell org latex previews to use lualatex, its better (i need it for some tikz functionalities)
  ;; (setq org-latex-compiler "lualatex")
  ;; (setq org-latex-compiler "pdflatex")
  ;; ;; make dvisvgm preview use lualatex
  ;; (let ((pos (assoc 'dvisvgm org-latex-preview-process-alist)))
  ;;   (plist-put (cdr pos) :programs '("lualatex" "dvisvgm")))
  ;; make org-agenda open up in the current window
  (setq org-agenda-window-setup 'current-window)
  ;; dont prompt for downloading remote files on export
  (setq org-resource-download-policy nil)
  ;; enable eval: keyword in local variables
  ;; (setq enable-local-eval t)
  ;; dont number headers on exports
  (setq org-export-with-section-numbers nil)
  (setq org-use-property-inheritance t)
  ;; dont override my labels, silly org
  (setq org-latex-prefer-user-labels t)
  ;; dont let org handle subscripts lol
  (setq org-export-with-sub-superscripts nil)
  ;; increase preview size
  ;; (plist-put org-latex-preview-appearance-options :scale 1.5)
  ;; (plist-put org-latex-preview-appearance-options :zoom 1.5)
  ;; dont limit the width of previews
  ;; (plist-put org-latex-preview-appearance-options :page-width nil)
  ;; (plist-put org-html-latex-image-options :page-width nil)
  ;; lower the debounce value
  ;; (setq org-latex-preview-live-debounce 0.25)
  (plist-put org-latex-preview-appearance-options :page-width 1.0)

  ;; make org not evaluate code blocks on exporting
  (add-to-list 'org-babel-default-header-args '(:eval . "no-export"))
  (add-to-list 'org-babel-default-inline-header-args '(:eval . "no-export"))

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
  )

;; disable ox-hugo relative path exports
;; (defun non-relative-path (obj)
;;   "return non-relative path for hugo"
;;   (interactive)
;;   (if (eq (type-of obj) 'string)
;;       (progn
;;         ;; if we're here, obj is a link to a file, file-truename can be used to get the full path of it
;;         ;; (message (format "filename: %s" obj))
;;         (file-name-nondirectory obj))
;;     obj))
;; (advice-add 'org-export-resolve-id-link :filter-return #'non-relative-path)

(defun all-roam-files ()
  "return a list of all files in the org-roam database"
  (seq-uniq
   (seq-map
    #'car
    (org-roam-db-query
     `(:select file :from nodes)))))

(defun roam-files-with-tag (tag-name)
  "Return a list of note files containing a specific tag.";
  (seq-uniq
   (seq-map
    #'car
    (org-roam-db-query
     `(:select [nodes:file]
               :from tags
               :left-join nodes
               :on (= tags:node-id nodes:id)
               :where (like tag ,tag-name))))))


;; (defun go-through-all-roam-files (callback)
;;   "run a callback function on each file in the org-roam database"
;;   (dolist (file (all-roam-files))
;;     (if (not (eq callback nil))
;;         (with-current-buffer (or (find-buffer-visiting file) (find-file-noselect file))
;;           (if (is-buffer-roam-note)
;;               (funcall callback))))))

(defun go-through-roam-files-with-tag (tag-name callback)
  "run a callback function on each file tagged with tag-name"
  (dolist (file (roam-files-with-tag tag-name))
    (with-current-buffer (or (find-buffer-visiting file) (find-file-noselect file))
      (when (is-buffer-roam-note)
        (funcall callback)
        (kill-this-buffer)
        ))))

(defun execute-code-files ()
  "execute files tagged with 'code'"
  (interactive)
  (go-through-roam-files-with-tag
   "code"
   (lambda ()
     (condition-case err
         (org-babel-execute-buffer)
       (error (message "got error %s while executing %s" err (buffer-file-name)))))))

(defun execute-files (tag)
  "execute files tagged with 'code'"
  (interactive)
  (go-through-roam-files-with-tag
   tag
   (lambda ()
     (condition-case err
         (org-babel-execute-buffer)
       (error (message "got error %s while executing %s" err (buffer-file-name)))))))

(defun lob-reload ()
  "load files tagged with 'code' into the org babel library (lob - library of babel)"
  (interactive)
  (go-through-roam-files-with-tag
   "code"
   (lambda ()
     (org-babel-lob-ingest (buffer-file-name)))))

;; most/all of my code files are lisp, load them with sly/slime
(add-hook 'sly-connected-hook (lambda () (execute-files "lisp-code")))
;; run some python code from my org notes on shell startup
;; (add-hook 'python-shell-first-prompt-hook (lambda () (execute-files "python-code")))
;; i need those in library of babel on startup too
;; (lob-reload)

;; (defun xenops-prerender ()
;;   "prerender latex blocks in roam files"
;;   (interactive)
;;   (go-through-roam-files-with-tag
;;    "math"
;;    (lambda ()
;;      (message "processing math file %s" (buffer-file-name)))))

;; (defun run-all-code-blocks ()
;;   "run code blocks in all org-roam files"
;;   (interactive)
;;   (go-through-all-roam-files
;;    (lambda ()
;;      (message "processing file %s" (buffer-file-name))
;;      (org-babel-execute-buffer))))

;; (go-through-roam-files-with-tag "math" (lambda () (message buffer-file-name)))
;; (defun publicize-files ()
;;   (interactive)
;;   (go-through-roam-files-with-tag
;;    "computer-science"
;;    (lambda ()
;;      (org-roam-tag-add '("public"))
;;      (save-buffer))))

(defun buffer-contains-substring (string)
  "check if the current buffer contains a specific string"
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (not (eq nil (search-forward string nil t))))))

(defun buffer-contains-math ()
  "check if the current buffer contains any math equations (latex blocks)"
  (or
   (buffer-contains-substring "$")
   (buffer-contains-substring "\\(")
   (buffer-contains-substring "\\[")))

(defmacro save-buffer-modified-p (&rest body)
  "Eval BODY without affected buffer modification status"
  `(let ((buffer-modified (buffer-modified-p))
         (buffer-undo-list t))
     (unwind-protect
         ,@body
       (set-buffer-modified-p buffer-modified))))

(defun update-math-file ()
  "add/remove the math tag to the file"
  (let ((kill-ring)  ;; keep kill ring, dont modify it
        (buffer-undo-list)) ;; keep the undo "ring" too, doesnt work tho, hmmmm
    (when (and (not (active-minibuffer-window))
               (is-buffer-roam-note))
      (save-excursion
        (goto-char (point-min))
        (if (buffer-contains-math)
            (org-roam-tag-add '("math"))
          (ignore-errors (org-roam-tag-remove '("math"))))))))
;; (add-hook 'before-save-hook #'update-math-file)

(defun find-math-files (basedir)
  "find all org files in a directory that are math files and tag them with 'math' tag"
  (interactive)
  (dolist (file (directory-files-recursively basedir ".*\\.org$"))
    (ignore-errors (with-current-buffer (or (find-buffer-visiting file) (find-file-noselect file))
                     (beginning-of-buffer)
                     (org-id-get-create)
                     (ignore-errors (update-math-file))
                     (save-buffer)))))

(defun update-math-files ()
  "go through all roam files and check each for math formulas and update the math tag"
  (dolist (file (all-roam-files))
    (with-current-buffer (or (find-buffer-visiting file) (find-file-noselect file))
      (update-math-file)
      (save-buffer))))

;; (defun buffer-contains-code ()
;;   "check if the buffer contains an org-babel source block"
;;   (interactive)
;;   (org-element-map
;;       (org-element-parse-buffer 'element)
;;       'src-block
;;     (lambda (h) t)
;;     ;; (or (eq (org-element-property :todo-type h)
;;     ;;         'todo)
;;     ;;     (eq (org-element-property :todo-type h)
;;     ;;         'done)))
;;     nil 'first-match))

;; (defun update-code-file ()
;;   "add/remove the code tag to the file"
;;   (when (and (not (active-minibuffer-window))
;;              (is-buffer-roam-note))
;;     (save-excursion
;;       (goto-char (point-min))
;;       (if (buffer-contains-code)
;;           (org-roam-tag-add '("math"))
;;         (org-roam-tag-remove '("math"))))))
;; (add-hook 'before-save-hook #'update-math-file)

;; (defun find-code-files (basedir)
;;   "find all org files in a directory that contain source blocks and tag them with 'code' tag"
;;   (interactive)
;;   (dolist (file (directory-files-recursively basedir ".*\\.org$"))
;;     (ignore-errors (with-current-buffer (or (find-buffer-visiting file) (find-file-noselect file))
;;       (beginning-of-buffer)
;;       (ignore-errors (update-code-file))
;;       (save-buffer)))))

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
          ;; (org-id-get-create)
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

(defun push-blog-github ()
  (interactive)
  (execute-kbd-macro (read-kbd-macro "SPC d g SPC s e M-r reexport RET RET")))

;; make links like [[id::blockname]] work, need to rebuild database after defining the advice using org-roam-db-clear-all and then org-roam-db-sync
;; (defun +org--follow-search-string-a (fn link &optional arg)
;;   "Support ::SEARCH syntax for id::name links.
;; note that this doesnt work for exports"
;;   (save-match-data
;;     (cl-destructuring-bind (id &optional search)
;;         (split-string link "::")
;;       (prog1 (funcall fn id arg)
;;         (cond ((null search))
;;               ((string-match-p "\\`[0-9]+\\'" search)
;;                ;; Move N lines after the ID (in case it's a heading), instead
;;                ;; of the start of the buffer.
;;                (forward-line (string-to-number option)))
;;               ((string-match "^/\\([^/]+\\)/$" search)
;;                (let ((match (match-string 1 search)))
;;                  (save-excursion (org-link-search search))
;;                  ;; `org-link-search' only reveals matches. Moving the point
;;                  ;; to the first match after point is a sensible change.
;;                  (when (re-search-forward match)
;;                    (goto-char (match-beginning 0)))))
;;               ((org-link-search search)))))))
;; (advice-add 'org-id-open :around #'+org--follow-search-string-a)
;; (advice-add 'org-roam-id-open :around #'+org--follow-search-string-a)

;; TODO: add latex auto-completion to org-mode, requires auctex
;; (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point)))
;; (TeX--completion-at-point
;;  t
;;  LaTeX--arguments-completion-at-point)

;; temporary fix for latex preview exports in html
;; (plist-put org-html-latex-image-options :inline "svg")
;; temporary fix for ox-hugo with new org latex preview system
;; (advice-add 'org-blackfriday--update-ltximg-path
;;             :around
;;             (lambda (orig-fn html-string)
;;               (if (plist-get org-html-latex-image-options :inline)
;;                   html-string
;;                 (funcall orig-fn html-string)))
;;             '((name . inline-image-workaround)))

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

;; automatic recursive exporting of linked notes
(defun nodes-linked-from-node (node)
  "return list of roam nodes linked to from node with node-id";
  (when node
    (mapcar
     #'org-roam-node-from-id
     (mapcar
      #'car
      (org-roam-db-query
       [:select :distinct [dest]
                :from links
                :where (= source $s1)
                :and (= type "id")]
       (org-roam-node-id node))))))

(defun nodes-linked-from-node-file (node)
  (when node
    (mapcar
     #'org-roam-node-from-id
     (mapcar
      #'car
      (org-roam-db-query
       [:select :distinct [links:dest]
                :from links
                :left-join nodes
                :on (= links:source nodes:id)
                :left-join files
                :on (= files:file nodes:file)
                :where (= nodes:file $s1)]
       (org-roam-node-file node))))))

(defun this-buffer-roam-node ()
  "get the roam-node of the current buffer"
  (save-excursion
    (goto-char 0)
    (when (org-id-get)
      (org-roam-node-from-id (org-id-get)))))

(defun nodes-linked-from-this-node ()
  "nodes linked to from this node"
  (let ((node (this-buffer-roam-node)))
    (nodes-linked-from-node node)))

(defun nodes-linked-from-this-file ()
  "nodes linked to from this node"
  (let ((node (this-buffer-roam-node)))
    (nodes-linked-from-node-file node)))

;; old one
;; (defun export-node (node)
;;   "export a node's file to both hugo md and pdf"
;;   (condition-case err
;;       (let ((org-startup-with-latex-preview nil)
;;             (file (org-roam-node-file node)))
;;         (with-current-buffer (or (find-buffer-visiting file) (find-file-noselect file))
;;           ;; (org-to-pdf)
;;           (org-hugo-export-to-md)))
;;     (error
;;      (error (format "export-node failed %s, not retrying, err: %s" (org-roam-node-file node) err))
;;      ;; (org-latex-preview--clear-preamble-cache)
;;      ;; (org-latex-preview-clear-cache)
;;      ;; (export-node node)
;;      )))

(defun export-node (node &rest kw)
  "export a node's file to both hugo md and pdf, if pdf-p is true, export to pdf, if html-p is true, export to html"
  (let ((file (org-roam-node-file node)))
    (with-current-buffer (or (find-buffer-visiting file) (find-file-noselect file))
      (when (plist-get kw :pdf-p)
        (org-to-pdf))
      (when (plist-get kw :html-p)
        (let ((org-hugo-section (or (node-hugo-section node) "index")))
          (org-hugo-export-to-md))))))

(defun export-current-buffer (&rest kw)
  "gets the node associated with the current buffer, exports it"
  (interactive)
  (let ((node (this-buffer-roam-node)))
    (when node
      (apply #'export-node node kw))))

(defun export-current-buffer-recursively ()
  "gets the node associated with the current buffer, exports it with export-node-recursively, see its docstring"
  (interactive)
  (let ((node (this-buffer-roam-node)))
    (when node
      (export-node-recursively node))))

(defun export-node-recursively (node &optional exceptions)
  "export node, export all nodes/files it links to, and all files linked from those and so on, basically we're exporting the connected subgraph the node exists in, `exceptions' is used for recursion to keep a record of exported nodes"
  ;; (message "%s" exceptions)
  (if (and node
           (when (not (cl-find node exceptions
                               :test (lambda (node1 node2)
                                       (string= (org-roam-node-file node1)
                                                (org-roam-node-file node2)))))))
      (progn
        (push node exceptions)
        (let ((nodes (nodes-linked-from-node-file node)))
          (dolist (other-node nodes)
            (when other-node (message (format "exporter jumping to: %s" (org-roam-node-file other-node))))
            (setf exceptions (export-node-recursively other-node exceptions))))
        (when (and node (should-export-node node))
          (message (format "exporting: %s" (org-roam-node-file node)))
          (export-node node :html-p t))
        exceptions)
    exceptions))

(defun should-export-node (node)
  "whether an org note should be exported"
  (or (member "public" (org-roam-node-tags node))
      (member "public-archive" (org-roam-node-tags node))))

(defun node-hugo-section (node)
  "the section the file should be placed into on hugo export, see HUGO_SECTION or whatever"
  (cond ((member "public" (org-roam-node-tags node)) "blog")
        ((member "public-archive" (org-roam-node-tags node)) "index")))

;; (defun my-org-link-advice (fn link desc info)
;;   "when exporting a file, it may contain links to other org files via id's, if a file being exported links to a note that is not tagged 'public', dont transcode the link to that note, just insert its description 'desc'"
;;   (let* ((link-type (org-element-property :type link))
;;          (link-path (org-element-property :path link))
;;          (is-id-link (string= link-type "id")))
;;     (if is-id-link
;;         (condition-case err ;; handle error when org-roam cannot find link in database
;;             (let* ((node (org-roam-node-from-id link-path))
;;                    (tags (org-roam-node-tags node)))
;;               (if (and (should-export-node node) ;; if note is public export as usual, otherwise dont export it as link but just as text
;;                        (not (string-match-p "::" link-path))) ;; if link isnt of form [[id::block]], dont export it as link, we cant handle those yet
;;                   (funcall fn link desc info)
;;                 (format "<b>%s</b>" desc)))
;;           (error (message "org-roam couldnt find link %s, error was: %s" link-path err)
;;                  (format "<b>%s</b>" desc))) ;; even when we cant find it in the database we still render it
;;       (funcall fn link desc info))))
;; (advice-add #'org-html-link :around #'my-org-link-advice)
;; (advice-add #'org-hugo-link :around #'my-org-link-advice)

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
    ;; (format-time-string "<%Y-%m-%d>" (file-modif-time myfile))))
    ;;(format-time-string "<%Y-%m-%d>" (file-creation-time myfile))))
    (format-time-string "<%Y-%m-%d>"
                        (file-creation-time-using-git myfile))))
(advice-add #'org-export-get-date :around #'my-org-date-advice)

(defun export-all-public ()
  "export nodes with tag 'public'"
  (interactive)
  (let ((exceptions))
    (go-through-roam-files-with-tag "public" #'export-all-public-helper)
    (go-through-roam-files-with-tag "public-archive" #'export-all-public-helper)))

(defun export-all-public-helper ()
  "a helper for `export-all-public', the variable `exceptions' is dynamically bound"
  ;; (message "%s" exceptions)
  (setf exceptions
        (export-node-recursively (org-roam-node-from-id (org-id-get)) exceptions)))

;; org-special-edit with lsp?, laggy af
;; (defun org-babel-edit-prep:python (babel-info)
;;   (setq-local buffer-file-name (->> babel-info caddr (alist-get :tangle)))
;;   (lsp))

;; org html title of blocks, unfinished
(defun my-org-block-advice (fn special-block contents info)
  "i use properties like :title <block title>, make those available in html output too"
  (let ((args (org-babel-parse-header-arguments
               (org-element-property :parameters
                                     special-block))))
    (if args
        (progn
          (let ((mytitle (alist-get :title args)))
            ;; this is temporary, it replaces all html attributes instead of appending
            (setf (plist-get (plist-get special-block 'special-block) :attr_html)
                  (list (format ":data-title %s" mytitle)))
            (funcall fn special-block contents info)))
      (funcall fn special-block contents info))))
;; (advice-add #'org-html-special-block :around #'my-org-block-advice)
;; (advice-add #'org-hugo-special-block :around #'my-org-block-advice)

;; (defun org-block-at-point ()
;;   (let ((blk (org-element-at-point)))
;;     (if (not (org-element-property :name blk)) ;; a block must have a :name
;;         (setq blk (org-element-parent blk)))
;;     (if (org-element-property :name blk)
;;         blk
;;       nil)))

;; (defun execute-src-block-with-dependencies (&optional arg info params executor-type)
;;   (save-excursion
;;     (let ((src-block-location (nth 5 info)))
;;       )
;;     (message "%s" info)))
;; (defun org-babel-execute-src-block-advice ()
;;   (message "hi"))
;; (advice-add #'org-babel-execute-src-block :before #'execute-src-block-with-dependencies)

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
;; (add-hook 'org-latex-preview-update-overlay-functions
;;          #'my/org-latex-preview-center)

(defun timestamp-midnight (timestamp)
  (let ((decoded (decode-time timestamp)))
    (setf (nth 0 decoded) 0)
    (setf (nth 1 decoded) 0)
    (setf (nth 2 decoded) 0)
    (apply #'encode-time decoded)))

(defun org-agenda-skip-if-scheduled-earlier ()
  "If this function returns nil, the current match should not be skipped.
Otherwise, the function must return a position from where the search
should be continued."
  (ignore-errors
    (let ((subtree-end (save-excursion (org-end-of-subtree t)))
          (scheduled-seconds (org-time-string-to-seconds (org-entry-get nil "SCHEDULED")))
          (now (time-to-seconds (timestamp-midnight (current-time)))))
      (and scheduled-seconds
           (>= scheduled-seconds now)
           subtree-end))))

(with-eval-after-load 'org-agenda
  (defvar prot-org-custom-daily-agenda
    `((agenda "" ((org-agenda-span 1)
                  (org-deadline-warning-days 0)
                  (org-scheduled-past-days 0)
                  ;; We don't need the `org-agenda-date-today'
                  ;; highlight because that only has a practical
                  ;; utility in multi-day views.
                  (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                  (org-agenda-format-date "%A %-e %B %Y")
                  (org-agenda-overriding-header "\ntoday's agenda")))
      (agenda "" ((org-agenda-start-on-weekday nil)
                  (org-agenda-start-day "+1d")
                  (org-agenda-span 14)
                  (org-agenda-show-all-dates nil)
                  (org-deadline-warning-days 0)
                  (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                  (org-agenda-overriding-header "\nnext 2 weeks")))
      (agenda "" ((org-agenda-overriding-header "overdue")
                  ;; (org-agenda-entry-types '(:deadline :scheduled))
                  (org-scheduled-past-days 10000)
                  (org-deadline-past-days 10000)
                  (org-agenda-span 1)
                  (org-agenda-start-on-weekday nil)
                  (org-agenda-show-all-dates nil)
                  (org-agenda-skip-function 'org-agenda-skip-if-scheduled-earlier)))
      (agenda "" ((org-agenda-time-grid nil)
                  (org-agenda-start-on-weekday nil)
                  ;; we don't want to replicate the previous section's
                  ;; three days, so we start counting from the day after.
                  (org-agenda-start-day "+4d")
                  (org-agenda-span 30)
                  (org-agenda-show-all-dates nil)
                  (org-deadline-warning-days 0)
                  (org-agenda-entry-types '(:deadline))
                  (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                  (org-agenda-overriding-header "\nupcoming deadlines (+14d)"))))
    "Custom agenda for use in `org-agenda-custom-commands'.")


  (setq org-agenda-custom-commands
        `(("A" "Daily agenda and top priority tasks"
           ,prot-org-custom-daily-agenda)
          ("P" "Plain text daily agenda and top priorities"
           ,prot-org-custom-daily-agenda
           ((org-agenda-with-colors nil)
            (org-agenda-prefix-format "%t %s")
            (org-agenda-current-time-string ,(car (last org-agenda-time-grid)))
            (org-agenda-fontify-priorities nil)
            (org-agenda-remove-tags t))
           ("agenda.txt")))))

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
                 "* TODO %?\nentered on %U\n %i\n %a")))

;; enforce some default keywords for all org buffers (in a hacky way)
(defun my-org-collect-keywords-advice (orig-func &rest args)
  (let ((old-buffer (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring old-buffer)
      (insert "\n#+setupfile: ~/.emacs.d/setup.org\n#+include: ~/brain/private.org\n#+setupfile: ~/brain/private.org\n")
      (apply orig-func args))))
(advice-add #'org-collect-keywords :around #'my-org-collect-keywords-advice)

;; link to arbitrary blocks in org
(with-eval-after-load 'org
  (org-link-set-parameters "blk"
                           :follow #'org-blk-open
                           :export #'org-blk-export))
                         ;; :store #'org-blk-store-link)

(defun grep-brain (rgx)
  "grep `*brain-dir*' for the regex `rgx'"
  (let* ((cmd (format "rg -e '%s' -g '*.org' '%s' --no-heading" rgx *brain-dir*))
         (output (shell-command-to-string cmd))
         (files (mapcar (lambda (line) (car (split-string line ":"))) (split-string output "\n"))))
    (car files)))

(defun org-blk-open (link _)
  "open the file containing a block with the name `link'"
  (find-file (grep-brain (regexp-quote (format "#+name: %s" link)))))
  ;; (shell-command-to-string "find . -name \"*.org\" -exec grep '^#+name: blk' {} \;"))

(defun org-blk-export (path _)
  (message "got: %s" path))

(require 'org-attach)

(provide 'setup-org)