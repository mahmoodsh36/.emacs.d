;; (use-package dired+
;;   :quelpa (dired+ :fetcher url :url "https://www.emacswiki.org/emacs/download/dired+.el")
;;   :defer 1
;;   :init
;;   (setq diredp-hide-details-initially-flag nil)
;;   (setq diredp-hide-details-propagate-flag nil)
;;   :config
;;   (diredp-toggle-find-file-reuse-dir 1))

(use-package diredfl)
(use-package dired-hacks-utils)
;; (use-package dired-avfs)
(use-package dired-rainbow)
(use-package dired-filter)
(use-package dired-open)
(use-package dired-subtree)
(use-package dired-ranger)
(use-package dired-narrow)
(use-package dired-list)
(use-package dired-collapse)
(use-package dired-rsync)

(use-package dired-du)
(use-package dired-single)
(use-package diredful)
;; (use-package dired-quick-sort)
(use-package dired-narrow)
(use-package dired-k)
(use-package dired-filter)

(use-package dired-recent
  :config
  (dired-recent-mode 1))

(use-package dired-hist
  :ensure ( :host github :repo "karthink/dired-hist")
  :config
  (dired-hist-mode 1))

;; (use-package filetree
;;   :ensure ( :host github :repo "knpatel401/filetree"))

;; elpaca doesnt support :fetcher url
;; (use-package dired-sort-menu
;;   :ensure ( :fetcher url :url "https://www.emacswiki.org/emacs/dired-sort-menu.el"))

;; alternative? https://wilkesley.org/~ian/xah/emacs/dired_sort.html
(defun xah-dired-sort ()
  "Sort dired dir listing in different ways.
Prompt for a choice.
URL `http://ergoemacs.org/emacs/dired_sort.html'
Version 2015-07-30"
  (interactive)
  (let (-sort-by -arg)
    (setq -sort-by (completing-read "Sort by:" '( "date" "size" "name" "dir")))
    (cond
     ((equal -sort-by "name") (setq -arg "-Al --si --time-style long-iso "))
     ((equal -sort-by "date") (setq -arg "-Al --si --time-style long-iso -t"))
     ((equal -sort-by "size") (setq -arg "-Al --si --time-style long-iso -S"))
     ((equal -sort-by "dir") (setq -arg "-Al --si --time-style long-iso --group-directories-first"))
     (t (error "logic error 09535" )))
    (dired-sort-other -arg )))


(defun kill-all-dired-buffers ()
  "Kill all dired buffers."
  (interactive)
  (save-excursion
    (let ((count 0))
      (dolist (buffer (buffer-list))
        (set-buffer buffer)
        (when (equal major-mode 'dired-mode)
          (setq count (1+ count))
          (kill-buffer buffer)))
      (message "Killed %i dired buffer(s)." count))))

;; hide unnecessary stuff
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
;; (setq dired-hide-details-preserved-columns '(5 6 7 8)) ;; dont hide some columns (date and size)
;; (setq dired-listing-switches "-Al") ;; default is ls -al
(setq dired-listing-switches "-A") ;; default is ls -al
(setq dired-dwim-target t) ;; moving files in a smart way when window is split into 2

;; function to get size of files in dired
(defun dired-get-size ()
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "du" nil t nil "-sch" files)
      (message "Size of all marked files: %s"
               (progn
                 (re-search-backward "\\(^[0-9.,]+[A-Za-z]+\\).*total$")
                 (match-string 1))))))
;; (define-key dired-mode-map (kbd "?") 'dired-get-size)
(with-eval-after-load 'general
  (general-define-key :states '(normal) :keymaps 'dired-mode-map "?" 'dired-get-size))

;; vim like keys for dired image viewer
(setq image-dired-show-all-from-dir-max-files 100000000)
(defun image-dired-up ()
  (interactive)
  (previous-line)
  (setq current-char (- (point) (point-at-bol)))
  (if (eq (% current-char 2) 1)
      (left-char))
  (image-dired-display-thumbnail-original-image))
(defun image-dired-down ()
  (interactive)
  (next-line)
  (setq current-char (- (point) (point-at-bol)))
  (if (eq (% current-char 2) 1)
      (left-char))
  (image-dired-display-thumbnail-original-image))
(defun image-dired-bol ()
  (interactive)
  (beginning-of-line)
  (image-dired-display-thumbnail-original-image))
(defun image-dired-eol ()
  (interactive)
  (end-of-line)
  (left-char)
  (image-dired-display-thumbnail-original-image))
(defun define-dired-thumbnail-mode-keys ()
  (define-key image-dired-thumbnail-mode-map (kbd "l") 'image-dired-display-next-thumbnail-original)
  (define-key image-dired-thumbnail-mode-map (kbd "h") 'image-dired-display-previous-thumbnail-original)
  (define-key image-dired-thumbnail-mode-map (kbd "k") 'image-dired-up)
  (define-key image-dired-thumbnail-mode-map (kbd "j") 'image-dired-down)
  (define-key image-dired-thumbnail-mode-map (kbd "0") 'image-dired-bol)
  (define-key image-dired-thumbnail-mode-map (kbd "$") 'image-dired-eol))
(add-hook 'image-dired-thumbnail-mode-hook 'define-dired-thumbnail-mode-keys)

(advice-add 'dired-get-file-for-visit :filter-return
            (lambda (filename)
              (add-to-history 'file-name-history filename)
              filename))

;; when moving a file, auto rename it if the target contains a file with the same name
;; (defun my/dired-rename-auto-rename (orig-fn &optional arg)
;;   "Rename current file or all marked (or next ARG) files, automatically renaming if needed."
;;   (let* ((files (dired-get-marked-files))
;;          (target (read-file-name "Rename to: " (dired-dwim-target-directory)))
;;          (target (if (file-directory-p target)
;;                      (concat (file-name-as-directory target)
;;                              (file-name-nondirectory (car files)))
;;                    target)))
;;     (if (file-exists-p target)
;;         (setq target (dired-unique-filename target)))
;;     ;; Perform the rename
;;     (rename-file (car files) target)
;;     ;; Refresh dired buffer after renaming
;;     (dired-revert)))
;; (defun dired-unique-filename (filename)
;;   "Generate a unique filename by appending numbers to FILENAME."
;;   (let ((base (file-name-sans-extension filename))
;;         (ext (file-name-extension filename t))
;;         (counter 1))
;;     (while (file-exists-p filename)
;;       (setq filename (format "%s_%d%s" base counter ext))
;;       (setq counter (1+ counter)))
;;     filename))
;; (advice-add 'dired-do-rename :around #'my/dired-rename-auto-rename)
(defun my/dired-auto-rename-file (&optional arg)
  "Rename current file or all marked (or next ARG) files, automatically renaming if needed."
  (interactive "P")
  (let* ((files (dired-get-marked-files))
         (target (read-file-name "Rename to: " (dired-dwim-target-directory)))
         (target (if (file-directory-p target)
                     (concat (file-name-as-directory target)
                             (file-name-nondirectory (car files)))
                   target)))
    (if (file-exists-p target)
        (setq target (dired-unique-filename target)))
    ;; Perform the rename
    (rename-file (car files) target)
    ;; Refresh dired buffer after renaming
    (dired-revert)))
(defun dired-unique-filename (filename)
  "Generate a unique filename by appending numbers to FILENAME."
  (let ((base (file-name-sans-extension filename))
        (ext (file-name-extension filename t))
        (counter 1))
    (while (file-exists-p filename)
      (setq filename (format "%s_%d%s" base counter ext))
      (setq counter (1+ counter)))
    filename))

(provide 'setup-dired)