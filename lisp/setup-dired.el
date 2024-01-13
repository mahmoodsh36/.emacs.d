;; disable some modes for large files (otherwise emacs will hang..)
(defun conditional-disable-modes ()
  (unless (eq major-mode 'pdf-view-mode)
    (when (> (buffer-size) (* 1024 1024))
      (flycheck-mode -1)
      (font-lock-mode -1)
      (fundamental-mode)
      (which-function-mode -1)
      (linum-mode 0)
      (lsp-mode 0)
      ))
  )
(add-hook 'find-file-hook 'conditional-disable-modes)

;; from https://stackoverflow.com/questions/18316665/how-to-improve-emacs-performance-when-view-large-file
;; (defun my-find-file-check-make-large-file-read-only-hook ()
;;   "If a file is over a given size, make the buffer read only."
;;   (when (> (buffer-size) (* 1024 1024))
;;     (setq buffer-read-only t)
;;     (buffer-disable-undo)
;;     (fundamental-mode)))

;; (add-hook 'find-file-hook 'my-find-file-check-make-large-file-read-only-hook)
;; there's also find-file-literally i guess

;; (use-package dired+
;;   :quelpa (dired+ :fetcher url :url "https://www.emacswiki.org/emacs/download/dired+.el")
;;   :defer 1
;;   :init
;;   (setq diredp-hide-details-initially-flag nil)
;;   (setq diredp-hide-details-propagate-flag nil)
;;   :config
;;   (diredp-toggle-find-file-reuse-dir 1))

;; (use-package diredfl)
(use-package dired-hacks-utils)
(use-package dired-avfs)
(use-package dired-rainbow)
(use-package dired-filter)
(use-package dired-open)
(use-package dired-subtree)
(use-package dired-ranger)
(use-package dired-narrow)
(use-package dired-list)
(use-package dired-collapse)
(use-package dired-rsync)

;; hide unnecessary stuff
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
(setq dired-listing-switches "-Al") ;; default is ls -al
(setq dired-dwim-target t) ;; moving files in a smart way when window is split into 2
(add-hook 'dired-mode-hook 'auto-revert-mode) ;; hook to make dired auto refresh files when they get edited/changed/created/whatever

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
(general-define-key :states '(normal) :keymaps 'dired-mode-map "?" 'dired-get-size)

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

(provide 'setup-dired)