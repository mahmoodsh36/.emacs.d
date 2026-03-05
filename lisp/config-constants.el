;; path where all my notes etc go
(defconst *brain-dir*
  (or (my-getenv "BRAIN_DIR")
      (file-truename "~/brain")))
(defconst *notes-dir* (join-path *brain-dir* "/notes"))

(defconst *vol-dir*
  (or (my-getenv "VOLUME_DIR")
      (file-truename "~/")))

(defconst *data-dir* (join-path *vol-dir* "data"))

(defconst *work-dir* (join-path *vol-dir* "work"))

;; used in my elisp scripts for launching mpv
(defconst *music-dir* (concat (my-getenv "MUSIC_DIR") "/"))

(defconst *personal-website-url* "https://mahmoodsheikh36.github.io")

(defconst *bibtex-file* (from-brain "bib.bib"))

;; where my html exports go to, for my blog or whatever
(defconst *static-html-dir* (concat (my-getenv "BLOG_DIR") "/"))
(defconst *html-static-route* "/")
(ignore-errors (mkdir *static-html-dir*)) ;; ensure it exists
;; where "template" html files go
(defconst *template-html-dir* (join-path (my-getenv "WORK_DIR") "template"))

;;(defconst *leader-key* "C-z");;"SPC")
;; (global-set-key (kbd "<SPC>") (make-sparse-keymap))
;;(defconst *leader-key* "<escape>")
(defconst *leader-key* "<SPC>")

(defconst *shell-program* "zsh")

;; if i need more recursion depth
;; (setq max-lisp-eval-depth 10000)

(defvar *use-sly* t)

(provide 'config-constants)