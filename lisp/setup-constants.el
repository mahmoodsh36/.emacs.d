;; path where all my notes etc go
(defconst *brain-dir*
  (or (getenv "BRAIN_DIR")
      (file-truename "~/brain")))
(defconst *notes-dir* (concat *brain-dir* "/notes/"))

;; used in my elisp scripts for launching mpv
(defconst *music-dir* (concat (getenv "MUSIC_DIR") "/"))

(defconst *personal-website-url* "https://mahmoodsheikh36.github.io")

;; where my html exports go to, for my blog or whatever
(defconst *static-html-dir* (concat (getenv "BLOG_DIR") "/"))
(defconst *static-html-dir-auto* (format "%s/static/auto/" *static-html-dir*)) ;; where automatic exports go
(ignore-errors (mkdir *static-html-dir-auto*)) ;; ensure it exists

;;(defconst *leader-key* "C-z");;"SPC")
;; (global-set-key (kbd "<SPC>") (make-sparse-keymap))
;;(defconst *leader-key* "<escape>")
(defconst *leader-key* "<SPC>")

;; if i need more recursion depth
;; (setq max-lisp-eval-depth 10000)

(provide 'setup-constants)