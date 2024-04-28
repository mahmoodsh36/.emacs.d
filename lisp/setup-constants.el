;; path where all my notes etc go
(defconst *brain-dir*
  (or (getenv "BRAIN_DIR")
      (file-truename "~/brain")))
(defconst *notes-dir* (concat *brain-dir* "/notes/"))

(defconst *music-dir* (concat (getenv "MUSIC_DIR") "/"))

(defconst *personal-website-url* "https://mahmoodsheikh36.github.io")

;;(defconst *leader-key* "C-z");;"SPC")
;; (global-set-key (kbd "<SPC>") (make-sparse-keymap))
;;(defconst *leader-key* "<escape>")
(defconst *leader-key* "<SPC>")

;; if i need more recursion depth
;; (setq max-lisp-eval-depth 10000)

(provide 'setup-constants)