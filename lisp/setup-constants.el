;; path where all my notes etc go
(defconst *brain-dir* (getenv "BRAIN_DIR"))

(defconst *music-dir* (concat (getenv "MUSIC_DIR") "/"))

;;(defconst *leader-key* "C-z");;"SPC")
;; (global-set-key (kbd "<SPC>") (make-sparse-keymap))
;;(defconst *leader-key* "<escape>")
(defconst *leader-key* "<SPC>")

(provide 'setup-constants)