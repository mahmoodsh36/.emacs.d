;; path where all my notes etc go
(defconst *brain-dir* (getenv "BRAIN_DIR"))
(defconst *music-dir* (concat (getenv "MUSIC_DIR") "/"))

(provide 'setup-constants)