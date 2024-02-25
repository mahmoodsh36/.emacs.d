(defun play-album ()
  (interactive)
  (let ((album-titles
         (apply
          #'cl-concatenate
          (list*
           'list
           (mapcar
            (lambda (dir)
              (mapcar (lambda (album-title)
                        (concat (file-name-nondirectory dir) "/" album-title))
                      (directory-files dir)))
            (cl-remove-if-not
             #'file-directory-p
             (directory-files *music-dir* t "^[^.].*$")))))))
    (let ((chosen-album (completing-read "pick album: " album-titles)))
      (message (join-path *music-dir* chosen-album))
      (call-process "play_dir_as_album.sh" nil 0 nil
                    (join-path *music-dir* chosen-album))
      (message "playing album %s" chosen-album))))
;; (defun play-album-2 ()
;;   (interactive)
;;   (let ((album-titles
;;          (apply
;;           #'cl-concatenate
;;           (list*
;;            'list
;;            (mapcar
;;             (lambda (dir)
;;               (directory-files dir nil "^[^.].*$"))
;;             (cl-remove-if-not
;;              #'file-directory-p
;;              (directory-files *music-dir* t "^[^.].*$")))))))
;;     (let ((chosen-album (completing-read "pick album: " album-titles)))
;;       (call-process "play_dir_as_album.sh" nil 0 nil
;;                     (cl-find-if (lambda (filepath) (string-match (format ".*/%s$" chosen-album) filepath)) (directory-files-recursively *music-dir* "" t)))
;;       (message "playing album %s" chosen-album))))
(defun open-lastfm-page ()
  (interactive)
  (let ((artist-names (mapcar #'file-name-nondirectory (cl-remove-if-not #'file-directory-p (directory-files *music-dir* t)))))
    (let ((chosen-artist (completing-read "pick artist: "
                                          artist-names
                                          nil
                                          nil
                                          (current-mpv-artist))))
      (browse-url (format "https://www.last.fm/music/%s" chosen-artist)))))
(defun play-artist ()
  (interactive)
  (let ((artist-names (mapcar #'file-name-nondirectory (cl-remove-if-not #'file-directory-p (directory-files *music-dir* t)))))
    (let ((chosen-artist (completing-read "pick artist: " artist-names)))
      (dired (join-path *music-dir* chosen-artist)))))
(defun play-file ()
  (interactive)
  (let ((my-file (completing-read "select file: " (cl-remove-if (lambda (filepath)
                                                                  (string-match "\\.\\(spotdl\\|lrc\\|jpg\\|json\\)$" filepath))
                                                                (directory-files-recursively *music-dir* "")))))
    (browse-url (expand-file-name my-file))))

(provide 'setup-music)