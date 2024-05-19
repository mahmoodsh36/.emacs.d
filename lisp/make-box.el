;;; make-box.el --- Box around part of a buffer -*- lexical-binding: t -*-

;; Copyright (C) 2024 Nicolas P. Rougier

;; Maintainer: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;; source: https://gist.github.com/rougier/c75dcc1365d15a327260051086d68309

(defvar default-box-alist `((foreground-color . ,(face-foreground 'default))
                            (background-color . ,(face-background 'default))
                            (header-face      . (:height 0.75 :inherit bold))))

(defun make--box (beg end &optional header parameters)
  "The make-box function allows to surround some part (from
 BEG to END) of a buffer with borders with an optional
 HEADER. Style is controlled with the PAREMETERS alist.

The text is left untouched and the method only exploits special
properties: display, wrap-prefix and line-prefix. The drawback is
that the buffer must have a left margin (at least one character)
and a right margin (at least 2 characters). Futhermore, the two
first and two last characters cannot be edited. The resulting box
is dynamic and will adapt automatically to the size of the window."

  (let* ((parameters (append parameters default-box-alist))
         (fg (alist-get 'foreground-color parameters))
         (bg (alist-get 'background-color parameters))
         (face (alist-get 'header-face parameters))

         (header-face (if (stringp header)
                          `(:background ,bg :overline ,fg :extend t :inherit ,face)
                      `(:background ,bg :overline ,fg :extend t :height 0.25)))
         (footer-face `(:background ,bg :underline ,fg :extend t :height 0.25))
         (header (if (stringp header)
                     (substring-no-properties header)
                   ""))

         (body (concat (propertize " " 'display `((margin left-margin)
                                                  ,(concat (propertize " " 'face `(:background ,fg :height 10))
                                                           (propertize " " 'face `(:background ,bg :height 10)))))
                       (propertize " " 'display `((margin right-margin)
                                                  ,(concat (propertize " " 'face `(:background ,bg))
                                                           (propertize " " 'face `(:background ,fg :height 10))
                                                           (propertize " " 'face `(:background ,(face-background 'default))))))))
         (top (concat (propertize " " 'display `((margin left-margin)
                                                 ,(concat (propertize " " 'face `(:background ,fg :overline ,fg :height 10))
                                                          (propertize " " 'face `(:background ,bg :overline ,fg)))))
                      (propertize " " 'display `((margin right-margin)
                                                 ,(concat (propertize " " 'face `(:background ,bg :overline ,fg))
                                                          (propertize " " 'face `(:background ,fg :overline ,fg :height 10))
                                                          (propertize " " 'face `(:background ,(face-background 'default))))))))
         (bot (concat (propertize " " 'display `((margin left-margin)
                                                 ,(concat (propertize " " 'face `(:background ,fg :underline ,fg :height 10))
                                                          (propertize " " 'face `(:background ,bg :underline ,fg)))))
                      (propertize " " 'display `((margin right-margin)
                                                 ,(concat (propertize " " 'face `(:background ,bg :underline ,fg))
                                                          (propertize " " 'face `(:background ,fg :underline ,fg :height 10))
                                                          (propertize " " 'face `(:background ,(face-background 'default)))))))))

         (add-text-properties (+ beg 1) (+ beg 2) `(display ,(buffer-substring beg (+ beg 2))
                                                    cursor-intangible t))
         (add-text-properties (- end 2) (- end 1) `(display ,(concat (buffer-substring (- end 2) (- end 0)) "")
                                                    cursor-intangible t))
         (add-text-properties (+ beg 0) (+ beg 1) `(face ,header-face
                                                    font-lock-face ,header-face
                                                    display ,(propertize (concat header"\n") 'face header-face
                                                                                             'font-lock-face header-face)
                                                    cursor-intangible t
                                                    wrap-prefix ,top
                                                    line-prefix ,top))
         (add-text-properties (+ beg 1) (- end 1) `(wrap-prefix ,body
                                                    line-prefix ,body))
;;         (add-face-text-property (+ beg 1) (- end 1) `(:background ,bg :extend t))
         (add-text-properties (- end 1) (- end 0)  `(face ,footer-face
                                                     font-lock-face ,footer-face
                                                     display ,(propertize "\n" 'face footer-face
                                                                              'font-lock-face footer-face)
                                                     cursor-intangible t
                                                     wrap-prefix ,bot
                                                     line-prefix ,bot))))


(defun make-box (header)
  "This interactive function adds a one pixel border to the
 current region (if active) or paragraph, adding an optional
 HEADER."

  (interactive "sHeader: ")

  (setq left-margin-width (max 1 left-margin-width)
        right-margin-width (max 2 right-margin-width))
  (set-window-margins nil left-margin-width right-margin-width)
  (set-window-buffer nil (current-buffer))

  (let* ((beg (if (region-active-p)
                  (region-beginning)
                (save-excursion
                  (start-of-paragraph-text)
                  (point))))
         (beg (save-excursion
                (goto-char beg)
                (line-beginning-position)))

        (end (if (region-active-p)
                 (region-end)
               (save-excursion
                 (end-of-paragraph-text)
                 (point))))
        (end (save-excursion
               (goto-char end)
               (1+ (line-end-position))))

        (header (when (and (stringp header)
                           (> (length header) 0))
                  header)))
    (make--box beg end header)))
