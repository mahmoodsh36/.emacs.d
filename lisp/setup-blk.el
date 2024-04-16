(if (file-exists-p "/home/mahmooz/work/blk/")
    ;; (push "/home/mahmooz/work/blk/" load-path)
    (use-package blk
      :after (org org-transclusion)
      :load-path "/home/mahmooz/work/blk/")
  (use-package blk
    :after (org org-transclusion)
    :ensure ( :host github :repo "mahmoodsheikh36/blk")))

(defun my-consult-completing-read (prompt table &rest whatever)
  (consult--read
   table
   :prompt prompt
   :sort nil
   :require-match t
   :history '(:input blk-find-history)
   :state (consult--jump-state)
   :annotate (consult--line-prefix)
   :category 'consult-location
   :lookup (lambda (selected &rest _) selected)
   :add-history (thing-at-point 'symbol)))

(defun blk-find-with-consult ()
  (interactive)
  (let ((completing-read-function 'my-consult-completing-read))
    (call-interactively 'blk-find)))

(defun blk-find-with-ivy ()
  (interactive)
  (let ((completing-read-function 'ivy-completing-read))
    (call-interactively 'blk-find)))

(with-eval-after-load 'blk
  (defvar blk-find-history nil)

  ;; (setq blk-grepper blk-grepper-grep)
  ;; (setq blk-patterns blk-grep-patterns)
  ;; (setq blk-grepper 'blk-grepper-emacs)
  ;; (setq blk-patterns blk-emacs-patterns)

  (setq blk-directories
        (list (from-brain "notes")
              (file-name-parent-directory (expand-file-name user-init-file))))

  ;; org-transclusion integration
  (blk-configure-org-transclusion)

  ;; use auctex
  (setq blk-tex-env-at-point-function 'blk-auctex-env-at-point-bounds)

  ;; add the :defines pattern
  (dolist (pattern-table '(blk-rg-patterns blk-grep-patterns))
    ;; (add-to-list pattern-table (list :title "definition or mention"
    ;;                                  :glob "*.org"
    ;;                                  :anchor-regex "(:defines|:mentions)\\s+[^:]+"
    ;;                                  :title-function 'blk-value-after-space-before-colon
    ;;                                  :extract-id-function 'blk-org-id-at-point))
    (add-to-list pattern-table (list :title "definition"
                                     :glob "*.org"
                                     :anchor-regex "(:defines)\\s+[^:]+"
                                     :title-function 'blk-value-after-space-before-colon
                                     :extract-id-function 'blk-org-id-at-point))
    )
  (setq blk-patterns blk-rg-patterns)
  )

(provide 'setup-blk)