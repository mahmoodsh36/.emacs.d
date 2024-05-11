(if (file-exists-p "/home/mahmooz/work/blk/")
    ;; (push "/home/mahmooz/work/blk/" load-path)
    (use-package blk
      :after (org org-transclusion)
      :load-path "/home/mahmooz/work/blk/")
  (use-package blk
    :after (org org-transclusion)
    :ensure ( :host github :repo "mahmoodsheikh36/blk")))

;; transclusions (including text from other documents) for org mode, causes problems when inserting ids to blocks that have a name using blk..
(use-package org-transclusion
  :after (org)
  :config
  (add-hook 'org-mode-hook #'org-transclusion-mode)
  )

;; (defun my-consult-completing-read (prompt table &rest whatever)
;;   (consult--read
;;    table
;;    :prompt prompt
;;    :sort nil
;;    :require-match t
;;    :history '(:input blk-find-history)
;;    :state (consult--jump-state)
;;    :annotate (consult--line-prefix)
;;    :category 'consult-location
;;    :lookup (lambda (selected &rest _) selected)
;;    :add-history (thing-at-point 'symbol)))

(defun blk-find-with-consult ()
  (interactive)
  (let ((completing-read-function 'my-consult-completing-read))
    (call-interactively 'blk-find)))

(defun blk-find-with-ivy ()
  (interactive)
  (let ((completing-read-function 'ivy-completing-read))
    (call-interactively 'blk-find)))

(with-eval-after-load 'blk
  ;; (defvar blk-find-history nil)

  ;; (setq blk-grepper blk-grepper-grep)
  ;; (setq blk-patterns blk-grep-patterns)
  ;; (setq blk-grepper 'blk-grepper-emacs)
  ;; (setq blk-patterns blk-emacs-patterns)

  (setq blk-directories
        (list (from-brain "notes")))

  ;; org-transclusion integration
  (blk-configure-org-transclusion)

  ;; use auctex
  (setq blk-tex-env-at-point-function 'blk-auctex-env-at-point-bounds)

  ;; allow for recursive grep
  (setq blk-search-recursively t)

  ;; enable group search (group things together)
  (setq blk-enable-groups t)

  ;; add the :defines pattern
  (dolist (pattern-table '(blk-rg-patterns blk-grep-patterns))
    (add-to-list pattern-table (list :title "definition or mention"
                                     :glob "*.org"
                                     :anchor-regex "(:defines|:mentions)\\s+[^:]+"
                                     :title-function 'blk-value-after-space-upto-colon
                                     :extract-id-function 'blk-org-id-at-point))
    ;; (add-to-list pattern-table (list :title "definition"
    ;;                                  :glob "*.org"
    ;;                                  :anchor-regex "(:defines)\\s+[^:]+"
    ;;                                  :title-function 'blk-value-after-space-upto-colon
    ;;                                  :extract-id-function 'blk-org-id-at-point))
    )
  (setq blk-patterns blk-rg-patterns)

  ;; (add-hook 'text-mode-hook #'blk-enable-completion)
  )

(provide 'setup-blk)
