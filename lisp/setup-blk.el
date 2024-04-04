(if (file-exists-p "/home/mahmooz/work/blk/")
    ;; (push "/home/mahmooz/work/blk/" load-path)
    (use-package blk
      :after (org org-transclusion)
      :load-path "/home/mahmooz/work/blk/")
  (use-package blk
    :after (org org-transclusion)
    :ensure ( :host github :repo "mahmoodsheikh36/blk")))

(with-eval-after-load 'blk
  ;; (setq blk-grepper blk-grepper-grep)
  ;; (setq blk-patterns blk-grep-patterns)
  ;; (setq blk-grepper 'blk-grepper-emacs)
  ;; (setq blk-patterns blk-emacs-patterns)
  (setq blk-directories
        (list (from-brain "notes")
              (file-name-parent-directory (expand-file-name user-init-file))))
  (blk-configure-org-link)
  (blk-configure-org-transclusion))

(provide 'setup-blk)