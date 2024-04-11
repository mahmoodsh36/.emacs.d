;; evil mode configuration and setup

(setq evil-want-keybinding nil)
;; enable emacs keys in evil insert mode
(setq evil-disable-insert-state-bindings t)
(setq enable-evil t)
;; use builtin undo system, may not be needed because i bind the keys below
;; (setq evil-undo-system nil)

(when enable-evil
  (use-package evil
    :config
    (evil-mode 1)
    (evil-set-initial-state 'image-dired-thumbnail-mode 'emacs)
    ;; undo/redo keys
    (define-key evil-normal-state-map "u" 'undo)
    (define-key evil-normal-state-map "r" 'undo-redo)
    ;; make ESC cancel all
    (define-key key-translation-map (kbd "ESC") (kbd "C-g")))
  ;; (define-key key-translation-map (kbd "<escape>") (kbd "C-g")))
  ;;dont copy the overwritten text when overwriting text by pasting
  ;;(setq-default evil-kill-on-visual-paste nil))

  ;; evil-surround for evil mode
  (use-package evil-surround
    :config
    (global-evil-surround-mode 1))

  ;; exchange portions of text more easily with evil
  (use-package evil-exchange
    :config
    (evil-exchange-install))

  ;; search for the current visual selection with *
  (use-package evil-visualstar
    :config
    (global-evil-visualstar-mode))

  ;; support to make evil more compatible with the whole of emacs
  (use-package evil-collection
    :after evil
    :config
    (evil-collection-init))

  ;; ;; display visual hints for evil actions
  (use-package evil-goggles
    :config
    (evil-goggles-mode))

  ;; make line a text object - yil dil cil, etc..
  (use-package evil-textobj-line)

  ;; quick commenting
  (use-package evil-nerd-commenter
    :config
    (global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines))

  ;; evil mode support for org
  (use-package evil-org
    :config
    (add-hook 'org-mode-hook 'evil-org-mode)
    (evil-org-set-key-theme '(textobjects navigation additional shift todo heading return calendar))
    (require 'evil-org-agenda)
    (evil-org-agenda-set-keys)
    ;; the package annoyingly binds O to another function so here im just restoring it
    (general-define-key :states 'normal :keymaps 'override "O" 'evil-open-above)

    ;; for some reason these keys act weirdly in org mode with xenops so im gonna rebind them..
    ;; (general-define-key :states 'normal :keymaps 'org-mode-map "p" 'evil-paste-after)
    ;; (general-define-key :states 'normal :keymaps 'override "o"
    ;;                     (lambda ()
    ;;                       (interactive)
    ;;                       (evil-org-append-line 1)
    ;;                       (evil-ret 1)
    ;;                       (indent-according-to-mode))))
    (general-define-key :states '(normal visual motion operator) :keymaps 'override "0" 'evil-beginning-of-line)
    (general-define-key :states '(normal visual motion operator) :keymaps 'override "$" 'evil-end-of-line)
    (general-define-key :states '(normal visual motion operator) :keymaps 'override "^" 'evil-first-non-blank))

  ;; for evil mode compatibility
  (use-package treemacs-evil
    :config
    (general-define-key :states '(normal motion treemacs) :keymaps 'override "SPC t" 'treemacs))

  ;; indentation-based text objects for evil
  (use-package evil-indent-plus
    :config
    (define-key evil-inner-text-objects-map "i" 'evil-indent-plus-i-indent)
    (define-key evil-outer-text-objects-map "i" 'evil-indent-plus-a-indent)
    (define-key evil-inner-text-objects-map "I" 'evil-indent-plus-i-indent-up-down)
    (define-key evil-outer-text-objects-map "I" 'evil-indent-plus-a-indent-up-down))

  ;; jump to matching tags
  (use-package evil-matchit
    :config
    (global-evil-matchit-mode 1)
    ;; override the default to make it work with custom blocks in org mode
    (setq evilmi-org-match-tags
          '((("begin_[a-z_]+") () ("end_[a-z_]+") "monogamy")
            (("results") () ("end") "monogamy"))))

  ;; text evil objects for latex
  ;; it requries auctex, so disable it on android
  (when (not (is-android-system))
    (use-package evil-tex
      :config
      (add-hook 'LaTeX-mode-hook #'evil-tex-mode)
      (add-hook 'org-mode-hook #'evil-tex-mode)))

  ;; preview registers and marks before actually using them
  (use-package evil-owl
    :config
    (evil-owl-mode))

  ;; interpret words of columns as a text object
  (use-package evil-textobj-column
    :config
    (define-key evil-inner-text-objects-map "c" 'evil-textobj-column-word)
    (define-key evil-inner-text-objects-map "C" 'evil-textobj-column-WORD))

  ;; display number of matches when searching
  ;; (use-package anzu)
  (use-package evil-anzu)

  ;; this macro was copied from here: https://stackoverflow.com/a/22418983/4921402
  (defmacro define-and-bind-quoted-text-object (name key start-regex end-regex)
    (let ((inner-name (make-symbol (concat "evil-inner-" name)))
          (outer-name (make-symbol (concat "evil-a-" name))))
      `(progn
         (evil-define-text-object ,inner-name (count &optional beg end type)
                                  (evil-select-paren ,start-regex ,end-regex beg end type count nil))
         (evil-define-text-object ,outer-name (count &optional beg end type)
                                  (evil-select-paren ,start-regex ,end-regex beg end type count t))
         (define-key evil-inner-text-objects-map ,key #',inner-name)
         (define-key evil-outer-text-objects-map ,key #',outer-name))))

  (with-eval-after-load 'evil
    (with-eval-after-load 'general

      ;; disable evil mode in deadgrep, they dont work well together
      ;; (evil-set-initial-state 'deadgrep-mode 'emacs)
      (evil-set-initial-state 'calc-mode 'emacs)
      (evil-set-initial-state 'eat-mode 'emacs)
      (evil-set-initial-state 'nov-mode 'emacs)
      ;; (evil-set-initial-state 'sldb-mode 'emacs) ;; for slime
      (evil-set-initial-state 'sly-db-mode 'emacs)
      ;; (evil-set-initial-state 'sly-inspector-mode 'emacs)
      (evil-set-initial-state 'vterm-mode 'emacs)
      ;; (evil-set-initial-state 'org-agenda-mode 'emacs)

      (general-evil-setup)

      ;; (general-define-key :states 'normal :keymaps 'override "{" 'evil-scroll-line-up)
      ;; (general-define-key :states 'normal :keymaps 'override "}" 'evil-scroll-line-down)

      (general-define-key :states 'normal :keymaps 'prog-mode-map "K" 'evil-jump-to-tag)

      (general-define-key :states 'normal :keymaps 'override ")" 'evil-scroll-page-down)
      (general-define-key :states 'normal :keymaps 'override "(" 'evil-scroll-page-up)

      (led-kbd "w" 'evil-window-map)

      (define-and-bind-quoted-text-object "dollar" "$" "\\$" "\\$")
      (define-and-bind-quoted-text-object "pipe" "|" "|" "|")
      (define-and-bind-quoted-text-object "slash" "/" "/" "/")
      (define-and-bind-quoted-text-object "space" " " " " " ")
      (define-and-bind-quoted-text-object "tilde" "~" "~" "~")
      (define-and-bind-quoted-text-object "asterisk" "*" "*" "*")

      ;; create "il"/"al" (inside/around) line text objects:
      ;; (define-and-bind-text-object "l" "^\\s-*" "\\s-*$")
      ;; create "ia"/"aa" (inside/around) entire buffer text objects:
      (define-and-bind-quoted-text-object "buffer" "A" "\\`\\s-*" "\\s-*\\'")

      ;; (general-define-key :states 'normal :keymaps '(text-mode-map prog-mode-map latex-mode-map tex-mode-map bibtex-mode-map fundamental-mode-map) "s" 'save-buffer)
      (general-define-key :states 'normal :keymaps 'override "s" 'save-buffer)

      (general-define-key :states 'normal :keymaps 'pdf-view-mode-map "d" 'pdf-view-scroll-up-or-next-page)
      (general-define-key :states 'normal :keymaps 'pdf-view-mode-map "u" 'pdf-view-scroll-down-or-previous-page)
      (general-define-key :states 'normal :keymaps 'pdf-view-mode-map "K" 'pdf-view-enlarge)
      (general-define-key :states 'normal :keymaps 'pdf-view-mode-map "J" 'pdf-view-shrink)
      ;; (general-define-key :states 'normal :keymaps 'dired-mode-map "l" 'dired-find-file)
      ;; (general-define-key :states 'normal :keymaps 'dired-mode-map "h" 'dired-up-directory)

      ;; dired
      (general-define-key :states 'normal :keymaps 'dired-mode-map "y" #'copy-file-path)

      ;; some sly keys
      ;; (general-define-key :states '(normal motion) :keymaps 'sly-repl-mode-map "K" 'sly-describe-symbol)

      (general-define-key :states 'normal :keymaps 'org-mode-map "]k" 'org-babel-next-src-block)
      (general-define-key :states 'normal :keymaps 'org-mode-map "[k" 'org-babel-previous-src-block)
      (general-define-key :states 'normal :keymaps 'org-mode-map "]o" 'org-next-block)
      (general-define-key :states 'normal :keymaps 'org-mode-map "[o" 'org-previous-block)

      ;; general keys for programming
      (general-define-key :states '(normal) :keymaps 'prog-mode-map "] r" 'next-error)
      (general-define-key :states '(normal) :keymaps 'prog-mode-map "[ r" 'previous-error)))

  ;; evil mode multiple cursors
  (use-package evil-mc
    :config
    (global-evil-mc-mode)
    (general-define-key :states 'normal :keymaps 'override "g . p" 'evil-mc-pause-cursors)
    (general-define-key :states 'normal :keymaps 'override "g . r" 'evil-mc-resume-cursors))

  ;; (use-package evil-escape)
  ;; (evil-escape-mode)

  ;; interpret function arguments as a text object
  (use-package evil-args)
  (use-package evil-lion)

  (use-package evil-extra-operator)

  (use-package evil-textobj-tree-sitter
    :config
    ;; bind `function.outer`(entire function block) to `f` for use in things like `vaf`, `yaf`
    (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
    ;; bind `function.inner`(function block without name and args) to `f` for use in things like `vif`, `yif`
    (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))
    ;; you can also bind multiple items and we will match the first one we can find
    (define-key evil-outer-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer")))
    (define-key evil-inner-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj ("conditional.inner" "loop.inner")))
    ;; goto start of next function
    (define-key evil-normal-state-map (kbd "]f")
                (lambda ()
                  (interactive)
                  (evil-textobj-tree-sitter-goto-textobj "function.outer")))
    ;; goto start of previous function
    (define-key evil-normal-state-map (kbd "[f")
                (lambda ()
                  (interactive)
                  (evil-textobj-tree-sitter-goto-textobj "function.outer" t)))
    ;; goto end of next function
    (define-key evil-normal-state-map (kbd "]F")
                (lambda ()
                  (interactive)
                  (evil-textobj-tree-sitter-goto-textobj "function.outer" nil t)))
    ;; goto end of previous function
    (define-key evil-normal-state-map (kbd "[F")
                (lambda ()
                  (interactive)
                  (evil-textobj-tree-sitter-goto-textobj "function.outer" t t)))
    )

  ;; make org roam insert link after cursor in evil mode
  (defun my-insert-advice-append-if-in-normal-mode (fn &rest args)
    "if in evil normal mode and cursor is on a whitespace character, then go into
append mode first before inserting the link. this is to put the link after the
space rather than before."
    (let ((is-in-evil-normal-mode (and (bound-and-true-p evil-mode)
                                       (not (bound-and-true-p evil-insert-state-minor-mode))
                                       (looking-at "[[:blank:]]"))))
      (if (not is-in-evil-normal-mode)
          (apply fn args)
        (evil-append 0)
        (apply fn args)
        (evil-normal-state))))
  (advice-add 'denote-insert-link :around #'my-insert-advice-append-if-in-normal-mode)
  (advice-add 'blk-insert :around #'my-insert-advice-append-if-in-normal-mode)

  ;; (use-package evil-snipe
  ;;   :config
  ;;   (evil-snipe-override-mode 1)
  ;;   (general-define-key :states '(normal motion) :keymaps 'override "SPC ;" 'evil-snipe-s))

  ;; so that forward-sexp works at end of line, see https://github.com/fuco1/smartparens/issues/1037
  ;; (setq evil-move-beyond-eol t)

  (with-eval-after-load 'general
    (general-define-key :states 'normal :keymaps 'doc-view-mode-map "k" 'doc-view-previous-page)
    (general-define-key :states 'normal :keymaps 'doc-view-mode-map "j" 'doc-view-next-page)
    )
  )


(provide 'setup-evil)