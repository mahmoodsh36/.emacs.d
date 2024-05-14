;; configure/setup some keybindings

(led-kbd "d w" (lambda () (interactive) (dired "~/dl/")))
(led-kbd "d a" (lambda () (interactive) (dired "~/data/")))
(led-kbd "d l" (lambda () (interactive) (dired (get-latex-cache-dir-path))))
(led-kbd "d b" (lambda () (interactive) (dired *brain-dir*)))
(led-kbd "d r" (lambda () (interactive) (dired (from-brain "/resources"))))
(led-kbd "d h" (lambda () (interactive) (dired "~/")))
(led-kbd "d p" (lambda () (interactive) (dired "~/p/")))
(led-kbd "d d" 'dired)
(led-kbd "d c" (lambda () (interactive) (dired default-directory)))
(led-kbd "d o" (lambda () (interactive) (dired "~/brain/out/")))
(led-kbd "d g" (lambda () (interactive) (dired "~/work/blog/")))
(led-kbd "d m" (lambda () (interactive) (dired *music-dir*)))
(led-kbd "f f" 'find-file)
(led-kbd "f v" 'find-alternate-file)
(led-kbd "d j" 'dired-jump)
(led-kbd "f s" 'sudo-find-file)
(led-kbd *leader-key* #'execute-extended-command)
(led-kbd "b k" 'kill-this-buffer)
;; if we manually kill the buffer it doesnt save eshell command history
(led-kbd "b k" (lambda () (interactive) (run-this-in-eshell "exit")) :keymaps 'eshell-mode-map)
(led-kbd "b k" 'sly-quit-lisp :keymaps 'sly-repl-mode)
(led-kbd "b K" 'kill-buffer-and-window)
;; (general-define-key :keymaps 'override (led "b a")
;;                     (lambda ()
;;                       (interactive)
;;                       (kill-all-buffers)
;;                       (switch-to-buffer "*scratch*")))
(led-kbd "b s" 'switch-to-buffer)
(led-kbd "b b" 'bookmark-jump)
(led-kbd "x" 'eval-defun  :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map))
(led-kbd "s g" 'deadgrep)
(led-kbd "s G" 'consult-ripgrep)
(led-kbd "x" 'org-ctrl-c-ctrl-c :keymaps 'org-mode-map)
(led-kbd "x" 'compile-current-document :keymaps '(TeX-mode-map tex-mode-map latex-mode-map) )
(led-kbd "e" (lambda () (interactive) (find-file user-init-file)))
(led-kbd "p" 'projectile-command-map)
;; (general-define-key :keymaps 'TeX-mode-map (led "c" 'compile-sagetex)
(led-kbd "r k" 'org-insert-link :keymaps 'org-mode-map)
;; (general-define-key :keymaps 'org-mode-map (led "z"
;;                     (lambda ()
;;                       (interactive)
;;                       (if (not xenops-mode)
;;                           (xenops-mode)
;;                         (xenops-render))))
(led-kbd "z" 'org-latex-preview :keymaps 'org-mode-map)
(led-kbd "a i" #'org-timestamp)
(led-kbd "a I"
         (lambda ()
           (interactive)
           (org-insert-time-stamp (current-time) t))) ;; timestamp with full time
;;(define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)
;; (general-define-key :keymaps 'override (led "r t" 'org-roam-buffer-toggle)
;; (led-kbd "r f"
;;          (lambda ()
;;            (interactive)
;;            (org-roam-node-find
;;             nil
;;             nil
;;             (lambda (my-roam-node)
;;               (and my-roam-node
;;                    (not (equal (org-roam-node-title my-roam-node) "")))))))
;; (led-kbd "r n" (lambda () (interactive) (org-roam-capture nil "n")))
;; (led-kbd "r i" 'denote-insert-link)
;; (led-kbd "r i" #'my-notes-insert-link-by-title)
(led-kbd "r i" #'blk-insert)
(led-kbd "r m" #'denote-rename-file)
;; (led-kbd "r c" 'org-id-get-create)
(led-kbd "r o" #'blk-open-at-point)
(led-kbd "r a" #'org-attach :keymaps 'org-mode-map)
(led-kbd "r A" #'org-attach-open :keymaps 'org-mode-map)
(led-kbd "r l" #'org-roam-alias-add :keymaps 'org-mode-map)
(led-kbd "r w" #'org-roam-tag-add :keymaps 'org-mode-map)
(led-kbd "r W" #'org-roam-tag-remove :keymaps 'org-mode-map)
;; (led-kbd "r q"
;;          (lambda ()
;;            (interactive)
;;            (org-roam-capture-no-title-prompt nil "k"))) ;; SPC r q - quick note
;; (general-define-key :keymaps 'override "SPC r h"
;;                     (lambda ()
;;                       (interactive)
;;                       (org-roam-capture nil "t")))
(led-kbd "r x" (lambda () (interactive) (export-current-buffer :html-p t)) :keymaps 'org-mode-map)
(led-kbd "r X" #'export-all-org-files-to-html)
;; (led-kbd "r d" (lambda () (interactive) (export-current-buffer :pdf-p t)) :keymaps 'org-mode-map)
(led-kbd "r d"
         (lambda () (interactive)
           (my-org-to-pdf)
           (switch-to-buffer-other-window "latex"))
         :keymaps 'org-mode-map)
(led-kbd "r u" #'org-latex-preview-clear-cache :keymaps 'org-mode-map)
(led-kbd "r e" 'org-babel-tangle :keymaps 'org-mode-map)
(led-kbd "r E" 'org-babel-tangle-file :keymaps 'org-mode-map)
(led-kbd "r g"
         (lambda ()
           (interactive)
           (find-file (from-brain "/bib.bib"))))
(led-kbd "r v" 'org-babel-execute-buffer :keymaps 'org-mode-map)
(led-kbd "r r" 'org-redisplay-inline-images :keymaps 'org-mode-map)
(led-kbd "r P" 'org-set-property :keymaps 'org-mode-map)
(led-kbd "r z" 'org-add-note :keymaps 'org-mode-map)
(led-kbd "r p" #'toggle-latex-previews-and-render-current-buffer)
(led-kbd "r p" #'toggle-latex-previews-and-render-current-buffer :keymaps 'org-mode-map)
;; (general-define-key :keymaps 'override "/" 'swiper)
(led-kbd "c" "C-c C-c" :keymaps 'org-mode-map)
(led-kbd "a N" #'today-entry-text-simple)

;; keys to search for files
(led-kbd "f b"
         (lambda () (interactive) (search-open-file-in-emacs *brain-dir* ".*\\(pdf\\|tex\\|doc\\|mp4\\|png\\|org\\)")))
;; (led-kbd "F b"
;;          (lambda () (interactive) (search-open-file-in-emacs *brain-dir* ".*\\(pdf\\|tex\\|doc\\|org\\)")))
(led-kbd "f h" (lambda () (interactive) (search-open-file "./" ".*")))

(led-kbd "f n"
         (lambda () (interactive) (find-file "~/work/nixos/configuration.nix")))
(led-kbd "f c"
         (lambda () (interactive) (find-file "~/.emacs.d/common.sty")))

;; (define-key evil-normal-state-map (kbd "SPC f d")
;;             (lambda () (interactive) (search-open-file "~/data" "")))
;; (define-key evil-normal-state-map (kbd "SPC F d")
;;             (lambda () (interactive)
;;               (search-open-file-in-emacs "~/data" "")))

;; music keys
(require 'setup-music)
(led-kbd "f m" #'play-file)
;; play artist
(led-kbd "m a" #'play-artist)
;; play album by artist name + album name
(led-kbd "m b" #'play-album)
;; play album
;; (led-kbd "m B" #'play-album-2)
;; open music table file
(led-kbd "m f"
         (lambda ()
           (interactive)
           (find-file "/home/mahmooz/brain/notes/20231010T211129--music-table__music.org")))
;; open artist's last.fm page
(led-kbd "m l" #'open-lastfm-page)

;; keybinding to evaluate math expressions
;; (general-define-key :states '(normal motion) :keymaps 'override "SPC m"
;;                     (lambda ()
;;                       (interactive)
;;                       (call-interactively 'calc-latex-language)
;;                       (let ((result (calc-eval (buffer-substring-no-properties (region-beginning) (region-end)))))
;;                         (end-of-line)
;;                         (insert " ")
;;                         (insert result))))

;; general keys
;; (general-define-key :states 'normal :keymaps 'override "SPC m" 'man)
(led-kbd "'" (general-simulate-key "C-c '"))
(led-kbd "w m"
         (lambda () (interactive)
           (when window-system (set-frame-size (selected-frame) 180 50))))
;; (led-kbd "s d" (lambda () (interactive) (switch-to-theme 'stimmung-themes-dark) (set-themed-pdf t)))
;; (led-kbd "s l" (lambda () (interactive) (switch-to-theme 'stimmung-themes-light) (set-themed-pdf nil)))
;; (led-kbd "s d" (lambda () (interactive) (switch-to-theme 'ef-melissa-dark) (set-themed-pdf t)))
(defun switch-to-dark-theme ()
  (interactive)
  (switch-to-theme 'ef-autumn)
  (set-themed-pdf t))
(defun switch-to-light-theme ()
  (interactive)
  (switch-to-theme 'ef-melissa-light)
  (set-themed-pdf nil))
(led-kbd "s d" #'switch-to-dark-theme)
(led-kbd "s l" #'switch-to-light-theme)
(led-kbd "s e" 'eshell)
;; (general-define-key :keymaps 'override (led "s g") 'magit)
(led-kbd "s i"
         (lambda ()
           (interactive)
           (let ((current-prefix-arg '-)) (call-interactively 'sly))))
(led-kbd "s r" #'eat);;'vterm)
(led-kbd "s s" #'shell)
(led-kbd "s S" #'shell-create-name-incremented)
(led-kbd "s w" #'term-create-name-incremented)
(led-kbd "u" (general-simulate-key "C-u"))
(led-kbd "o l" 'avy-goto-line)
(led-kbd "o c" 'avy-goto-char)
;; (led-kbd "s s" 'spotify-lyrics)
;; (led-kbd "s w" 'open-spotify-lyrics-file)
(led-kbd "s t" #'consult-theme)
(led-kbd "s k" (lambda () (interactive) (start-process-shell-command "zathura" "zathura" (format "zathura '%s'" buffer-file-name))))
(led-kbd "s q" 'calc)
(led-kbd "s u" 'copy-file-path)

;; agenda keys
(led-kbd "a a" #'my-org-agenda)
(led-kbd "a q" #'my-org-ql-agenda)
(general-define-key :states 'normal :keymaps 'org-agenda-mode-map "q" 'org-agenda-exit)
(general-define-key :states 'normal :keymaps 'org-agenda-mode-map "[" 'org-agenda-earlier)
(general-define-key :states 'normal :keymaps 'org-agenda-mode-map "]" 'org-agenda-later)
(led-kbd "a s" #'org-schedule)
(led-kbd "a d" #'org-deadline)
(led-kbd "a j" #'org-clock-in :keymaps 'org-mode-map)
(led-kbd "a J" #'org-clock-in-last)
(led-kbd "a k" #'org-clock-out)
(led-kbd "a b" #'org-clock-cancel)
(led-kbd "a p" #'org-clock-display :keymaps 'org-mode-map)
(led-kbd "a t" (lambda () (interactive) (org-capture nil "t")))
(led-kbd "a c" #'org-todo :keymaps 'org-mode-map)
(led-kbd "a C" (lambda () (interactive) (execute-kbd-macro "C-- 1 C-c C-t")) :keymaps 'org-mode-map) ;; modify recurring task "forever"
(led-kbd "" (make-sparse-keymap) :keymaps 'org-agenda-mode-map) ;; needed for the next one
(led-kbd "a c" #'org-agenda-todo :keymaps 'org-agenda-mode-map)
(led-kbd "a n" 'today-entry)
(led-kbd "a o" 'open-todays-file)
(led-kbd "s n" 'yas-new-snippet)
(led-kbd "s v" 'yas-visit-snippet-file)
(led-kbd "s h" 'yas-insert-snippet)
(led-kbd "s a" 'dictionary-search)
(led-kbd "r s" 'org-cite-insert :keymaps 'org-mode-map)
;; key to clear the screen in eshell
(defun run-this-in-eshell (cmd)
  "runs the command 'cmd' in eshell."
  (with-current-buffer "*eshell*"
    (end-of-buffer)
    (eshell-kill-input)
    (message (concat "running in eshell: " cmd))
    (insert cmd)
    (eshell-send-input)
    (end-of-buffer)
    (eshell-bol)
    (yank)))
(add-hook 'eshell-mode-hook
          (lambda ()
            (led-kbd "c" (lambda () (interactive) (run-this-in-eshell "clear 1")) :keymaps 'local)))
(led-kbd "x" 'eshell-interrupt-process :keymaps 'eshell-mode-map)
(led-kbd "x" 'sly-compile-defun :keymaps 'lisp-mode-map)
(led-kbd "c" 'sly-eval-buffer :keymaps 'lisp-mode-map)
(led-kbd "z"
         (lambda ()
           (interactive)
           (sly-end-of-defun)
           (call-interactively 'sly-eval-last-expression-in-repl))
         :keymaps 'lisp-mode-map)
(led-kbd "s e" 'eshell)
(led-kbd "s m" 'woman)

;; language-specific keybindings
(led-kbd "l i" 'sly-repl-inspect :keymaps 'lisp-mode-map)
(led-kbd "l i" 'sly-repl-inspect :keymaps 'sly-repl-mode-map)
(led-kbd "l s" 'sly-inspect-presentation-at-point :keymaps 'sly-repl-mode-map)
(led-kbd "c" 'eval-buffer :keymaps 'emacs-lisp-mode-map)

;; jummp to definition of thing at point
(led-kbd "l d" 'xref-find-definitions :keymaps 'prog-mode-map)

;; common lisp/sly
(led-kbd "l d" 'sly-documentation-lookup :keymaps '(lisp-mode-map sly-mrepl-mode-map))

;; julia
(led-kbd "s j" 'julia-snail)

;; python
(led-kbd "s p" 'run-python)
(led-kbd "x" 'python-shell-send-defun :keymaps 'python-ts-mode-map)
(led-kbd "l x" 'python-shell-send-defun :keymaps 'python-ts-mode-map)
(led-kbd "l t" 'python-shell-send-statement :keymaps 'python-ts-mode-map)
(led-kbd "c" 'python-shell-send-buffer :keymaps 'python-ts-mode-map)
(led-kbd "l b" 'python-shell-send-buffer :keymaps 'python-ts-mode-map)

;; general keys for programming languages i guess
(led-kbd "l f" 'apheleia-format-buffer :keymaps 'prog-mode-map)

;;sagemath
(led-kbd "b k" 'comint-quit-subjob :keymaps 'sage-shell-mode-map)

;; elisp
(led-kbd "c" 'eval-buffer :keymaps 'emacs-lisp-mode-map)

;; other keybinds
(general-define-key :states 'normal :keymaps 'override "C-S-k" 'kill-whole-line)
;; (general-define-key :keymaps 'override "m-ret"
;;                     (lambda ()
;;                       (interactive)
;;                       (end-of-line)
;;                       (call-interactively 'newline))) ;; call newline interactively for proper indentation in code
;; (led-kbd "w v" #'split-window-right)
;; (led-kbd "w s" #'split-window-below)
;; (led-kbd "w o" #'other-window)
;; (led-kbd "w c" #'delete-window)
;; (led-kbd "w t" #'recenter)
;; (led-kbd "w f" #'windmove-right)
;; (led-kbd "w b" #'windmove-left)
;; (led-kbd "w n" #'windmove-down)
;; (led-kbd "w p" #'windmove-up)
(led-kbd "v" #'open-current-document-this-window :keymaps '(org-mode-map TeX-mode-map latex-mode-map))
(general-define-key :states 'normal :keymaps 'override
                    "M-o" ;; new line without breaking current line
                    (lambda ()
                      (interactive)
                      (end-of-line)
                      (newline-and-indent)))
(general-define-key :states 'normal :keymaps 'override
                    "M-O" ;;"M-S-o" ;; new line above current line without breaking it
                    (lambda ()
                      (interactive)
                      (move-beginning-of-line nil)
                      (newline-and-indent)
                      (forward-line -1)
                      (indent-according-to-mode)))
;; (global-set-key (kbd "M-S-o"))
(general-define-key :states 'normal :keymaps 'override "M-z" #'zap-up-to-char)

;; i hate backspace and return
(define-key input-decode-map [?\C-m] [C-m]) ;; so that c-m wouldnt be attached to <return> anymore
(global-set-key (kbd "<C-m>") #'newline)
;; (keymap-global-set (kbd "<C-m>") #'newline) ;; not sure why this doesnt work like above
;; (keymap-global-unset "<RET>")
;; (keymap-global-unset "<backspace>")
(keymap-global-set "C-S-d" #'backward-delete-char-untabify)
(keymap-global-set "M-D" #'backward-kill-word)
(keymap-global-set "C-M-S-k" #'backward-kill-sexp)
(keymap-global-set "C-c c" #'recenter)
;; (general-define-key :keymaps 'override "C-'" #'save-buffer)

;; c-j in repl's to emulate return
(define-key comint-mode-map (kbd "C-j") #'comint-send-input)
;; spc g to cancel like C-g
                                        ; (define-key key-translation-map (kbd (led "g")) (kbd "C-g"))

;; evaluate and insert without truncating output
(general-define-key
 "C-x C-S-e"
 (lambda ()
   (interactive)
   (let ((current-prefix-arg (list 0)))
     (call-interactively 'eros-eval-last-sexp))))

(led-kbd "t" #'treemacs)
;; (keymap-global-set "C-a" #'back-to-indentation)
;; (keymap-global-set "M-m" #'beginning-of-line)

;; multiple cursors keys
;; (keymap-global-set (led ", e") #'mc/edit-lines)
;; (keymap-global-set (led ", n") #'mc/mark-next-like-this)
;; (keymap-global-set (led ", p") #'mc/mark-previous-like-this)

(define-key org-mode-map (kbd "M-N") #'org-metadown)
(define-key org-mode-map (kbd "M-P") #'org-metaup)

;; remap the help map, use c-h for something else
(led-kbd "h" help-map)

;; (keymap-global-set "C-h" #'indent-according-to-mode)

;; perspective keys
;; (led-kbd "s 1" (lambda () (interactive) (persp-switch "main")))
;; (led-kbd "s 2" (lambda () (interactive) (persp-switch "college")))
;; (led-kbd "s 3" (lambda () (interactive) (persp-switch "agenda")))
(led-kbd "0" (lambda () (interactive) (persp-switch "agenda")))
(led-kbd "1" (lambda () (interactive) (persp-switch "main")))
(led-kbd "2" (lambda () (interactive) (persp-switch "work2")))
(led-kbd "3" (lambda () (interactive) (persp-switch "work3")))
(led-kbd "4" (lambda () (interactive) (persp-switch "work4")))
(led-kbd "s c" #'persp-switch)

(led-kbd "s z" #'zeal-at-point)
(led-kbd "s f" #'devdocs-lookup)

;; bind esc to c-g to make it cancel stuff
;; (general-define-key :states 'override :keymaps 'override "ESC" (general-simulate-key "C-g"))
;; (general-define-key :states 'override :keymaps 'override "<escape>" (general-simulate-key "C-g"))

;; c-x c-l to complete line like vim
(defun my-expand-lines ()
  (interactive)
  (let ((hippie-expand-try-functions-list
         '(try-expand-line)))
    (call-interactively 'hippie-expand)))
(general-define-key "C-x C-l" 'my-expand-lines)

(global-set-key (kbd "C-x k") 'kill-this-buffer-volatile)
(global-set-key (kbd "C-x K") 'kill-buffer-and-window)

(led-kbd "s b" #'ein:run)
(led-kbd "s o" #'insert-random-string)
(led-kbd "s y" #'avy-goto-char)
(led-kbd "s x" #'save-buffers-kill-terminal)

;; denote
(setq denote-templates
      '((note . "#+title: ")
        (memo . "* Some heading")))
(led-kbd "r n" #'denote-create-note)
(led-kbd "r N" #'denote-type)
;; (led-kbd "r f" #'denote-open-or-create)
;; (led-kbd "r f" #'my-notes-open-by-title)
;; (led-kbd "r f" #'my-notes-open-all)
;; (led-kbd "r f" #'my-notes-open)
;; (led-kbd "r F" #'my-notes-open-1)
(led-kbd "r f" #'blk-find)
(led-kbd "r F" #'blk-find-with-ivy)
(global-set-key (kbd "<f6>") #'my-notes-open)

(led-kbd "r c q" (lambda () (interactive) (org-capture nil "q")))
(led-kbd "r c i" (lambda () (interactive) (org-capture nil "i")))
;; (led-kbd "r c t" (lambda () (interactive) (org-capture nil "t")))

;; dired keys
(defun my-xdg-open-file ()
  "in dired, open the file named on this line."
  (interactive)
  (let* ((file (if (derived-mode-p 'dired-mode) (dired-get-filename nil t) buffer-file-name)))
    (call-process "xdg-open" nil 0 nil file)))
(led-kbd "s ." 'my-xdg-open-file)

;; pdf tools keys, make j,k navigation update the location bookmarked (hey atleast it works)
(general-define-key :states 'normal :keymaps 'pdf-view-mode-map "j"
                    (lambda ()
                      (interactive)
                      (call-interactively 'pdf-view-next-line-or-next-page)
                      (brds/pdf-set-last-viewed-bookmark)))
(general-define-key :states 'normal :keymaps 'pdf-view-mode-map "k"
                    (lambda ()
                      (interactive)
                      (call-interactively 'pdf-view-previous-line-or-previous-page)
                      (brds/pdf-set-last-viewed-bookmark)))

;; crashes
(defun run-from-zsh-history ()
  (interactive)
  (let ((cmd (completing-read "command" (split-string (shell-command-to-string "cat ~/brain/zsh_history | grep -v '; exit$' | tac | cut -d ';' -f2-") "\n"))))
    (when cmd
      ;; (start-process-shell-command "shell" nil (format "terminal_with_cmd.sh \"%s; exec zsh -i\"" (shell-quote-argument cmd))))))
      (start-process-shell-command "shell" nil (format "terminal_with_cmd.sh \"%s; exec zsh -i\"" cmd)))))

(defun run-from-zsh-history-with-emacs-term ()
  (interactive)
  (let ((cmd (completing-read "command" (split-string (shell-command-to-string "cat ~/brain/zsh_history | grep -v '; exit$' | tac | cut -d ';' -f2-") "\n"))))
    (when cmd
      (new-shell-with-cmd cmd)
      ;; (start-process-shell-command "shell" nil cmd)
      )))

(led-kbd "; x" #'consult-complex-command)
(led-kbd "; s" #'run-from-zsh-history-with-emacs-term)
(led-kbd "; S" #'run-from-zsh-history)
(led-kbd "; f" #'consult-find)
(led-kbd "; g" #'consult-grep)
(led-kbd "; m" (lambda ()
                 (interactive)
                 (let ((notmuch-search-oldest-first nil))
                   (notmuch-search "tag:unread"))))
(led-kbd "; r" (lambda () (interactive) (find-alternate-file buffer-file-name))) ;; reload file
(led-kbd "; t" (lambda () (interactive) (today-entry "THOUGHT")))
(led-kbd "; e" (lambda () (interactive) (today-entry "FEELING")))

(global-set-key (kbd "C-;") #'flyspell-correct-wrapper)

;; annoyingly some modes dont handle it newline indentation properly..
;; (general-define-key :states 'normal :keymaps 'prog-mode-hook "o" 'my-insert-newline-same-indentation)

(provide 'setup-keys)