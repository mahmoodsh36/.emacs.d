(require 'proced) ;; needed for my-kill-process

(defun my-kill-process ()
  (interactive)
  (let* ((process-list (proced-process-attributes))
         (names (mapcar (lambda (process-alist) (alist-get 'comm process-alist)) process-list))
         (pids (mapcar (lambda (process-alist) (alist-get 'pid process-alist)) process-list))
         ;; both for pids and names, concat them
         (both (mapcar (lambda (entry) (format "%s \t %s" (car entry) (cdr entry))) (cl-pairlis pids names)))
         (picked (completing-read "process" both))
         (picked-pid (car (split-string picked "\t")))
         ;; we dont need the buffer that pops up with the output of async-shell-command,
         ;; this is a hack to disable it
         (display-buffer-alist
          (append display-buffer-alist
                  (list (cons shell-command-buffer-name-async
                              #'display-buffer-no-window)))))
    (async-shell-command (format "kill -9 %s" picked-pid))))

(defun submit-for-single-cand ()
  (message "GOT %s" vertico--total)
  (when (= 1 vertico--total)
    ;; (2) exit if only a single candidate
    (vertico-exit)))

(defun my-yas-prompt (templates &optional prompt)
  (when templates
    (setq templates
          (sort templates #'(lambda (t1 t2)
                              (< (length (yas--template-name t1))
                                 (length (yas--template-name t2))))))
    (cl-some (lambda (fn)
               (minibuffer-with-setup-hook
                   (lambda ()
                     ;; (1) adjust completion style to achieve desired matching
                     (add-hook 'post-command-hook 'submit-for-single-cand t t))
                 (funcall fn (or prompt "Choose a snippet: ")
                          templates
                          #'yas--template-key)))
             yas-prompt-functions)))

(defun my-yas-insert ()
  (interactive)
  (let* ((templates (yas--all-templates (yas--get-snippet-tables)))
         (yas--current-template
          (and templates
               (or (and (cl-rest templates) ;; more than one template for same key
                        (my-yas-prompt templates))
                   (car templates))))
         (where (if (region-active-p)
                    (cons (region-beginning) (region-end))
                  (cons (point) (point)))))
    (if yas--current-template
        (yas-expand-snippet yas--current-template (car where) (cdr where))
      (yas--message 1 "No snippets can be inserted here!"))))

(provide 'setup-other)
