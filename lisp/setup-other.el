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

(provide 'setup-other)
