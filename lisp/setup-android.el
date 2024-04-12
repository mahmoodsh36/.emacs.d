;; stuff that is specifically related to emacs on android

(defun is-android-system ()
  (or (string-equal system-type "android")
      (eq (let ((inhibit-message t)) (shell-command "command -v termux-setup-storage" nil)) 0)))

;; for termux binaries on android
(when (is-android-system)
  (setenv "PATH" (format "%s:%s" "/data/data/com.termux/files/usr/bin"
                         (getenv "PATH")))
  (setenv "LD_LIBRARY_PATH" (format "%s:%s"
                                    "/data/data/com.termux/files/usr/lib"
                                    (getenv "LD_LIBRARY_PATH")))
  (push "/data/data/com.termux/files/usr/bin" exec-path))

;; disable native comp on android to prevent exhausting the cpu, doesnt work?
(when (is-android-system)
  (setq native-comp-speed -1))

;; some annoying error messages on android's termux keep interfering with shell-command-to-string, redefine it here to discard all error messages
(when (is-android-system)
  (defalias 'shell-command-to-string 'shell-command-to-string-no-stderr)
  (defalias 'call-process-shell-command 'my-call-process-shell-command))

(defun my-call-process-shell-command (cmd &optional infile buffer display)
  (with-current-buffer buffer
    (insert (shell-command-to-string-no-stderr cmd))
    0))

(provide 'setup-android)