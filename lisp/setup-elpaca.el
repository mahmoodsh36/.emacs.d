;; disable package.el, needed for straight.el or elpaca to work properly with use-package
(setq package-enable-at-startup nil)

;; setup elpaca package manager
(defvar elpaca-installer-version 0.6)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))
(elpaca elpaca-use-package
  ;; enable use-package :ensure support for elpaca.
  (elpaca-use-package-mode)
  (setq use-package-always-ensure t))

;; block until current queue processed.
(elpaca-wait)

;; (setq lexical-binding t)
;; (elpaca-test
;;   :early-init (setq elpaca-menu-functions '(elpaca-menu-org))
;;   :init (elpaca (org :remotes ("tecosaur" :repo "https://git.tecosaur.net/tec/org-mode.git"
;;                                :branch "dev")
;;                      :files
;;                      (:defaults "etc")))
;;   (elpaca-wait)
;;   (with-current-buffer (elpaca-info 'org)
;;     (message "%s" (buffer-substring-no-properties (point-min) (point-max)))))

(defun join-path (&rest paths)
  "join file paths using a forward slash"
  (let ((mypath (car paths)))
    (dolist (path (cdr paths))
      (setq mypath (concat mypath "/" path)))
    (file-truename mypath)))

(defun kill-all-buffers ()
  "kill all buffers excluding internal buffers (buffers starting with a space)"
  (interactive)
  (setq list (buffer-list))
  (while list
    (let* ((buffer (car list))
           (name (buffer-name buffer)))
      (and name
           (not (string-equal name ""))
           (/= (aref name 0) ?\s)
           (kill-buffer buffer)))
    (setq list (cdr list))))

(defun run-command-show-output (cmd)
  "run shell command and show continuous output in new buffer"
  (interactive)
  (progn
    (start-process-shell-command cmd cmd cmd)
    (display-buffer cmd)
    (end-of-buffer-other-window nil)))

(defun yas-delete-if-empty ()
  "function to remove _{} or ^{} fields, used by some of my latex yasnippets"
  (interactive)
  (point-to-register 'my-stored-pos)
  (save-excursion
    (while (re-search-backward "\\(_{}\\)" (line-beginning-position) t)
      (progn
        (replace-match "" t t nil 1)
        (jump-to-register 'my-stored-pos)))
    (while (re-search-backward "\\(\\^{}\\)" (line-beginning-position) t)
      (progn
        (replace-match "" t t nil 1)
        (jump-to-register 'my-stored-pos)))))

(defun message-no-format (msg)
  "invoke 'message' without it invoking 'format' (not really)"
  (message "%s" msg))

(provide 'setup-elpaca)