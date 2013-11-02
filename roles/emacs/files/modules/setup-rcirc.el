;; You can autoload, but at the end of this block we'll
;; connect to two networks anyway.
(require 'rcirc)

(setq rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY" "MODE"))

;; Adjust the colours of one of the faces.
(set-face-foreground 'rcirc-my-nick "blue" nil)

;; Include date in time stamp.
(setq rcirc-time-format "%Y-%m-%d %H:%M ")

;; Auto reconnect
;; Taken from http://www.emacswiki.org/emacs/rcircReconnect
(defun-rcirc-command reconnect (arg)
  "Reconnect the server process."
  (interactive "i")
  (if (buffer-live-p rcirc-server-buffer)
      (with-current-buffer rcirc-server-buffer
        (let ((reconnect-buffer (current-buffer))
              (server (or rcirc-server rcirc-default-server))
              (port (if (boundp 'rcirc-port) rcirc-port rcirc-default-port))
              (nick (or rcirc-nick rcirc-default-nick))
              channels)
          (dolist (buf (buffer-list))
            (with-current-buffer buf
              (when (equal reconnect-buffer rcirc-server-buffer)
                (remove-hook 'change-major-mode-hook
                             'rcirc-change-major-mode-hook)
                (let ((server-plist (cdr (assoc-string server rcirc-server-alist))))
                  (when server-plist
                    (setq channels (plist-get server-plist :channels))
                    (setq password (plist-get server-plist :password))))
                  )))
          (if process (delete-process process))
          (rcirc-connect server port nick
                         nil
                         nil
                         channels
                         password)))))

;;; Attempt reconnection at increasing intervals when a connection is
;;; lost.

(defvar rcirc-reconnect-attempts 0)

;;;###autoload
(define-minor-mode rcirc-reconnect-mode
  nil
  nil
  " Auto-Reconnect"
  nil
  (if rcirc-reconnect-mode
      (progn
        (make-local-variable 'rcirc-reconnect-attempts)
        (add-hook 'rcirc-sentinel-hooks
                  'rcirc-reconnect-schedule nil t))
    (remove-hook 'rcirc-sentinel-hooks
                 'rcirc-reconnect-schedule t)))

(defun rcirc-reconnect-schedule (process &optional sentinel seconds)
  (condition-case err
      (when (and (eq 'closed (process-status process))
                 (buffer-live-p (process-buffer process)))
        (with-rcirc-process-buffer process
          (unless seconds
            (setq seconds (exp (1+ rcirc-reconnect-attempts))))
          (rcirc-print
           process "my-rcirc.el" "ERROR" rcirc-target
           (format "scheduling reconnection attempt in %s second(s)." seconds) t)
          (run-with-timer
           seconds
           nil
           'rcirc-reconnect-perform-reconnect
           process)))
    (error
     (rcirc-print process "RCIRC" "ERROR" nil
                  (format "%S" err) t)))
)

(defun rcirc-reconnect-perform-reconnect (process)
  (when (and (eq 'closed (process-status process))
             (buffer-live-p (process-buffer process))
             )
    (with-rcirc-process-buffer process
      (when rcirc-reconnect-mode
        (if (get-buffer-process (process-buffer process))
            ;; user reconnected manually
            (setq rcirc-reconnect-attempts 0)
          (let ((msg (format "attempting reconnect to %s..."
                             (process-name process)
                             )))
            (rcirc-print process "my-rcirc.el" "ERROR" rcirc-target
                         msg t))
          ;; remove the prompt from buffers
          (condition-case err
              (progn
                (save-window-excursion
                  (save-excursion
                    (rcirc-cmd-reconnect nil)))
                (setq rcirc-reconnect-attempts 0))
            ((quit error)
             (incf rcirc-reconnect-attempts)
             (rcirc-print process "my-rcirc.el" "ERROR" rcirc-target
                          (format "reconnection attempt failed: %s" err)  t)
             (rcirc-reconnect-schedule process))))))))

(add-hook 'rcirc-mode-hook (lambda ()
                             (rcirc-omit-mode)
                             (rcirc-track-minor-mode 1)
                             ;; Keep input line at bottom.
                             (set (make-local-variable 'scroll-conservatively)
                                  8192)
                             (rcirc-reconnect-mode 1)))

(defun rcirc-generate-log-filename (process target)
  (when (not (member ?. (string-to-list target)))
    (concat target
            (format-time-string "_%Y_%m_%d"))))

(defun rcirc-write-log (process sender response target text)
  (when rcirc-log-directory
    (with-temp-buffer
      ;; Sometimes TARGET is a buffer :-(
      (when (bufferp target)
        (setq target (with-current-buffer buffer rcirc-target)))

      ;; Sometimes buffer is not anything at all!
      (unless (or (null target) (string= target ""))
        ;; Print the line into the temp buffer.
        (insert (format-time-string "%Y-%m-%d %H:%M "))
        (insert (format "%-16s " (rcirc-user-nick sender)))
        (unless (string= response "PRIVMSG")
          (insert "/" (downcase response) " "))
        (insert text "\n")

        ;; Append the line to the appropriate logfile.
        (let ((coding-system-for-write 'no-conversion)
              (file-name (rcirc-generate-log-filename process target)))

              (when file-name
                (write-region (point-min) (point-max)
                        (concat rcirc-log-directory "/" file-name)
                        t 'quietly)))))))

(add-hook 'rcirc-print-hooks 'rcirc-write-log)

(defun-rcirc-command join (channel)
  "Join CHANNEL."
  (interactive "sJoin channel: ")
  (let ((channel-key (nth 1 (split-string channel "@")))
        (channel-name (nth 0 (split-string channel "@"))))
    (let ((buffer (rcirc-get-buffer-create process channel-name))
          (channel-query (if channel-key
                             (concat channel-name " " channel-key)
                           channel-name)))

      (rcirc-send-string process (concat "JOIN " channel-query))
      (when (not (eq (selected-window) (minibuffer-window)))
        (switch-to-buffer buffer)))))


(require 'erc-terminal-notifier)

(defun rcirc-osx-notify (process sender response target text)
  (let ((nick (rcirc-nick process)))
    (when (and target
               (not (string= nick sender))
               (not (string= (rcirc-server-name process) sender))
               (or (string-match (regexp-quote nick) text)
                   (and (not (string= (substring target 0 1) "#"))
                        (not (member ?. (string-to-list target))))))
      (erc-terminal-notifier-notify sender text))))

(add-hook 'rcirc-print-functions 'rcirc-osx-notify)

(provide 'setup-rcirc)
