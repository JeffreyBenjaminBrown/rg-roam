(defun visit-org-roam-link-targets-ni (id &optional directory)
  "Find property lines containing the given ID.
Takes an ID string and displays an interactive grep results buffer.
Optional DIRECTORY specifies where to search (defaults to current buffer's directory).
Only searches .org files.
(The -ni means not interactive.)"
  (unless id
    (error "No ID provided"))
  (when ;; "prevents command injection" --claude
      (not (string-match-p "^[a-f0-9]\\{8\\}-[a-f0-9]\\{4\\}-[a-f0-9]\\{4\\}-[a-f0-9]\\{4\\}-[a-f0-9]\\{12\\}$" id))
    (error "Invalid ID format: %s" id))
  (let ((search-dir (or directory
                        (file-name-directory buffer-file-name)
                        default-directory)))
    (rgrep (format ":ID:[[:space:]]*%s" id) "*.org" search-dir)))

(defun return-id-from-link-under-point ()
  "Extract the ID from an org-mode link at point, if pointis on one."
  (interactive)
  (let (link-start link-end link-text id)
    (save-excursion
      (let ((line-start (line-beginning-position)))
        (if (search-backward "[[" line-start t)
            (setq link-start (point))
          (message "Not a link")
          (setq link-start nil))))
    (when link-start
      (save-excursion
        (let ((line-end (line-end-position)))
          (goto-char link-start)
          (if (search-forward "]]" line-end t)
              (setq link-end (point))
            (message "Not a link")
            (setq link-end nil)))))
    (when (and link-start link-end)
      (setq link-text ( buffer-substring-no-properties
			link-start link-end))
      (if (string-match "\\[\\[id:\\([a-f0-9]\\{8\\}-[a-f0-9]\\{4\\}-[a-f0-9]\\{4\\}-[a-f0-9]\\{4\\}-[a-f0-9]\\{12\\}\\)\\]\\[.*?\\]\\]" link-text)
          (setq id (match-string 1 link-text))
        (message "Not a link")
        (setq id nil)))
    (when id
      (when (called-interactively-p 'any)
        (message "Found ID: %s" id))
      id)))

(defun random-uid ()
    (format "%04x%04x-%04x-%04x-%04x-%04x%04x%04x"
            (random 65536) (random 65536)
            (random 65536)
            (logior #x4000 (logand #x0fff (random 65536)))
            (logior #x8000 (logand #x3fff (random 65536)))
            (random 65536) (random 65536) (random 65536)))
