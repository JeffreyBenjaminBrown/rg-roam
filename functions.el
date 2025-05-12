(defun return-id-under-point ()
  "Extract the ID from an org-mode link at point.
If point is on a link, return the ID.
Otherwise, print 'not a link' and return nil."
  (interactive)
  (let* ((line (thing-at-point 'line t))
         (line-beginning (line-beginning-position))
         (point-in-line (- (point) line-beginning))
         (start-pos 0)
         (found-id nil))

    ;; Search for all org ID links in the current line
    (while (and (not found-id)
                (string-match "\\[\\[id:\\([a-f0-9]\\{8\\}-[a-f0-9]\\{4\\}-[a-f0-9]\\{4\\}-[a-f0-9]\\{4\\}-[a-f0-9]\\{12\\}\\)\\]\\[.*?\\]\\]" line start-pos))
      (let ((match-start (+ line-beginning (match-beginning 0)))
            (match-end (+ line-beginning (match-end 0))))
        
        ;; Check if point is within this link
        (if (and (<= match-start (point)) (<= (point) match-end))
            (setq found-id (match-string 1 line))
          ;; If not in this link, continue searching from after this match
          (setq start-pos (match-end 0)))))
    
    ;; Return the ID or print a message
    (if found-id
        (progn
          (when (called-interactively-p 'any)
            (message "Found ID: %s" found-id))
          found-id)
      (message "Not a link")
      nil)))

(defun org-roam-link-targets (id &optional directory)
  "Find files containing properties buckets with the given ID.
Takes an ID string and displays an interactive grep results buffer.
Optional DIRECTORY specifies where to search (defaults to current buffer's directory).
Only searches .org files."
  (unless id
    (error "No ID provided"))
  (when ;; "prevents command injection" --claude
      (not (string-match-p "^[a-f0-9]\\{8\\}-[a-f0-9]\\{4\\}-[a-f0-9]\\{4\\}-[a-f0-9]\\{4\\}-[a-f0-9]\\{12\\}$" id))
    (error "Invalid ID format: %s" id))
  (let ((search-dir (or directory
                        (file-name-directory buffer-file-name)
                        default-directory)))
    (rgrep (format ":ID:[[:space:]]*%s" id) "*.org" search-dir)))

(defun visit-org-roam-link-target (&optional id-arg)
  "Visit the target of an org-roam link.
If ID is provided, find files containing that ID.
Otherwise, extract the ID from the link at point using `return-id-under-point`.
Prompts the user for a folder to search, suggesting the current one."
  (interactive)
  (let ((id (or id-arg (return-id-under-point))))
    (unless id
      (error "No ID provided or found at point"))
    (let ( ( search-dir
	     ( read-directory-name 
               "Directory to search: " 
               (or (file-name-directory buffer-file-name)
                   default-directory))))
      (org-roam-link-targets id search-dir))))


