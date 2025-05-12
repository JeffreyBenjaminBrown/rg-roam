(defun return-id-from-properties-at-point ()
  "Extract the ID from a properties drawer at point.
If point is within a properties drawer, return the ID.
Otherwise, print 'not in a properties drawer' and return nil."
  (interactive)
  (let (drawer-start drawer-end drawer-text id in-drawer)
    (save-excursion
      (setq in-drawer nil) 
      (if (re-search-backward
	   "^[[:space:]]*:PROPERTIES:[[:space:]]*$" nil t)
          (progn
            (setq drawer-start (point))
            ;; Search forward for the end of the drawer from drawer start
            (goto-char drawer-start)
            (if (re-search-forward "^[[:space:]]*:END:[[:space:]]*$" nil t)
                (progn
                  (setq drawer-end (point))
                  ;; Check if point is within the drawer
                  (setq in-drawer (and (<= drawer-start (point))
                                        (<= (point) drawer-end))))
              ;; No END found
              (setq drawer-start nil)))
        ;; No PROPERTIES found looking backward
        (setq drawer-start nil)))
    
    ;; If we're not in a drawer, try searching forward for a drawer start
    (when (not in-drawer)
      (save-excursion
        (if (re-search-forward "^[[:space:]]*:PROPERTIES:[[:space:]]*$" nil t)
            (progn
              (setq drawer-start (match-beginning 0))
              (if (re-search-forward "^[[:space:]]*:END:[[:space:]]*$" nil t)
                  (progn
                    (setq drawer-end (point))
                    ;; Check if point is within the drawer
                    (setq in-drawer (and (<= drawer-start (point))
                                         (<= (point) drawer-end))))
                ;; No END found
                (setq drawer-start nil)))
          ;; No PROPERTIES found looking forward
          (setq drawer-start nil))))
    
    ;; If we're in a drawer, extract the ID
    (when in-drawer
      (save-excursion
        (goto-char drawer-start)
        (when (re-search-forward "^[[:space:]]*:ID:[[:space:]]*\\([a-f0-9]\\{8\\}-[a-f0-9]\\{4\\}-[a-f0-9]\\{4\\}-[a-f0-9]\\{4\\}-[a-f0-9]\\{12\\}\\)[[:space:]]*$" drawer-end t)
          (setq id (match-string 1)))))
    
    ;; Return the ID or a message
    (if id
        (progn
          (when (called-interactively-p 'any)
            (message "Found ID: %s" id))
          id)
      (message "Not in a properties drawer with ID")
      nil)))

(defun return-id-from-link-under-point ()
  "Extract the ID from an org-mode link at point.
If point is on a link, return the ID.
Otherwise, print 'not a link' and return nil."
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

(defun visit-org-roam-link-targets-ni (id &optional directory)
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
Otherwise, extract the ID from the link at point using `return-id-from-link-under-point`.
Prompts the user for a folder to search, suggesting the current one."
  (interactive)
  (let ((id (or id-arg (return-id-from-link-under-point))))
    (unless id
      (error "No ID provided or found at point"))
    (let ( ( search-dir
	     ( read-directory-name 
               "Directory to search: " 
               (or (file-name-directory buffer-file-name)
                   default-directory))))
      (visit-org-roam-link-targets-ni id search-dir))))

(defun new-org-roam-file ()
  "Creates a new org-roam file with a random ID.
That is, it opens an unsaved buffer with
  a properties drawer containing a random ID, and
  a title line with the cursor positioned after it."
  (interactive)
  (let ((new-id (format "%04x%04x-%04x-%04x-%04x-%04x%04x%04x"
                        (random 65536) (random 65536)
                        (random 65536)
                        (logior #x4000 (logand #x0fff (random 65536)))
                        (logior #x8000 (logand #x3fff (random 65536)))
                        (random 65536) (random 65536) (random 65536))))
    (let ((buffer (generate-new-buffer "untitled.org")))
      (with-current-buffer buffer
        (insert (format
		 ":PROPERTIES:\n:ID:       %s\n:END:\n#+title: "
		 new-id))
        (org-mode)
        (goto-char (point-max)))
      (switch-to-buffer buffer)
      (setq-local buffer-offer-save t))))
