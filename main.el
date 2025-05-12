(load "~/org-roam/rescue/property-drawer-id.el")
(load "~/org-roam/rescue/more.el")

(defun return-id ()
  "Extract the ID under point if there is one -- from a link or a properties drawer."
  (interactive)
  (let ((id nil))
    (setq id (return-id-from-link-under-point))
    (when (not id)
      (setq id (return-id-from-properties-drawer)))
    (if id
        (progn
          (when (called-interactively-p 'any)
            (message "Found ID: %s" id))
          (kill-new id)
          id)
      (message "No ID found under point.")
      nil)))

(defun visit-org-roam-link-target (&optional id-arg)
  "Show potential targets an id might refer to -- the id from the link under point, or the id provided as an argument. Prompts the user for a folder to search, suggesting the current one."
  (interactive)
  (let ((id (or id-arg (return-id))))
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
That is, open an unsaved buffer in the current folder with:
- a properties drawer containing a random ID
- a title line with the cursor positioned after"
  (interactive)
  (let ((new-id (random-uid)))
    (let ((buffer (generate-new-buffer "untitled.org")))
      (with-current-buffer buffer
        (insert (format
		 ":PROPERTIES:\n:ID:       %s\n:END:\n#+title: "
		 new-id))
        (org-mode)
        (goto-char (point-max)))
      (switch-to-buffer buffer)
      (setq-local buffer-offer-save t))))

(defun make-org-roam-link (id label)
  "Create an org-roam link with the specified ID and LABEL.
If region is active, use the selected text as the label.
If the label contains a newline, abort."
  (interactive
   (let* ((id-input (read-string "Enter ID: "))
          (label-input (if (use-region-p)
                           (buffer-substring-no-properties
                            (region-beginning) (region-end))
                         (read-string "Enter label: "))))
     (list id-input label-input)))
  (if (string-match-p "\n" label)
      (error "Label cannot contain a newline")
    (progn
      (when (use-region-p)
        (delete-region (region-beginning) (region-end)))
      (insert (format "[[id:%s][%s]]" id label))
      (format "[[id:%s][%s]]" id label))))

(defun insert-properties-drawer-with-id ()
  "Insert a properties drawer with a random ID after the current line."
  (interactive)
  (let ((id (format "%s" (random-uid))))
    (save-excursion
      (end-of-line)
      (open-line 1)
      (forward-line 1)
      (insert ":PROPERTIES:\n:ID: " id "\n:END:"))
    (message "Inserted properties drawer with ID: %s" id)
    (kill-new id)))
