(load "~/org-roam/rescue/property-drawer-id.el")
(load "~/org-roam/rescue/more.el")

(defun return-id ()
  "Extract the ID under point, either from a link or properties drawer.
First tries to extract ID from a link at point.
If no link ID is found, tries to extract from properties drawer.
Returns the ID if found, nil otherwise."
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
  "Visit the target of an org-roam link.
If ID is provided, find files containing that ID.
Otherwise, extract the ID from the link at point using `return-id-from-link-under-point`.
Prompts the user for a folder to search, suggesting the current one."
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

(defun make-org-roam-link (id label)
  "Create an org-roam link with the specified ID and LABEL.
If region is active, use the selected text as the label.
Otherwise, prompt for the label. If the label contains a newline, the operation is canceled."
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
