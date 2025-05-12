(load "~/org-roam/rescue/property-drawer-id.el")
(load "~/org-roam/rescue/more.el")

(require 'grep)

(defun rg-roam-find (terms dir)
  "Search DIR for .org files matching the space-separated TERMS provided.
They must appear in that order,
but can be among other intervening characters.
Results are displayed in a `grep-mode' interactive buffer."
  (interactive
   (list (read-string "Search terms (space-separated): ")
         (read-directory-name "Folder to search: " default-directory nil t)))
  (unless (string-match-p "[^[:space:]]" terms)
    (user-error "No search terms supplied"))
  (let* ((default-directory (file-name-as-directory (expand-file-name dir)))
         (words (split-string terms "[[:space:]]+" t))
         ;; Build a flexible PCRE pattern that allows anything in between terms
         (pattern
          (concat "^\\s*(:(ROAM_ALIASES:)|(#\\+title:))\\s+"
                  (mapconcat (lambda (w) (regexp-quote w)) words ".*")))
         (cmd (format
               "rg -P --no-heading --line-number --color=never -e %S -g '*.org' --glob '!*.git'"
               pattern)))
    (compilation-start
     cmd 'grep-mode
     (lambda (_) (format "*org-roam RG <%s>*" terms)))))

(defun rg-roam-visit-link (&optional id-arg)
  "Show potential targets an id might refer to -- the id from the link under point, or the id provided as an argument. Prompts the user for a folder to search, suggesting the current one."
  (interactive)
  (let ((id (or id-arg (rg-roam-id))))
    (unless id
      (error "No ID provided or found at point"))
    (let ( ( search-dir
	     ( read-directory-name 
               "Directory to search: " 
               (or (file-name-directory buffer-file-name)
                   default-directory))))
      (rg-roam-visit-link-ni id search-dir))))

(defun rg-roam-new-file ()
  "Creates a new org-roam file with a random ID. 
That is, open an unsaved buffer in the current folder with:
- a properties drawer containing a random ID
- a title line with the cursor positioned after"
  (interactive)
  (let ((new-id (rg-roam-random-uid)))
    (let ((buffer (generate-new-buffer "untitled.org")))
      (with-current-buffer buffer
        (insert (format
		 ":PROPERTIES:\n:ID:       %s\n:END:\n#+title: "
		 new-id))
        (org-mode)
        (goto-char (point-max)))
      (switch-to-buffer buffer)
      (setq-local buffer-offer-save t))))

(defun rg-roam-assign-id ()
  "Insert a properties drawer with a random ID after the current line."
  (interactive)
  (let ((id (format "%s" (rg-roam-random-uid))))
    (save-excursion
      (end-of-line)
      (open-line 1)
      (forward-line 1)
      (insert ":PROPERTIES:\n:ID: " id "\n:END:"))
    (message "Inserted properties drawer with ID: %s" id)
    (kill-new id)))

(defun rg-roam-make-link (id label)
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

(defun rg-roam-id ()
  "Extract the ID under point if there is one, from a link or a properties drawer."
  (interactive)
  (let ((id nil))
    (setq id (rg-roam-id-from-link))
    (when (not id)
      (setq id (rg-roam-id-from-properties-drawer)))
    (if id
        (progn
          (when (called-interactively-p 'any)
            (message "Found ID: %s" id))
          (kill-new id)
          id)
      (message "No ID found under point.")
      nil)))
