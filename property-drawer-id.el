(defun rg-roam-end-of-properties-opener-this-line ()
  "Check if current line is the opening of a properties drawer.
If so, return the position of the *last* character on the line.
Otherwise return nil."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((result (if (looking-at
                       "^[[:space:]]*:PROPERTIES:[[:space:]]*$")
                      (progn
                        (end-of-line)
                        (point))
                    nil)))
      (message "Result: %s" result)
      result)))

(defun rg-roam-start-of-properties-closer-this-line ()
  "Check if current line is the opening of a properties drawer.
If so, return the position of the *first* character on the line.
Otherwise return nil."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((result (if (looking-at
                       "^[[:space:]]*:END:[[:space:]]*$")
                      (progn
                        (beginning-of-line)
                        (point))
                    nil)))
      (message "Result: %s" result)
      result)))

(defun rg-roam-end-of-properties-opener ()
  "Find the start of a property drawer.
If the current line does not start with whitespace and a colon, return nil.
If it is a properties drawer ending (:PROPERTIES::), return its end.
Otherwise, continue searching line by line for the end."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((result nil))
      (while (and (not result)
                  (looking-at "^[[:space:]]*:"))
        (setq result (rg-roam-end-of-properties-opener-this-line))
        (unless result
          (when (= (forward-line -1) 0)
            (beginning-of-line))))
      (when (called-interactively-p 'any)
        (message "Result: %s" result))
      result)))

(defun rg-roam-start-of-properties-closer-this-line ()
  "Find the end of a property drawer.
If the current line does not start with whitespace and a colon, return nil.
If it is a properties drawer ending (:END:), return its beginning.
Otherwise, continue searching line by line for the end."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((result nil))
      (while (and (not result)
                  (looking-at "^[[:space:]]*:"))
        (setq result (rg-roam-start-of-properties-closer-this-line))
        (unless result
          (when (= (forward-line 1) 0)
            (beginning-of-line))))
      (when (called-interactively-p 'any)
        (message "Result: %s" result))
      result)))

(defun rg-roam-id-from-properties-drawer ()
  "Extract the ID from a properties drawer.
First finds the boundaries of the properties drawer.
Then searches for an :ID: line between those boundaries.
If found, returns the ID value. Otherwise returns nil."
  (interactive)
  (let (drawer-start drawer-end id)
    (setq drawer-start (rg-roam-end-of-properties-opener))
    (when drawer-start
      (message "start: %s" drawer-start) ;; DEBUG
      (setq drawer-end (rg-roam-start-of-properties-closer-this-line))
      (when drawer-end
	(message "end: %s" drawer-end) ;; DEBUG
        (save-excursion
          (goto-char drawer-start)
          (when (re-search-forward "^[[:space:]]*:ID:[[:space:]]*\\([a-f0-9]\\{8\\}-[a-f0-9]\\{4\\}-[a-f0-9]\\{4\\}-[a-f0-9]\\{4\\}-[a-f0-9]\\{12\\}\\)[[:space:]]*$" drawer-end t)
            (setq id (match-string-no-properties 1))))))
    (if id
        (progn
          (when (called-interactively-p 'any)
            (message "Found ID: %s" id))
          id)
      (message "No ID found in properties drawer")
      nil)))
