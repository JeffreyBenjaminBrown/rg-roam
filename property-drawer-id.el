(defun start-of-properties-drawer-this-line ()
  "Check if current line is the opening of a properties drawer.
If the line matches the pattern (whitespace):PROPERTIES:(whitespace),
return the position of the last character on the line.
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

(defun end-of-properties-drawer-this-line ()
  "Check if current line is the opening of a properties drawer.
If the line matches the pattern (whitespace):PROPERTIES:(whitespace),
return the position of the last character on the line.
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

(defun property-drawer-start ()
  "See property-drawer-end; same idea."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((result nil))
      (setq result (start-of-properties-drawer-this-line))
      (when (not result)
        (while (and (not result)
                    (looking-at ;; line start, whitespace, colon
		     "^[[:space:]]*:")) 
          (setq result (start-of-properties-drawer-this-line))
          (when (and (not result)
                    (= (forward-line 1) 0))  ;; Successfully moved down
            (beginning-of-line))))
      (when (called-interactively-p 'any)
        (message "Result: %s" result))
      result)))

(defun property-drawer-end ()
  "Find the end of a property drawer.
Checks if current line is a properties drawer ending (:END:).
If it's a properties drawer ending, returns the position of its first character.
If not, moves downward line by line until finding a properties drawer end or a line without a colon.
Returns nil if no property drawer end is found."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((result nil))
      (setq result (end-of-properties-drawer-this-line))
      (when (not result)
        (while (and (not result)
                    (looking-at ;; line start, whitespace, colon
		     "^[[:space:]]*:")) 
          (setq result (end-of-properties-drawer-this-line))
          (when (and (not result)
                    (= (forward-line 1) 0))  ;; Successfully moved down
            (beginning-of-line))))
      (when (called-interactively-p 'any)
        (message "Result: %s" result))
      result)))
