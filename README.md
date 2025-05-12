rg-roam is a minimal standalone org-roam implementation, using ripgrep instead of sql. It might get extended it but it works as of commit 98320c96b34ec6f0841fac6e2d5c3cf917eb1ae2 in only 250 lines of code.

# Purpose
If org-roam and even org-mode break, but emacs still works,
you can use these commands to do everything org-roam does.
It will be slightly awkward relative to org-roam,
but worlds better than having to spec all your searches and make all your edits manually.

# To load the code
Be sure you have ripgrep installed outside of Emacs, 
and the `ripgrep` packages installed in Emacs.
Open `main.el` and evaluate it (`M-x eval-buffer`).

# Only five commands are necessary.
These are all interactive.
	
## rg-roam-find
Search for terms.

## rg-roam-visit-link

## rg-roam-new-file

## rg-roam-assign-id
Create a property bucket with a new random ID
below the current line.

## rg-roam-make-link
Insert a link.

## rg-roam-id
Get the id at point, from a link or a properties drawer.

# Some context for understanding the code.

## org-roam uses ID properties
These are what make links possible in org-roam. Anything that can have a properties bucket -- either a heading in the file, or the file itself -- can have such a property. Every file has such a property, at the top. Nothing can have more than one ID.

### Property buckets and ID properties look like this
  :PROPERTIES:
  :ID:       6972d099-7ff6-47ba-ac67-1898ef5fd549
  :END:

  There might also be other lines in the property bucket, like this:

  :PROPERTIES:
  :ROAM_ALIASES: method experiment
  :ID:       6972d099-7ff6-47ba-ac67-1898ef5fd549
  :WHATEVER: yeah
  :END:

## org-roam uses links, which look like this
   [[id:5270486e-0b02-4726-b859-2986d8e4f25a][never seem to have time \ backup tasks]]

(The second bracketed portion contains the label.)

## org-mode uses title lines, which look like this.
#+title: science

## org-mode uses alias lines, which look like this.
:PROPERTIES:
:ROAM_ALIASES: speech communication "sharing through noises"
:END:

