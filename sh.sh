find . -type f -name "*.el" -exec grep --color=auto -nH --null -e defun \{\} +
