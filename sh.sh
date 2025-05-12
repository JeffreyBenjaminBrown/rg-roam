find . -type f -name "*.el" -exec grep --color=auto -nH --null -e defun \{\} +

git diff  | grep -E '^[+-].*defun' \
   | sed '/^+++/d'     \
   | sed '/^---/d'
