(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
		       str)
    (setq str (replace-match "" t t str)))
  str)

(defun untab (str)
  (while (string-match "\t" str)
    (setq str (replace-match " " t t str)))
  str)

(provide 'csound-util)
