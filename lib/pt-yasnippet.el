(require 'yasnippet)

(defun pt-snip-indent-or-expand ()
  (interactive)
  (if (and
       (or (bobp) (= ?w (char-syntax (char-before))))
       (or (eobp)
           (not (= ?w (char-syntax (char-after))))))
      (yas-expand)
		(indent-according-to-mode)))

(local-set-key [tab] 'pt-snip-indent-or-expand)

(provide 'pt-snippet)
