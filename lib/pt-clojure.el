(defmacro gsub! (sym reg rep)
  `(set ',sym (replace-regexp-in-string ,reg ,rep ,sym)))

(defun pt-clj-ns-for-file-name (file-name)
  "Compute a viable clojure namespace for the given file name.
   Shamelessly ripped from github.com/kyleburton/krbemacs"
  (interactive)
  (cond ((or (string-match "/src/" file-name)
             (string-match "/clj/" file-name)
             (string-match "/test/" file-name))
         (gsub! file-name "^.*/clj/" "")
         (gsub! file-name "^.*/src/" "")
         (gsub! file-name "^.*/test/" "")
         (gsub! file-name "/" "."))
        (t
         (gsub! file-name "^.+/\\([^/]+\\)$" "\\1")))
  (gsub! file-name "_" "-")
  (gsub! file-name "\\.clj$" "")
  file-name)
