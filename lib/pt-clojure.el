(defmacro gsub! (sym reg rep)
  `(set ',sym (replace-regexp-in-string ,reg ,rep ,sym)))

(defun string/starts-with (s begins)
  "returns non-nil if string S starts with BEGINS.  Else nil."
  (cond ((>= (length s) (length begins))
         (string-equal (substring s 0 (length begins)) begins))
        (t nil)))

(defun string/ends-with (s ends)
  "returns non-nil if string S ends with ENDS.  Else nil."
  (cond ((>= (length s) (length ends))
         (string-equal (substring s (- (length s) (length ends)) (length s)) ends))
        (t nil)))

(defun pt-clj-ns-for-file-name (file-name)
  "Compute a viable clojure namespace for the given file name.
   Shamelessly ripped from github.com/kyleburton/krbemacs"
  (interactive)
  (cond ((or (string-match "/src/" file-name)
             (string-match "/clj/" file-name)
             (string-match "/cljs/" file-name)
             (string-match "/test/" file-name))
         (gsub! file-name "^.*/clj/" "")
         (gsub! file-name "^.*/cljs/" "")
         (gsub! file-name "^.*/src/" "")
         (gsub! file-name "^.*/test/" "")
         (gsub! file-name "/" "."))
        (t
         (gsub! file-name "^.+/\\([^/]+\\)$" "\\1")))
  (gsub! file-name "_" "-")
  (gsub! file-name "\\.clj$" "")
  (gsub! file-name "\\.cljs$" "")
  file-name)

(defun type-annotation-p (elt)
	(string/starts-with (format "%s" elt) "^"))

(defun destructuring-keyword-p (elt)
	(or (string/ends-with elt ":keys")
			(equal ":as" elt)))

(defun precedes-prismatic-schema-type-annotation-p (elt)
	(equal ":-" elt))

(defun ampersand-p (elt)
	(equal "&" elt))

(defun remove-non-symbol-arguments (elts)
	;; strip meta-data/destructuring from the list
	(remove-if
	 (lambda (elt)
		 (or (type-annotation-p elt)
				 (destructuring-keyword-p elt)
				 (ampersand-p elt)
				 (precedes-prismatic-schema-type-annotation-p elt)))
	 elts))

(defun handle-map-and-vector-destructuring (elt)
	(replace-regexp-in-string
	 "[\[]" ""
	 (replace-regexp-in-string
		"[\]]" ""
		(replace-regexp-in-string
		 "[\{]" ""
		 (replace-regexp-in-string
			"[}]" "" elt)))))

(defun pt-clj-get-current-fn-args ()
	"Shamelessly ripped from Relay/KBot's emacs conf."
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (search-forward "[")
    (let ((start (point)))
      (backward-char 1)
      (forward-sexp 1)
      (backward-char 1)
			(mapcar
			 'handle-map-and-vector-destructuring
			 (remove-non-symbol-arguments
				(split-string
				 (buffer-substring start (point))
				 " "))))))

(defun pt-clj-fn-args-to-defs ()
  "Shamelessly ripped from Relay/KBot's emacs conf.
   Handle the following conditions:
   (defn name [] ...)                        [x]
   (defn name [& args] ...)                  [x]
   (defn name [& [args] ...)                 [ ]
   (defn name [^Type arg1] ...)              [ ]
   (defn name [{:keys [a b c] :as foo}] ...) [ ]
"
  (interactive)
  (save-excursion
    (let ((args-list (pt-clj-get-current-fn-args)))
      ;; NB: ignore type hints
      (beginning-of-defun)
      (search-forward "[")
      (backward-char 1)
      (forward-sexp 1)
      (next-line 1)
      (beginning-of-line)
      (loop for arg in args-list
            do
            (beginning-of-line)
            (insert (format "  (def %s %s)\n" arg arg)))
      (save-buffer))))
