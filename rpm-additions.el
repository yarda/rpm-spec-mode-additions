; TODO ido-like goto section

; util
(defun rpm-get-name ()
  "Return the current %{name}."
  (save-excursion
    (beginning-of-buffer)
    (re-search-forward "^Name:[ \t]+\\(.+\\)$" nil nil)
    (match-string 1)))

(defun rpm-get-version ()
  "Return the current %{version}."
  (save-excursion
    (beginning-of-buffer)
    (re-search-forward "^Version:[ \t]+\\(.+\\)$" nil nil)
    (match-string 1)))

; main
(defun increase-patch-number (patch-number)
  "Increase the PATCH-NUMBER by 1.

PATCH-NUMBER is a string. The length of it is preserved.

Examples:

(increase-patch-number \"1\")
=> \"2\"

(increase-patch-number \"001\")
=> \"002\"
"
  (let ((fmt (format "%%0%dd" (length patch-number))))
    (format fmt (1+ (string-to-int patch-number)))))

(defun get-patch-backup-name (patch-name)
  "Turn PATCH-NAME into a name suitable for the -b argument to %patch.

PATCH-NAME is first stripped of prefix string \"%{name}-\", if there is any.
PATCH-NAME is second stripped of prefix string \"%{version}-\", if there is any.
PATCH-NAME is then stripped of suffix string \".patch\", if there is any.

The above is a standard naming convention when adding patches to an RPM spec file.

Example:

The 'iputils.spec' file contains the following:

Name: iputils
Version: 20121221
...
Patch001: iputils-20121221-fix-ugly-bug.patch
...

The resulting %patch line will look like this:

%patch001 -p1 -b .fix-ugly-bug
"
  (let ((result patch-name))
    ; strip "%{name}-" if any
    (let* ((prefix (concat (rpm-get-name) "-"))
	   (prefixlen (length prefix)))
      (when (string-match prefix patch-name)
	(setq result (substring patch-name prefixlen))))
    ; strip "%{version}-" if any
    (let* ((prefix (concat (rpm-get-version) "-"))
	   (prefixlen (length prefix)))
      (when (string-match prefix result)
	(setq result (substring result prefixlen))))
    ; strip the ".patch" suffix if any
    (if (string-match "\\(.+\\)\\.patch$" result)
      (match-string 1 result)
    result)))

(defun rpm-spec-add-patch (patch-file)
  (interactive "fPatch: ")
  (end-of-buffer)
  ;; ^Patch
  (re-search-backward "^Patch\\([0-9]+\\):\\([ \t]*\\)" nil nil)
  (let ((new-patch (increase-patch-number (match-string 1)))
	(patch-name (file-name-nondirectory patch-file)))
    (end-of-line)
    (newline)
    (insert "Patch" new-patch ":" (match-string 2) patch-name)
    ;; ^%patch
    (let ((p (point)))
      (end-of-buffer)
      (if (re-search-backward "^%patch[0-9]+" nil t)
	  (progn
	    (end-of-line)
	    (newline)
	    (insert "%patch" new-patch " -p1 -b ." (get-patch-backup-name patch-name)))
	(goto-char p)))))

(defun rpm-spec-add-patch2 ()
  (interactive)
  (let ((patch-file
	 (completing-read "Patch: " 'read-file-name-internal 'file-exists-p t
			  (concat (file-name-directory (buffer-file-name))
				  (rpm-get-name)
				  "-"
				  (rpm-get-version)
				  "-")
			  'file-name-history)))
    (end-of-buffer)
    ;; ^Patch
    (re-search-backward "^Patch\\([0-9]+\\):\\([ \t]*\\)" nil nil)
    (let ((new-patch (increase-patch-number (match-string 1)))
	  (patch-name (file-name-nondirectory patch-file)))
      (end-of-line)
      (newline)
      (insert "Patch" new-patch ":" (match-string 2) patch-name)
      ;; ^%patch
      (let ((p (point)))
	(end-of-buffer)
	(if (re-search-backward "^%patch[0-9]+" nil t)
	    (progn
	      (end-of-line)
	      (newline)
	      (insert "%patch" new-patch " -p1 -b ." (get-patch-backup-name patch-name)))
	  (goto-char p))))))

(provide 'rpm-additions)
