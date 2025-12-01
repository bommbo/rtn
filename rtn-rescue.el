;;; rtn-rescue.el --- Rescue RTN annotations from deleted files -*- lexical-binding: t -*-

(require 'rtn-db)
(require 'rtn-extra)
(require 'tabulated-list)

(defun rtn--file-size (file)
  "Return size of FILE in bytes, or 0 if not accessible."
  (let ((attrs (file-attributes file)))
	(if attrs (nth 7 attrs) 0)))

;;;###autoload
(defun rtn-rescue-annotations ()
  "List all RTN annotations whose files no longer exist."
  (interactive)
  (let ((entries '()))
	(dolist (file (rtn-get-all-files))
	  (unless (file-exists-p file)
		(dolist (anno (rtn-get-file file))
		  (let ((pos (nth 0 anno))
				(content (or (nth 2 anno) ""))
				(icon (or (nth 3 anno) "📝")))
			(push (list (cons file pos)
						(vector (propertize icon 'display icon)
								(file-name-nondirectory file)
								(number-to-string pos)
								(truncate-string-to-width content 60 nil nil "...")))
				  entries)))))
	(if (null entries)
		(message "✅ No dead annotations found.")
	  (let ((buf (get-buffer-create "*RTN Rescue*")))
		(with-current-buffer buf
		  (rtn-rescue-mode)
		  (setq tabulated-list-entries (nreverse entries))
		  (tabulated-list-init-header)
		  (tabulated-list-print))
		(pop-to-buffer buf)
		(message "⚠️ Found %d dead annotations." (length entries))))))

;;;###autoload
(defun rtn-rescue-save-to-file (target-file)
  "Save the selected dead annotation to TARGET-FILE as a rescued note."
  (interactive "FRescue annotation to file: ")
  (let ((id (tabulated-list-get-id)))
	(unless id (user-error "No annotation selected"))
	(let* ((file (car id))
		   (pos (cdr id))
		   (anno (rtn-extra-get-annotation file pos)))
	  (unless anno (user-error "Annotation not found in DB"))
	  (let ((content (nth 2 anno))
			(icon (or (nth 3 anno) "📝")))
		(with-temp-file target-file
		  (insert (format "%s [Rescued from %s @POS:%d]\n%s\n"
						  icon file pos (or content ""))))
		(message "✅ Rescued to: %s" target-file)))))

;;;###autoload
(defun rtn-rescue-delete-entry ()
  "Delete the selected dead annotation from the RTN database and refresh view."
  (interactive)
  (let ((id (tabulated-list-get-id)))
	(unless id (user-error "No annotation selected"))
	(let ((file (car id))
		  (pos (cdr id)))
	  (when (y-or-n-p (format "Permanently delete annotation from %s:%d? " file pos))
		(rtn-delete-db file pos)
		(rtn-rescue-refresh)
		(message "🗑️ Deleted.")))))

;;;###autoload
(defun rtn-rescue-edit-file-path ()
  "Edit the file path of the selected dead annotation and restore it.
If the target file doesn't exist, create it empty."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (unless id (user-error "No annotation selected"))
    (let* ((old-file (car id))
           (pos (cdr id))
           (anno (rtn-extra-get-annotation old-file pos)))
      (unless anno (user-error "Annotation not found"))
      (let* ((new-file (read-file-name "Edit file path: " old-file))
             (new-abs (expand-file-name new-file)))
		
        (unless (file-exists-p new-abs)
          (make-empty-file new-abs)
          (message "🆕 Created empty file: %s" new-abs))
		
        (let ((content (nth 2 anno))
              (icon (or (nth 3 anno) "📝"))
              (new-size (rtn--file-size new-abs)))
          (let ((new-pos (if (> pos new-size) new-size (max 1 pos))))
            (rtn-extra-add-db new-abs new-pos new-pos content icon))
          (rtn-delete-db old-file pos)
          (rtn-rescue-refresh)
          (message "✅ Restored annotation to %s" new-abs))))))

(defun rtn-rescue-refresh ()
  "Refresh the current RTN Rescue buffer."
  (interactive)
  (when (eq major-mode 'rtn-rescue-mode)
	(let ((entries '()))
	  (dolist (file (rtn-get-all-files))
		(unless (file-exists-p file)
		  (dolist (anno (rtn-get-file file))
			(let ((pos (nth 0 anno))
				  (content (or (nth 2 anno) ""))
				  (icon (or (nth 3 anno) "📝")))
			  (push (list (cons file pos)
						  (vector (propertize icon 'display icon)
								  (file-name-nondirectory file)
								  (number-to-string pos)
								  (truncate-string-to-width content 60 nil nil "...")))
					entries)))))
	  (setq tabulated-list-entries (nreverse entries))
	  (tabulated-list-print))))

(defun rtn-rescue-show-help ()
  "Show brief help for RTN Rescue mode in a help buffer."
  (interactive)
  (with-help-window "*RTN Rescue Help*"
    (with-current-buffer standard-output
      (insert "RTN Rescue Mode — Key Bindings\n\n")
      (insert "`RET`  : Save selected annotation to a new file\n")
      (insert "`d`    : Delete selected annotation from database\n")
      (insert "`e`    : Edit file path and restore annotation\n")
      (insert "`g`    : Refresh the list\n")
      (insert "`q`    : Quit window\n")
      (insert "`?`    : Show this help\n"))))

(define-derived-mode rtn-rescue-mode tabulated-list-mode "RTN-Rescue"
  "Major mode for rescuing dead RTN annotations."
  (setq tabulated-list-format [("Icon" 2 nil) ("File" 20 t) ("Pos" 6 nil) ("Content" 0 t)])
  (setq mode-name "RTN Rescue")
  (local-set-key (kbd "RET") #'rtn-rescue-save-to-file)
  (local-set-key "d"       #'rtn-rescue-delete-entry)
  (local-set-key "e"       #'rtn-rescue-edit-file-path)
  (local-set-key "g"       #'rtn-rescue-refresh)
  (local-set-key "?"       #'rtn-rescue-show-help)
  (local-set-key "q"       #'quit-window))

(provide 'rtn-rescue)
;;; rtn-rescue.el ends here
