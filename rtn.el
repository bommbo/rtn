;;; rtn.el --- File annotation system for Emacs -*- lexical-binding: t -*-

(require 'rtn-db)
(require 'cl-lib)
(require 'ov)

;;;; Customization
(defgroup rtn nil
  "File annotation system for Emacs (RTN)."
  :group 'applications
  :prefix "rtn-")

(defface rtn-annotation-face
  '((t (:background "#fff3cd" :foreground "#333333")))
  "Face for annotation overlays."
  :group 'rtn)

(defcustom rtn-popup-width 50
  "Width of annotation popup."
  :type 'integer
  :group 'rtn)

(defcustom rtn-icon-prefix "📝"
  "Icon shown in the left margin for each annotation."
  :type 'string
  :group 'rtn)

(defcustom rtn-show-note-on-hover t
  "Whether to show note content in minibuffer when cursor is on annotation."
  :type 'boolean
  :group 'rtn)

;;;; Variables
(defvar rtn-annotations-overlays (make-hash-table :test 'equal)
  "Hash table storing annotation overlays by file.")

(defvar rtn-annotations-visible t
  "Whether annotations are currently visible.")

(defvar rtn-edit-window-config nil
  "Store window configuration for split.")

(defvar rtn-edit-original-buffer nil
  "Store original buffer during edit.")

(defvar rtn-edit-file nil
  "File being edited.")

(defvar rtn-edit-pos nil
  "Position of annotation.")

(defvar rtn-edit-end-pos nil
  "End position of annotation.")

;;;; Helpers
(defun rtn-file ()
  "Get current buffer's file path."
  (buffer-file-name))

(defun rtn-make-overlay-key (file pos)
  "Create key for overlay hash."
  (format "%s:%d" file pos))

(defun rtn-get-annotation (file pos)
  "Return (pos end-pos content) or nil."
  (let ((row (rtn-get-db file pos)))
	(when row
	  (list (nth 0 row) (nth 1 row) (nth 2 row)))))

(defun rtn-overlay-at-pos (pos)
  "Get overlay at POS in current buffer."
  (let ((file (rtn-file)))
	(when file
	  (let ((overlays (gethash file rtn-annotations-overlays)))
		(cl-find pos overlays :key (lambda (ov) (overlay-start ov)))))))

;;;; Hover functionality
(defun rtn--show-note-on-hover ()
  "Show note content in minibuffer when cursor is on annotation."
  (when (and rtn-show-note-on-hover
			 (rtn-file)
			 rtn-mode)
	(let ((ov (rtn-overlay-at-pos (point))))
	  (if ov
		  (let ((content (overlay-get ov 'rtn-content)))
			(when content
			  (let ((truncated (truncate-string-to-width content 80 0 nil "...")))
				(message "%s" truncated))))
		(unless (memq this-command '(rtn--show-note-on-hover
									 keyboard-quit
									 minibuffer-keyboard-quit))
		  (message ""))))))

(defun rtn--setup-hover ()
  "Setup hover functionality when RTN mode is enabled."
  (add-hook 'post-command-hook #'rtn--show-note-on-hover nil t))

(defun rtn--cleanup-hover ()
  "Cleanup hover functionality when RTN mode is disabled."
  (remove-hook 'post-command-hook #'rtn--show-note-on-hover t)
  (message ""))

;;;; Display & Overlays
(defun rtn-display-annotations ()
  "Display all annotations in current file."
  (let ((file (rtn-file)))
	(when file
	  (rtn-clear-overlays file)
	  (let ((notes (rtn-get-file file)))
		(dolist (note notes)
		  (let* ((pos (nth 0 note))
				 (end-pos (nth 1 note))
				 (content (nth 2 note))
				 (ov (ov pos end-pos
						 'face 'rtn-annotation-face  ;; ✅ 现在这是有效的 face symbol
						 'rtn-content content
						 'evaporate nil
						 'modification-hooks '(rtn-overlay-modified)
						 'before-string
						 (propertize rtn-icon-prefix
									 'mouse-face 'highlight
									 'help-echo (truncate-string-to-width content 50 0 nil "...")))))
			(let* ((key (rtn-make-overlay-key file pos))
				   (overlays (gethash file rtn-annotations-overlays '())))
			  (puthash file (cons ov overlays) rtn-annotations-overlays))))))))

(defun rtn-clear-overlays (file)
  "Clear overlays for FILE."
  (when-let ((overlays (gethash file rtn-annotations-overlays)))
	(dolist (ov overlays)
	  (delete-overlay ov))
	(puthash file '() rtn-annotations-overlays)))

(defun rtn-overlay-modified (ov after beg end &optional len)
  "Handle overlay modification."
  (unless after
	(message "⚠️ Annotations may need updating after edit")))

(defun rtn-show-annotation-popup ()
  "Show popup for annotation at point."
  (interactive)
  (when-let* ((ov (rtn-overlay-at-pos (point)))
			  (content (overlay-get ov 'rtn-content)))
	(message (truncate-string-to-width content 100))))

;;;; Toggle Annotations
(defun rtn-toggle-annotations ()
  "Toggle annotation display."
  (interactive)
  (let ((file (rtn-file)))
	(if (not file)
		(message "⚠️ Not in a saved file")
	  (setq rtn-annotations-visible (not rtn-annotations-visible))
	  (if rtn-annotations-visible
		  (progn
			(rtn-display-annotations)
			(message "✅ Annotations shown"))
		(progn
		  (rtn-clear-overlays file)
		  (message "✅ Annotations hidden"))))))

;;;; Add/Edit Annotation
;;;###autoload
(defun rtn-add-edit ()
  "Add or edit annotation at point."
  (interactive)
  (let ((file (rtn-file)))
	(if (not file)
		(message "⚠️ Save buffer to file first")
	  (let* ((pos (point))
			 (annotation (rtn-get-annotation file pos))
			 (old-content (or (and annotation (nth 2 annotation)) ""))
			 (edit-buf (get-buffer-create "*RTN Edit*")))
		(setq rtn-edit-window-config (current-window-configuration))
		(setq rtn-edit-original-buffer (current-buffer))
		(setq rtn-edit-file file)
		(setq rtn-edit-pos pos)
		(setq rtn-edit-end-pos (if annotation (nth 1 annotation) (1+ pos)))
		(split-window-right (- (window-width) rtn-popup-width))
		(select-window (next-window))
		(switch-to-buffer edit-buf)
		(erase-buffer)
		(insert old-content)
		(rtn-edit-mode)
		(message "📝 Edit annotation | C-c C-c save | C-c C-k cancel")))))

(defun rtn-save-edit ()
  "Save annotation."
  (interactive)
  (let ((content (string-trim (buffer-string)))
		(file rtn-edit-file)
		(pos rtn-edit-pos)
		(end-pos rtn-edit-end-pos))
	(cond
	 ((string-empty-p content)
	  (message "⚠️ Annotation cannot be empty"))
	 (t
	  (if (rtn-get-annotation file pos)
		  (rtn-update-db file pos content)
		(rtn-add-db file pos end-pos content))
	  (rtn-close-edit-window)
	  (message "✅ Annotation saved")))))

(defun rtn-cancel-edit ()
  "Cancel annotation edit."
  (interactive)
  (rtn-close-edit-window)
  (message "Cancelled"))

(defun rtn-close-edit-window ()
  "Close edit window and restore configuration."
  (kill-buffer (current-buffer))
  (when rtn-edit-window-config
	(set-window-configuration rtn-edit-window-config)
	(when rtn-annotations-visible
	  (rtn-display-annotations))))

;;;; Delete Annotation
;;;###autoload
(defun rtn-clear-at-point ()
  "Delete annotation at point."
  (interactive)
  (let ((file (rtn-file)))
	(if (not file)
		(message "⚠️ Not in a saved buffer")
	  (let ((annotation (rtn-get-annotation file (point))))
		(if (not annotation)
			(message "❌ No annotation here")
		  (when (y-or-n-p "Delete annotation? ")
			(rtn-delete-db file (nth 0 annotation))
			(rtn-display-annotations)
			(message "✅ Annotation deleted")))))))

;;;###autoload
(defun rtn-clear-buffer ()
  "Clear all annotations for the current buffer's file."
  (interactive)
  (let ((file (rtn-file)))
	(if (not file)
		(user-error "⚠️ Current buffer has no associated file")
	  (when (yes-or-no-p
			 (format "🧹 Are you sure you want to delete all annotations for %s? "
					 (file-name-nondirectory file)))
		(if (fboundp 'rtn-delete-file-db)
			(progn
			  (condition-case err
				  (progn
					(rtn-delete-file-db file)
					(when (fboundp 'rtn-clear-overlays)
					  (rtn-clear-overlays file))
					(when (fboundp 'rtn-display-annotations)
					  (rtn-display-annotations))
					(message "✅ Deleted and cleared all annotations for %s."
							 (file-name-nondirectory file)))
				(error (message "❌ Error during deletion: %S" err))))
		  (if (and (fboundp 'rtn-get-file) (fboundp 'rtn-delete-db))
			  (progn
				(let ((notes (rtn-get-file file)))
				  (dolist (note notes)
					(when (and (listp note) (integerp (nth 0 note)))
					  (rtn-delete-db file (nth 0 note)))))
				(when (fboundp 'rtn-clear-overlays)
				  (rtn-clear-overlays file))
				(when (fboundp 'rtn-display-annotations)
				  (rtn-display-annotations))
				(message "✅ Deleted and cleared annotations for %s using fallback."
						 (file-name-nondirectory file)))
			(message "❌ Cannot delete: missing DB interface")))))))

;;;###autoload
(defun rtn-clear-file ()
  "Prompt user to pick a file and clear its annotations."
  (interactive)
  (if (not (fboundp 'rtn-get-all-files))
	  (user-error "❌ Database missing rtn-get-all-files interface")
	(let* ((all (rtn-get-all-files))
		   (files (if (and all (> (length all) 0)) all nil)))
	  (if (null files)
		  (message "⚠️ No files with annotations in database.")
		(let* ((cands (mapcar #'abbreviate-file-name files))
			   (alist (cl-pairlis cands files))
			   (choice (completing-read "🧹 Select file to clear annotations: " cands nil t)))
		  (let ((abs (or (alist-get choice alist nil nil #'string=)
						 (and (stringp choice) (expand-file-name choice)))))
			(unless (and abs (file-exists-p abs))
			  (user-error "⚠️ Selected file is invalid: %s" choice))
			(when (yes-or-no-p (format "🧹 Are you sure you want to delete all annotations for %s? "
									   (file-name-nondirectory abs)))
			  (if (fboundp 'rtn-delete-file-db)
				  (condition-case err
					  (progn
						(rtn-delete-file-db abs)
						(let ((buf (get-file-buffer abs)))
						  (when buf
							(with-current-buffer buf
							  (when (fboundp 'rtn-clear-overlays)
								(rtn-clear-overlays abs))
							  (when (fboundp 'rtn-display-annotations)
								(rtn-display-annotations)))))
						(message "✅ Deleted and cleared: %s" (file-name-nondirectory abs)))
					(error (message "❌ Error during deletion: %S" err)))
				(if (and (fboundp 'rtn-get-file) (fboundp 'rtn-delete-db))
					(progn
					  (let ((notes (rtn-get-file abs)))
						(dolist (note notes)
						  (when (and (listp note) (integerp (nth 0 note)))
							(rtn-delete-db abs (nth 0 note)))))
					  (let ((buf (get-file-buffer abs)))
						(when buf
						  (with-current-buffer buf
							(when (fboundp 'rtn-clear-overlays)
							  (rtn-clear-overlays abs))
							(when (fboundp 'rtn-display-annotations)
							  (rtn-display-annotations)))))
					  (message "✅ Deleted and cleared using fallback: %s" (file-name-nondirectory abs)))
				  (message "❌ Cannot delete: missing rtn-delete-file-db"))))))))))

;;;; Major Modes
(define-derived-mode rtn-edit-mode text-mode "RTN-Edit"
  "Mode for editing annotations."
  (local-set-key (kbd "C-c C-c") #'rtn-save-edit)
  (local-set-key (kbd "C-c C-k") #'rtn-cancel-edit))

;;;; Minor Mode
(define-minor-mode rtn-mode
  "Rich Text Notes (RTN) annotation system."
  :lighter " 📝"
  (unless (rtn-file)
	(setq rtn-mode nil))
  (if rtn-mode
	  (progn
		(rtn-display-annotations)
		(rtn--setup-hover)
		(message "✅ RTN enabled in %s"
				 (file-name-nondirectory (buffer-file-name))))
	(progn
	  (rtn-clear-overlays (rtn-file))
	  (rtn--cleanup-hover)
	  (message "✅ RTN disabled"))))

(defun rtn--delayed-enable ()
  "Enable RTN after buffer is fully loaded."
  (when (and (buffer-file-name)
			 (not rtn-mode)
			 (rtn-get-file (buffer-file-name)))
	(rtn-mode 1)))

(add-hook 'hack-local-variables-hook #'rtn--delayed-enable)
(add-hook 'after-change-major-mode-hook #'rtn--delayed-enable)

(provide 'rtn)
;;; rtn.el ends here
