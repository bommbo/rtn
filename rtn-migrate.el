;;; rtn-migrate.el --- Migrate RTN annotations after renaming/moving files or directories
;;; -*- lexical-binding: t -*-

(require 'rtn-db)
(require 'rtn-extra)
(require 'cl-lib)

;;;###autoload
(defun rtn-migrate-file (source-file target-file)
  "Migrate all RTN annotations from SOURCE-FILE to TARGET-FILE.
Updates all references pointing to SOURCE-FILE to TARGET-FILE.

Usage:
1. You have renamed/moved a file outside Emacs.
2. Run this command.
3. Select the original file (source).
4. Select the new file (target).
5. Confirm.

Example:
  Source: ~/project/old.el
  Target: ~/project/new.el"
  (interactive
   (list (read-file-name "Migrate annotations from file (source): ")
		 (read-file-name "To file (target): ")))
  (let ((src (expand-file-name source-file))
		(tgt (expand-file-name target-file)))
	(unless (file-exists-p tgt)
	  (error "Target file does not exist: %s" tgt))
	(let ((annos (rtn-get-file src)))
	  (if (null annos)
		  (message "ℹ️ No RTN annotations found in %s" src)
		(unless (y-or-n-p (format "Migrate %d annotations from\n%s\nto\n%s? "
								  (length annos) src tgt))
		  (user-error "Aborted"))
		;; Copy annotations to target
		(dolist (a annos)
		  (rtn-extra-add-db tgt (nth 0 a) (nth 1 a) (nth 2 a) (nth 3 a)))
		;; Delete from source
		(dolist (a annos)
		  (rtn-delete-db src (nth 0 a)))
		;; Update all references in DB
		(let ((ref-count 0))
		  (dolist (ref-file (rtn-get-all-files))
			(dolist (anno (rtn-get-file ref-file))
			  (let* ((pos (nth 0 anno))
					 (content (nth 2 anno))
					 (icon (nth 3 anno)))
				(when (string-match-p (regexp-quote src) content)
				  (let ((new-content (replace-regexp-in-string
									  (regexp-quote src) tgt content t t)))
					(rtn-extra-update-db ref-file pos new-content icon)
					(cl-incf ref-count))))))
		;; Refresh buffers if open
		(dolist (buf (list (get-file-buffer src) (get-file-buffer tgt)))
		  (when (and buf (buffer-live-p buf))
			(with-current-buffer buf
			  (rtn-display-annotations))))
		(message "✅ Migrated %d annotations and updated %d references"
				 (length annos) ref-count))))))

;;;###autoload
(defun rtn-migrate-directory (source-dir target-dir)
  "Migrate all RTN annotations under SOURCE-DIR to TARGET-DIR (preserving structure).
Recursively moves annotations for every file in SOURCE-DIR to corresponding path in TARGET-DIR."
  (interactive
   (list (read-directory-name "Migrate annotations from directory (source): ")
		 (read-directory-name "To directory (target): ")))
  (let ((src-root (file-name-as-directory (expand-file-name source-dir)))
		(tgt-root (file-name-as-directory (expand-file-name target-dir))))
	(unless (file-directory-p tgt-root)
	  (error "Target directory does not exist: %s" tgt-root))
	;; Find all RTN-tracked files under src-root
	(let ((file-mappings '()))
	  (dolist (file (rtn-get-all-files))
		(when (string-prefix-p src-root file)
		  (let ((rel (file-relative-name file src-root)))
			(push (cons file (expand-file-name rel tgt-root)) file-mappings))))
	  (if (null file-mappings)
		  (message "ℹ️ No RTN annotations found under %s" src-root)
		(unless (y-or-n-p (format "Migrate annotations from %d files under\n%s\nto\n%s? "
								  (length file-mappings) src-root tgt-root))
		  (user-error "Aborted"))
		(let ((total-annos 0))
		  ;; Process each file
		  (dolist (mapping file-mappings)
			(let ((old-file (car mapping))
				  (new-file (cdr mapping)))
			  ;; Ensure parent dir exists
			  (make-directory (file-name-directory new-file) t)
			  ;; Migrate annotations
			  (dolist (a (rtn-get-file old-file))
				(rtn-extra-add-db new-file (nth 0 a) (nth 1 a) (nth 2 a) (nth 3 a))
				(cl-incf total-annos))
			  ;; Delete old
			  (dolist (a (rtn-get-file old-file))
				(rtn-delete-db old-file (nth 0 a)))
			  ;; Update references
			  (dolist (ref-file (rtn-get-all-files))
				(dolist (anno (rtn-get-file ref-file))
				  (let* ((pos (nth 0 anno))
						 (content (nth 2 anno))
						 (icon (nth 3 anno)))
					(when (string-match-p (regexp-quote old-file) content)
					  (let ((new-content (replace-regexp-in-string
										  (regexp-quote old-file) new-file content t t)))
						(rtn-extra-update-db ref-file pos new-content icon))))))))
		  (message "✅ Migrated %d files, %d annotations. All references updated."
				   (length file-mappings) total-annos)

		  (dolist (mapping file-mappings)
			(let ((old-file (car mapping)))
			  (let ((buf (get-file-buffer old-file)))
				(when (and buf (buffer-live-p buf))
				  (with-current-buffer buf
					(rtn-display-annotations)))))))))))

(provide 'rtn-migrate)
;;; rtn-migrate.el ends here
