;;; rtn-reference.el --- Copy-paste position references for RTN  -*- lexical-binding: t -*-

(require 'rtn-db)
(require 'rtn-extra)

(defvar rtn-clipboard nil
  "clipboard：((file . pos) . original-icon)")

;;;###autoload
(defun rtn-copy-position ()
  "Copy current annotation position + original icon to clipboard."
  (interactive)
  (let ((file (rtn-file)))
	(if (not file)
		(message "⚠️ Not in a saved buffer")
	  (let* ((anno (rtn-extra-get-annotation file (point)))
			 (pos  (if anno (nth 0 anno) (point)))
			 (icon (or (and anno (nth 3 anno)) "📝")))
		(setq rtn-clipboard (cons (cons file pos) icon))
		(message "✅ Copied: %s:%d" (file-name-nondirectory file) pos)))))

;;;###autoload
(defun rtn-paste-reference ()
  "Paste reference marker at point."
  (interactive)
  (if (not rtn-clipboard)
	  (message "⚠️ Nothing copied yet (use C-c n c to copy)")
	(let ((file (rtn-file)))
	  (if (not file)
		  (message "⚠️ Not in a saved buffer")
		(let* ((current-pos (point))
			   (target-file (car (car rtn-clipboard)))
			   (target-pos  (cdr (car rtn-clipboard)))
			   (orig-icon   (or (cdr rtn-clipboard) "📝"))
			   (content     (format "%s %s @POS:%d@"
									orig-icon (expand-file-name target-file) target-pos))
			   (icon        "🔗"))
		  (rtn-extra-add-db file current-pos current-pos content icon)
		  (rtn-display-annotations)
		  (message "✅ Reference pasted: %s @POS:%d" target-file target-pos))))))

;;;###autoload
(defun rtn-ref-jump-to-target ()
  "Jump to target offset parsed from current line content."
  (interactive)
  (let* ((here-file (rtn-file))
		 (here-pos  (point))
		 (here-anno (rtn-extra-get-annotation here-file here-pos)))
	(if (null here-anno)
		(message "❌ Not on an annotation")
	  (let ((content (nth 2 here-anno)))
		(if (not (string-match " \\([^ ]+\\) @POS:\\([0-9]+\\)@\\'" content))
			(message "❌ Not a reference marker")
		  (let* ((target-pos  (string-to-number (match-string 2 content)))
				 (target-file (match-string 1 content)))
			(let ((buf (or (get-file-buffer target-file)
						   (when (file-exists-p target-file)
							 (find-file-noselect target-file)))))
			  (if (not buf)
				  (message "❌ Cannot open target file: %s" target-file)
				(with-current-buffer buf
				  (switch-to-buffer buf)
				  (goto-char (min target-pos (point-max)))
				  (pulse-momentary-highlight-one-line)
				  (message "✅ Jumped to offset %d in %s" target-pos target-file))))))))))

(defadvice rtn-update-position-db (after rtn-ref-sync-positions activate)
  (let ((file (ad-get-arg 0))
		(old-pos (ad-get-arg 1))
		(new-pos (ad-get-arg 2)))
	(when (/= old-pos new-pos)
	  (dolist (ref-file (rtn-get-all-files))
		(let ((annos (rtn-get-file ref-file)))
		  (dolist (anno annos)
			(let* ((content (nth 2 anno))
				   (anno-pos (nth 0 anno))
				   (new-content (replace-regexp-in-string
								 (format "@POS:%d@" old-pos)
								 (format "@POS:%d@" new-pos)
								 content t t)))
			  (when (not (string= content new-content))
				(rtn-extra-update-db ref-file anno-pos new-content (nth 3 anno))))))))))

;;;###autoload
(defun rtn-copy-anno-text ()
  "Copy the plain text of the current annotation: icon + content (without jump info)."
  (interactive)
  (let* ((file (rtn-file))
		 (pos  (point))
		 (anno (rtn-extra-get-annotation file pos)))
	(if (null anno)
		(message "❌ Not on an annotation")
	  (let* ((icon  (or (nth 3 anno) "📝"))
			 (text  (nth 2 anno))
			 (clean (replace-regexp-in-string "@POS:[0-9]+@" "" text t t)))
		(kill-new (format "%s %s" icon (string-trim clean)))
		(message "✅ Copied text: %s" (truncate-string-to-width clean 50 nil nil "..."))))))

;;;###autoload
(defun rtn-paste-anno-as-marker ()
  "Insert the last copied plain text (icon + content) as a normal annotation at point."
  (interactive)
  (let ((text (or (car kill-ring) "")))
	(if (string-empty-p text)
		(message "❌ Clipboard empty")
	  (let* ((file   (rtn-file))
			 (pos    (point))
			 (parts  (split-string text " " 2))
			 (icon   (or (car parts) "📝"))
			 (content (or (cadr parts) text)))
		(if (not file)
			(message "⚠️ Not in a saved buffer")
		  (rtn-extra-add-db file pos pos content icon)
		  (rtn-display-annotations)
		  (message "✅ Pasted marker: %s" (truncate-string-to-width content 50 nil nil "...")))))))

(defvar rtn-move-clipboard nil
  "Clipboard for moving annotations: (file . pos)")

;;;###autoload
(defun rtn-move-annotation ()
  "Copy current annotation for moving (like cut)."
  (interactive)
  (let ((file (rtn-file)))
	(if (not file)
		(message "⚠️ Not in a saved buffer")
	  (let* ((anno (rtn-extra-get-annotation file (point)))
			 (pos  (if anno (nth 0 anno) (point))))
		(if (null anno)
			(message "❌ Not on an annotation")
		  (setq rtn-move-clipboard (cons file pos))
		  (message "✂️ Cut annotation: %s:%d" (file-name-nondirectory file) pos))))))

;;;###autoload
(defun rtn-paste-moved-annotation ()
  "Paste moved annotation at point, deleting original and updating all references."
  (interactive)
  (if (not rtn-move-clipboard)
	  (message "⚠️ Nothing cut (use C-c n m to cut)")
	(let ((target-file (rtn-file)))
	  (if (not target-file)
		  (message "⚠️ Not in a saved buffer")
		(let* ((orig-file (car rtn-move-clipboard))
			   (orig-pos  (cdr rtn-move-clipboard))
			   (orig-anno (rtn-extra-get-annotation orig-file orig-pos)))
		  (if (null orig-anno)
			  (message "❌ Original annotation no longer exists")
			(rtn-delete-db orig-file orig-pos)

			(let* ((content (nth 2 orig-anno))
				   (icon    (or (nth 3 orig-anno) "📝"))
				   (new-pos (point)))
			  (rtn-extra-add-db target-file new-pos new-pos content icon)
			  (rtn-display-annotations)

			  (rtn-update-all-references orig-file orig-pos target-file new-pos)

			  (let ((orig-buffer (get-file-buffer orig-file)))
				(when (and orig-buffer
						   (not (equal orig-buffer (current-buffer))))
				  (with-current-buffer orig-buffer
					(rtn-display-annotations))))

			  (message "✅ Moved annotation and updated %d references"
					   (rtn-count-references orig-file orig-pos))

			  (setq rtn-move-clipboard nil))))))))

(defun rtn-count-references (target-file target-pos)
  "Count how many references point to TARGET-FILE:TARGET-POS."
  (let ((count 0))
	(dolist (ref-file (rtn-get-all-files))
	  (dolist (anno (rtn-get-file ref-file))
		(let ((content (nth 2 anno)))
		  (when (and (string-match "@POS:\\([0-9]+\\)@" content)
					 (string= (match-string 1 content) (number-to-string target-pos))
					 (string-match (regexp-quote target-file) content))
			(cl-incf count)))))
	count))

(defun rtn-update-all-references (orig-file orig-pos new-file new-pos)
  "Update all references pointing to ORIG-FILE:ORIG-POS to NEW-FILE:NEW-POS."
  (dolist (ref-file (rtn-get-all-files))
	(let ((annos (rtn-get-file ref-file)))
	  (dolist (anno annos)
		(let* ((ref-pos (nth 0 anno))
			   (content (nth 2 anno))
			   (icon    (nth 3 anno)))
		  (when (and (string-match (format "@POS:%d@" orig-pos) content)
					 (string-match (regexp-quote orig-file) content))
			(let ((new-content
				   (replace-regexp-in-string
					(regexp-quote orig-file) new-file
					(replace-regexp-in-string
					 (format "@POS:%d@" orig-pos)
					 (format "@POS:%d@" new-pos)
					 content t t) t t)))
			  (rtn-extra-update-db ref-file ref-pos new-content icon)
			  (when (equal (buffer-file-name) ref-file)
				(rtn-display-annotations)))))))))

;;;###autoload
(defun rtn-copy-reference-with-target ()
  "Copy reference annotation with its target intact."
  (interactive)
  (let* ((file (rtn-file))
		 (pos  (point))
		 (anno (rtn-extra-get-annotation file pos)))
	(if (null anno)
		(message "❌ Not on an annotation")
	  (let ((content (nth 2 anno))
			(icon    (or (nth 3 anno) "📝")))
		(if (not (string-match "@POS:[0-9]+@" content))
			(message "❌ Not a reference annotation")
		  (setq rtn-clipboard (cons (cons file pos) icon))
		  (message "✅ Copied reference: %s" (truncate-string-to-width content 50 nil nil "...")))))))

;;;###autoload
(defun rtn-paste-reference-with-target ()
  "Paste reference that points to original target."
  (interactive)
  (if (not rtn-clipboard)
	  (message "⚠️ Nothing copied")
	(let ((target-file (rtn-file)))
	  (if (not target-file)
		  (message "⚠️ Not in a saved buffer")
		(let* ((orig-file (car (car rtn-clipboard)))
			   (orig-pos  (cdr (car rtn-clipboard)))
			   (orig-anno (rtn-extra-get-annotation orig-file orig-pos)))
		  (if (null orig-anno)
			  (message "❌ Original annotation missing")
			(let* ((content (nth 2 orig-anno))
				   (icon    (or (nth 3 orig-anno) "📝")))
			  (rtn-extra-add-db target-file (point) (point) content icon)
			  (rtn-display-annotations)
			  (message "✅ Pasted reference to %s @POS:%d"
					   (file-name-nondirectory orig-file) orig-pos))))))))

(provide 'rtn-reference)
;;; rtn-reference.el ends here
