;;; rtn-list-tabulated.el --- Tabulated list UI for RTN -*- lexical-binding: t -*-

(require 'rtn-db)
(require 'rtn-extra) ; for rtn-icon-alist
(require 'cl-lib)

(defface rtn-tabulated-match-face
  '((t :inherit highlight :weight bold))
  "Face for matched terms in tabulated list content."
  :group 'rtn)

(defcustom rtn-tabulated-max-content-lines 3
  "Maximum number of lines to display for each annotation's content.
Set to nil to show all lines (may cause performance issues with very long notes).
Each line is truncated to 80 characters."
  :type '(choice (const :tag "Unlimited" nil)
				 (integer :tag "Max lines" :value 3))
  :group 'rtn)

(defun rtn--icon-to-name (icon)
  "Return the type name (e.g., \"bug\") for ICON (e.g., \"🐛\"), or \"note\" if unknown."
  (let ((pair (rassoc icon rtn-icon-alist)))
	(if pair (car pair) "note")))

(defun rtn--parse-filter-query (query)
  "Parse QUERY into AND-clauses (each is list of OR-terms).
- Split by ' && ' → AND clauses
- Each clause split by space → OR terms
Example: \"bug fix && type:todo\" → ((\"bug\" \"fix\") (\"type:todo\"))"
  (mapcar (lambda (clause)
			(split-string (string-trim clause) "[ \t]+" t))
		  (split-string query " && " t)))

(defun rtn--match-field-p (field value entry-data)
  "Match VALUE against FIELD in ENTRY-DATA."
  (let ((actual (cl-case (intern-soft field)
				  (type    (plist-get entry-data :type))
				  (icon    (plist-get entry-data :icon))
				  (file    (plist-get entry-data :file))
				  (pos     (plist-get entry-data :pos))
				  (content (plist-get entry-data :content))
				  (otherwise nil))))
	(and actual
		 (string-match-p (regexp-quote value) actual))))

(defun rtn--match-anywhere-p (term entry-data)
  "Match TERM against all fields."
  (let ((text (mapconcat #'identity
						 (list (plist-get entry-data :icon)
							   (plist-get entry-data :type)
							   (plist-get entry-data :file)
							   (plist-get entry-data :pos)
							   (plist-get entry-data :content))
						 " ")))
	(string-match-p (regexp-quote term) text)))

(defun rtn--match-entry-p (entry-data and-clauses)
  "Return t if ENTRY-DATA matches all AND clauses (each clause: match any term)."
  (cl-every
   (lambda (or-terms)
	 (cl-some
	  (lambda (term)
		(if (string-match "\\`\\([^:]+\\):\\(.+\\)\\'" term)
			(rtn--match-field-p (match-string 1 term) (match-string 2 term) entry-data)
		  (rtn--match-anywhere-p term entry-data)))
	  or-terms))
   and-clauses))

(defun rtn--highlight-matches (text terms)
  "Highlight plain TERMS (not field:value) in TEXT."
  (let ((plain-terms (cl-remove-if (lambda (x)
									 (and (stringp x)
										  (string-match-p ":" x)))
								   terms)))
	(dolist (term plain-terms text)
	  (unless (string-empty-p term)
		(setq text
			  (replace-regexp-in-string
			   (regexp-quote term)
			   (propertize term 'face 'rtn-tabulated-match-face)
			   text
			   t  ; LITERAL: t = replace all occurrences
			   ))))))

(defun rtn-tabulated-show-help ()
  "Show key bindings for RTN tabulated list mode."
  (interactive)
  (with-help-window "*RTN List Help*"
    (with-current-buffer standard-output
      (insert "RTN List Mode — Key Bindings\n\n")
      (insert "`RET` : Go to annotation in its file\n")
      (insert "`e`   : Edit annotation content\n")
      (insert "`d`   : Delete annotation\n")
      (insert "`o`   : preview annotation in other window\n")
      (insert "`g`   : Refresh list\n")
      (insert "`/`   : Filter annotations\n")
      (insert "`L`   : Show all annotations\n")
      (insert "`q`   : Quit window\n")
      (insert "`?`   : Show this help\n"))))

(defvar rtn-tabulated-mode-map
  (let ((map (make-sparse-keymap)))
	(set-keymap-parent map tabulated-list-mode-map)
	(define-key map (kbd "RET") #'rtn-tabulated-jump)
	(define-key map "e"         #'rtn-list-edit-annotation)
	(define-key map "d"         #'rtn-tabulated-delete)
	(define-key map "o"         #'rtn-tabulated-other-window)
	(define-key map "g"         #'rtn-tabulated-refresh)
	(define-key map "/"         #'rtn-tabulated-filter)
	(define-key map "?"         #'rtn-tabulated-show-help)
	(define-key map "L"         #'rtn-list-tabulated)
	map)
  "Keymap for `rtn-tabulated-mode'.")

(defvar-local rtn-tabulated--filter-term nil)
(defconst rtn-tabulated--content-indent (+ 12 1 20 1 6 1))

(defvar rtn-list-edit-original-buffer nil
  "Original RTN list buffer for edit session.")
(defvar rtn-list-edit-original-mode nil
  "Original major mode of the list buffer.")
(defvar rtn-list-edit-file nil)
(defvar rtn-list-edit-pos nil)
(defvar rtn-list-edit-icon nil)

(defun rtn-tabulated--format-content (raw)
  "Format RAW with line limit and indentation."
  (let* ((lines (split-string raw "\n" t))
		 (limited-lines
		  (cond
		   ((null rtn-tabulated-max-content-lines) lines)
		   ((<= (length lines) rtn-tabulated-max-content-lines) lines)
		   (t (append (cl-subseq lines 0 rtn-tabulated-max-content-lines) '("…")))))
		 (truncated-lines
		  (mapcar (lambda (line)
					(truncate-string-to-width line 80 0 nil ""))
				  limited-lines)))
	(mapconcat (lambda (line) line)
			   truncated-lines
			   (concat "\n" (make-string rtn-tabulated--content-indent ?\s)))))

(defun rtn-tabulated--build-entries (&optional filter-term)
  "Build entries with: space=OR, &&=AND, field prefixes, and highlighting."
  (let* ((entries '())
		 (and-clauses (and filter-term (rtn--parse-filter-query filter-term)))
		 (all-terms (and and-clauses (apply #'append and-clauses))))
	(dolist (file (rtn-get-all-files))
	  (let ((notes (rtn-get-file file)))
		(dolist (note notes)
		  (let* ((pos          (nth 0 note))
				 (raw          (or (nth 2 note) ""))
				 (stored-icon  (or (nth 3 note) "📝"))
				 (type-name    (rtn--icon-to-name stored-icon))
				 (file-base    (file-name-nondirectory file))
				 (pos-str      (number-to-string pos))
				 (entry-data `(:icon ,stored-icon
								 :type ,type-name
								 :file ,file-base
								 :pos ,pos-str
								 :content ,raw))
				 (match? (or (null and-clauses)
							 (rtn--match-entry-p entry-data and-clauses)))
				 (display-type (concat stored-icon " " type-name)))
			(when match?
			  (let* ((content-to-display
					  (if all-terms
						  (rtn--highlight-matches raw all-terms)
						raw))
					 (formatted-content (rtn-tabulated--format-content content-to-display))
					 (id      (cons file pos))
					 (row     (vector display-type file-base pos-str formatted-content)))
				(push (list id row) entries)))))))
	(nreverse entries)))

(defun rtn-tabulated--get-current-id ()
  (let ((id (tabulated-list-get-id)))
	(unless id (user-error "No annotation on this line"))
	id))

(defun rtn-tabulated-jump ()
  "Jump to annotation location and close the list window."
  (interactive)
  (let* ((id (rtn-tabulated--get-current-id))
		 (file (car id))
		 (pos (cdr id))
		 (list-buffer (current-buffer)))
	(when (file-exists-p file)
	  (let ((buf (or (get-file-buffer file) (find-file-noselect file))))
		(pop-to-buffer buf)
		(goto-char pos)
		(pulse-momentary-highlight-one-line))
	  (let ((list-win (get-buffer-window list-buffer)))
		(when (window-live-p list-win)
		  (delete-window list-win))))))

;;;###autoload
(defun rtn-list-edit-annotation ()
  "Edit annotation in RTN edit buffer, maintain original window layout."
  (interactive)
  (let* ((id (rtn-tabulated--get-current-id))
		 (file (car id))
		 (pos (if (stringp (cdr id))
				  (string-to-number (cdr id))
				(cdr id)))
		 (annotation (rtn-extra-get-annotation file pos))
		 (content "")
		 (icon "📝"))
	(when annotation
	  (setq content (nth 2 annotation))
	  (setq icon (nth 3 annotation)))
	(when (file-exists-p file)
	  (setq rtn-list-edit-original-buffer (current-buffer))
	  (setq rtn-list-edit-original-mode major-mode)  ; ← 保存原始模式
	  (let ((edit-buf (get-buffer-create "*RTN Edit*")))
		(pop-to-buffer edit-buf)
		(erase-buffer)
		(insert (or content ""))
		(setq rtn-list-edit-file file)
		(setq rtn-list-edit-pos pos)
		(setq rtn-list-edit-icon (or icon "📝"))
		(rtn-list-edit-mode)
		(local-set-key (kbd "C-c C-c") #'rtn-list-edit-save)
		(local-set-key (kbd "C-c C-k") #'rtn-list-edit-cancel)
		(message "📝 RTN Edit | C-c C-c save | C-c C-k cancel")))))

(defun rtn-list-edit-return-to-list ()
  "Return to RTN list after saving."
  (interactive)
  (when (and rtn-list-edit-original-buffer
			 (buffer-live-p rtn-list-edit-original-buffer))
	(let ((win (get-buffer-window rtn-list-edit-original-buffer 0)))
	  (if win
		  (select-window win)
		(pop-to-buffer rtn-list-edit-original-buffer)))
	(setq rtn-list-edit-original-buffer nil)
	(remove-hook 'after-save-hook #'rtn-list-edit-return-to-list t))
  (message "✅ Returned to RTN list"))

(define-derived-mode rtn-list-edit-mode text-mode "RTN-List-Edit"
  "Mode for RTN list editing."
  (local-set-key (kbd "C-c C-c") #'rtn-list-edit-save)
  (local-set-key (kbd "C-c C-k") #'rtn-list-edit-cancel))

(defun rtn-list-edit-save ()
  "Save the current annotation, close the edit buffer, and return to list view."
  (interactive)
  (let ((content (string-trim (buffer-string))))
	(if (string-empty-p content)
		(message "⚠️ Annotation cannot be empty")
	  (let ((file rtn-list-edit-file)
			(pos rtn-list-edit-pos)
			(icon rtn-list-edit-icon)
			(edit-buf (current-buffer)))

		(if (rtn-extra-get-annotation file pos)
			(rtn-extra-update-db file pos content icon)
		  (rtn-extra-add-db file pos (1+ pos) content icon))
		(message "✅ Annotation saved")

		(when (buffer-live-p edit-buf)
		  (kill-buffer edit-buf))

		(when (and rtn-list-edit-original-buffer
				   (buffer-live-p rtn-list-edit-original-buffer))
		  (let ((list-buf rtn-list-edit-original-buffer)
				(original-mode rtn-list-edit-original-mode)
				(target-id (cons file pos)))
			(if (get-buffer-window list-buf 0)
				(select-window (get-buffer-window list-buf 0))
			  (pop-to-buffer list-buf))

			(cond
			 ((eq original-mode 'rtn-search-tabulated-mode)
			  (with-current-buffer list-buf
				(let ((search-term rtn-tabulated--filter-term))
				  (if search-term
					  (rtn-tabulated-filter search-term)
					(rtn-tabulated-refresh)))))
			 ((eq original-mode 'rtn-tabulated-mode)
			  (rtn-tabulated-refresh))
			 (t
			  (rtn-tabulated-refresh)))

			(condition-case err
				(progn
				  (goto-char (point-min))
				  (let ((found nil))
					(while (and (not (eobp)) (not found))
					  (if (equal (tabulated-list-get-id) target-id)
						  (setq found t)
						(forward-line 1)))
					(if found
						(progn
						  (beginning-of-line)
						  (recenter 0))
					  (message "Saved — but could not locate the entry in the list."))))
			  (error (message "Error restoring RTN list view: %S" err))))))
		;; 清理变量
		(setq rtn-list-edit-original-buffer nil
			  rtn-list-edit-original-mode nil))))

(defun rtn-list-edit-cancel ()
  "Cancel RTN list edit and return to list."
  (interactive)
  (delete-window)
  (kill-buffer "*RTN Edit*")
  (when (and rtn-list-edit-original-buffer
			 (buffer-live-p rtn-list-edit-original-buffer))
	(let ((win (get-buffer-window rtn-list-edit-original-buffer 0)))
	  (if win
		  (select-window win)
		(pop-to-buffer rtn-list-edit-original-buffer)))
	(setq rtn-list-edit-original-buffer nil
		  rtn-list-edit-original-mode nil))
  (message "Cancelled"))

(defun rtn-tabulated-delete ()
  (interactive)
  (let* ((id (rtn-tabulated--get-current-id))
		 (file (car id))
		 (pos (cdr id)))
	(when (y-or-n-p (format "Delete note at %s:%s? " (file-name-nondirectory file) pos))
	  (rtn-delete-db file pos)
	  (let ((buf (get-file-buffer file)))
		(when buf
		  (with-current-buffer buf
			(when (fboundp 'rtn-display-annotations)
			  (rtn-display-annotations)))))
	  (rtn-tabulated-refresh)
	  (message "Deleted."))))

(defun rtn-tabulated-other-window ()
  (interactive)
  (let* ((id (rtn-tabulated--get-current-id))
		 (file (car id))
		 (pos (cdr id)))
	(when (file-exists-p file)
	  (let ((buf (or (get-file-buffer file) (find-file-noselect file))))
		(save-selected-window
		  (pop-to-buffer buf 'other-window)
		  (goto-char pos)
		  (recenter)
		  (pulse-momentary-highlight-one-line))))))

(defun rtn-tabulated-refresh ()
  "Refresh the tabulated list, keeping point on the same entry if possible."
  (interactive)
  (unless (memq major-mode '(rtn-tabulated-mode rtn-search-tabulated-mode))
	(user-error "Not in RTN tabulated list buffer"))
  (let* ((origin-id (tabulated-list-get-id))
		 (origin-line (line-number-at-pos)))
	(setq tabulated-list-entries
		  (rtn-tabulated--build-entries rtn-tabulated--filter-term))
	(tabulated-list-print t)
	(when origin-id
	  (goto-char (point-min))
	  (while (and (not (eobp))
				  (not (equal (tabulated-list-get-id) origin-id)))
		(forward-line 1)))
	(unless (equal (tabulated-list-get-id) origin-id)
	  (goto-char (point-min))
	  (forward-line (1- (max 1 (min origin-line (count-lines (point-min) (point-max)))))))
	(run-at-time 0 nil
				 (lambda (win)
				   (when (and (window-live-p win)
							  (eq (window-buffer win) (current-buffer)))
					 (with-selected-window win
					   (recenter))))
				 (get-buffer-window (current-buffer)))))

(defun rtn-tabulated-filter (term)
  "Filter with: space=OR, &&=AND.
- Terms separated by space: match any (OR)
- Terms separated by '&&': all must match (AND)
- Field prefixes: type:todo, file:main.c, content:FIXME
Examples:
  \"bug fix\"            → bug OR fix
  \"bug && fix\"         → bug AND fix
  \"type:todo && file:.el\" → todo in .el files"
  (interactive "sFilter (space=OR, &&=AND): ")
  (setq rtn-tabulated--filter-term (unless (string-empty-p term) term))
  (rtn-tabulated-refresh))

(define-derived-mode rtn-tabulated-mode tabulated-list-mode "RTN-List"
  "Tabulated list of RTN annotations."
  :keymap rtn-tabulated-mode-map
  (setq tabulated-list-format
		[("Type"    12 nil)
		 ("File"    20 t)
		 ("Pos"      6 nil)
		 ("Content"  0 t)])
  (setq rtn-tabulated--filter-term nil)
  (tabulated-list-init-header))

;;;###autoload
(defun rtn-list-tabulated ()
  "List all RTN annotations in a split window."
  (interactive)
  (let ((buf (get-buffer-create "*RTN List*")))
	(with-current-buffer buf
	  (rtn-tabulated-mode)
	  (rtn-tabulated-refresh))
	(pop-to-buffer buf)))

;;;###autoload
(defun rtn-search-tabulated (term)
  "Search RTN annotations and display in *RTN Search* list."
  (interactive "sSearch annotations: ")
  (let* ((entries (rtn-search-tabulated--build-entries term))
		 (buf     (get-buffer-create "*RTN Search*")))
	(if (null entries)
		(message "No results for \"%s\"" term)
	  (with-current-buffer buf
		(rtn-tabulated-mode)
		(setq rtn-tabulated--filter-term term)
		(setq tabulated-list-entries entries)
		(tabulated-list-init-header)
		(tabulated-list-print t))
	  (pop-to-buffer buf))))

(defun rtn-search-to-list ()
  "Replace current search buffer with full annotation list."
  (interactive)
  (unless (eq major-mode 'rtn-tabulated-mode)
	(user-error "Not in RTN Search buffer"))
  (rtn-tabulated-mode)
  (rtn-tabulated-refresh)
  (rename-buffer "*RTN List*" t))

(defun rtn-search-tabulated--build-entries (term)
  "Build entries for search TERM."
  (let ((results (rtn-search-db term))
		(entries '()))
	(dolist (r results)
	  (let* ((file (nth 0 r))
			 (pos (nth 1 r))
			 (raw (or (nth 3 r) ""))
			 (stored-icon (or (nth 4 r) "📝"))
			 (type-name (rtn--icon-to-name stored-icon))
			 (display-type (concat stored-icon " " type-name))
			 (content (rtn-tabulated--format-content raw))
			 (id (cons file pos))
			 (row (vector display-type
						  (file-name-nondirectory file)
						  (number-to-string pos)
						  content)))
		(push (list id row) entries)))
	(nreverse entries)))

(provide 'rtn-list-tabulated)
;;; rtn-list-tabulated.el ends here
