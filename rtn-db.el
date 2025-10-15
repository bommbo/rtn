;;; rtn-db.el --- Database layer for RTN (Rich Text Notes) -*- lexical-binding: t -*-

(require 'emacsql)
(require 'emacsql-sqlite-builtin)
(require 'json)
(require 'cl-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Basic Configuration ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup rtn nil
  "Database backend for RTN (Rich Text Notes)."
  :group 'local)

(defcustom rtn-db-file (expand-file-name "rtn.db" user-emacs-directory)
  "Path to the SQLite database file for RTN."
  :type 'file
  :group 'rtn)

(defvar rtn-db-conn nil
  "Current database connection for RTN.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Connection Management ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rtn-db ()
  "Return database connection, create if necessary."
  (unless (and rtn-db-conn (emacsql-live-p rtn-db-conn))
	(make-directory (file-name-directory rtn-db-file) t)
	(setq rtn-db-conn (emacsql-sqlite-builtin rtn-db-file))
	(emacsql rtn-db-conn
			 "CREATE TABLE IF NOT EXISTS annotations (
				id        INTEGER PRIMARY KEY AUTOINCREMENT,
				file      TEXT    NOT NULL,
				pos       INTEGER NOT NULL,
				end_pos   INTEGER NOT NULL,
				content   TEXT    NOT NULL,
				icon      TEXT,
				created_at TEXT,
				updated_at TEXT,
				UNIQUE(file, pos))")
	(emacsql rtn-db-conn
			 "CREATE TABLE IF NOT EXISTS annotation_tags (
				id        INTEGER PRIMARY KEY AUTOINCREMENT,
				tag       TEXT    NOT NULL,
				created_at TEXT)"))
  rtn-db-conn)

(defun rtn-close-db ()
  "Close database connection."
  (when (and rtn-db-conn (emacsql-live-p rtn-db-conn))
	(emacsql-close rtn-db-conn)
	(setq rtn-db-conn nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CRUD ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rtn-add-db (file pos end-pos content)
  "Insert or replace an annotation."
  (emacsql (rtn-db)
		   "INSERT OR REPLACE INTO annotations
			  (id, file, pos, end_pos, content, icon, created_at, updated_at)
			VALUES
			  (NULL, $s1, $s2, $s3, $s4, NULL, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)"
		   file pos end-pos content))

(defun rtn-get-db (file pos)
  "Return single annotation list for FILE POS."
  (car (emacsql (rtn-db)
				[:select [pos end_pos content icon created_at updated_at]
				 :from annotations
				 :where (and (= file $s1) (= pos $s2))]
				file pos)))

(defun rtn-get-file (file)
  "Return all annotations for FILE, ordered by pos."
  (emacsql (rtn-db)
		   [:select [pos end_pos content icon updated_at]
			:from annotations
			:where (= file $s1)
			:order-by pos]
		   file))

(defun rtn-get-all-files ()
  "Return distinct file names that have annotations."
  (mapcar #'car
		  (emacsql (rtn-db)
				   [:select :distinct file
					:from annotations])))

(defun rtn-update-db (file pos content)
  "Update content and timestamp for annotation at FILE POS."
  (emacsql (rtn-db)
		   [:update annotations
			:set [(= content $s1)
				  (= updated_at :CURRENT_TIMESTAMP)]
			:where (and (= file $s2) (= pos $s3))]
		   content file pos))

(defun rtn-delete-db (file pos)
  "Delete single annotation."
  (emacsql (rtn-db)
		   [:delete :from annotations
			:where (and (= file $s1) (= pos $s2))]
		   file pos))

(defun rtn-delete-file-db (file)
  "Delete all annotations for FILE."
  (emacsql (rtn-db)
		   [:delete :from annotations
			:where (= file $s1)]
		   file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Position Synchronization (New) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rtn-update-position-db (file old-pos new-pos new-end-pos)
  "Update position in database when overlay moves.
This is called when saving to sync overlay positions back to DB."
  (when (and file old-pos new-pos)
	(emacsql (rtn-db)
			 [:update annotations
			  :set [(= pos $s1) (= end_pos $s2) (= updated_at :CURRENT_TIMESTAMP)]
			  :where (and (= file $s3) (= pos $s4))]
			 new-pos new-end-pos file old-pos)))

(defun rtn-sync-overlays-to-db (&optional file)
  "Sync current overlay positions back to database.
Called on save to update positions after editing.
If FILE is nil, uses current buffer's file."
  (let ((target-file (or file (buffer-file-name))))
	(when target-file
	  (let ((synced-count 0))
		(dolist (ov (overlays-in (point-min) (point-max)))
		  (when (overlay-get ov 'rtn-original-pos)
			(let ((old-pos (overlay-get ov 'rtn-original-pos))
				  (new-pos (overlay-start ov))
				  (new-end-pos (overlay-end ov)))
			  ;; Only update when position actually changed
			  (unless (and (= old-pos new-pos)
						   (= (overlay-get ov 'rtn-original-end-pos) new-end-pos))
				(rtn-update-position-db target-file old-pos new-pos new-end-pos)
				;; Update overlay properties to record new original position
				(overlay-put ov 'rtn-original-pos new-pos)
				(overlay-put ov 'rtn-original-end-pos new-end-pos)
				(setq synced-count (1+ synced-count))))))
		(when (> synced-count 0)
		  (message "✓ Synced %d annotation%s position%s to database"
				   synced-count
				   (if (= synced-count 1) "" "s")
				   (if (= synced-count 1) "" "s")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Search and Statistics ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rtn-search-db (term)
  "Global content search."
  (rtn-db)
  (emacsql (rtn-db)
		   [:select [file pos end_pos content icon]
			:from annotations
			:where (like content $r1)
			:order-by (file pos)]
		   (concat "%" term "%")))

(defun rtn-search-file (file term)
  "Search content in FILE only."
  (emacsql (rtn-db)
		   [:select [pos end_pos content icon]
			:from annotations
			:where (and (= file $s1)
						(like content (concat "%" $s2 "%")))
			:order-by pos]
		   file term))

(defun rtn-count-all ()
  "Total number of annotations."
  (caar (emacsql (rtn-db) [:select (count) :from annotations])))

(defun rtn-count-file (file)
  "Number of annotations in FILE."
  (caar (emacsql (rtn-db)
				 [:select (count) :from annotations
				  :where (= file $s1)]
				 file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Import / Export ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rtn-export-json (file-path)
  "Export all annotations to FILE-PATH, return count."
  (let ((data (emacsql (rtn-db)
					   [:select [file pos end_pos content icon created_at updated_at]
						:from annotations
						:order-by file pos])))
	(with-temp-buffer
	  (insert (json-encode data))
	  (write-region (point-min) (point-max) file-path nil :silent))
	(length data)))

(defun rtn-import-json (file-path)
  "Import annotations from FILE-PATH, return count."
  (let ((json-array-type 'list)
		(json-object-type 'alist)
		(count 0))
	(with-temp-buffer
	  (insert-file-contents file-path)
	  (let ((rows (json-read)))
		(dolist (row rows)
		  (rtn-add-db (alist-get 'file row)
					  (alist-get 'pos row)
					  (alist-get 'end_pos row)
					  (alist-get 'content row))
		  (cl-incf count))
	  count))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Cleanup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rtn-clear-db ()
  "Delete all annotations (keep structure)."
  (emacsql (rtn-db) [:delete :from annotations]))

(provide 'rtn-db)
;;; rtn-db.el ends here
