;;; rtn-extra.el --- Extra features for RTN (Rich Text Notes) -*- lexical-binding: t -*-

(require 'rtn)
(require 'rtn-db)
(require 'ov)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Icon selection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst rtn-icon-alist
  '(("note"    . "📝")
	("star"    . "⭐")
	("warn"    . "❗")
	("check"   . "✅")
	("cross"   . "❌")
	("idea"    . "💡")
	("link"    . "🔗")
	("pin"     . "📌")
	("fix"     . "🔧")
	("bug"     . "🐛")
	("date"    . "📅")
	("info"    . "ℹ️")
	("question". "❓")
	("help"    . "🆘")
	("fire"    . "🔥")
	("new"     . "🆕")
	("soon"    . "🔜")
	("clock"   . "⏰")
	("time"    . "🕒")
	("user"    . "👤")
	("people"  . "👥")
	("lock"    . "🔒")
	("unlock"  . "🔓")
	("cloud"   . "☁️")
	("heart"   . "❤️")
	("like"    . "👍")
	("dislike" . "👎")
	("speech"  . "💬")
	("thought" . "💭")
	("mail"    . "✉️")
	("bell"    . "🔔")
	("mute"    . "🔕")
	("trash"   . "🗑️")
	("folder"  . "📁")
	("file"    . "📄")
	("book"    . "📖")
	("memo"    . "🗒️")
	("chart"   . "📊")
	("globe"   . "🌍")
	("rocket"  . "🚀")
	("gift"    . "🎁")
	("party"   . "🎉")
	("sad"     . "😢")
	("angry"   . "😠")
	("happy"   . "😊")
	("cool"    . "😎")
	("sleep"   . "😴")
	("zzz"     . "💤")
	("money"   . "💰")
	("diamond" . "💎")
	("key"     . "🔑")
	("phone"   . "📱")
	("computer". "💻")
	("tv"      . "📺")
	("camera"  . "📷")
	("music"   . "🎵")
	("sound"   . "🔊")
	("mute2"   . "🔇")
	("wifi"    . "📶")
	("battery" . "🔋")
	("electric". "⚡")
	("sun"     . "☀️")
	("moon"    . "🌙")
	("rain"    . "🌧️")
	("snow"    . "❄️")
	("umbrella". "☔")
	("fire2"   . "🧨")
	("skull"   . "💀")
	("ghost"   . "👻")
	("alien"   . "👽")
	("robot"   . "🤖")
	("flag"    . "🚩")
	("target"  . "🎯")
	("trophy"  . "🏆")
	("medal"   . "🏅")
	("1st"     . "🥇")
	("2nd"     . "🥈")
	("3rd"     . "🥉")
	("eye"     . "👀"))
  "Predefined icon set for RTN annotations.")

(defvar rtn-icon--display-alist nil)

(defun rtn-icon--build-display-alist ()
  (or rtn-icon--display-alist
	  (setq rtn-icon--display-alist
			(mapcar (lambda (pair)
					  (cons (format "%-6s %s" (car pair) (cdr pair))
							(cdr pair)))
					rtn-icon-alist))))

(defun rtn-icon-read ()
  "Prompt user to pick by *name + icon*, return pure emoji."
  (let* ((alist (rtn-icon--build-display-alist))
		 (cands (mapcar #'car alist))
		 (choice (completing-read "Icon: " cands nil t)))
	(alist-get choice alist nil nil #'string=)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Database: icon column
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar rtn-extra--icon-column-added nil)

(defun rtn-extra--ensure-icon-column ()
  (unless rtn-extra--icon-column-added
	(setq rtn-extra--icon-column-added t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Enhanced DB access
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rtn-extra-get-annotation (file pos)
  "Return annotation data list (pos end_pos content icon) for FILE and POS, or nil."
  (let ((row (car (emacsql (rtn-db)
						   [:select [pos end_pos content icon]
							:from annotations
							:where (and (= file $s1) (= pos $s2))]
						   file pos))))
	(when row
	  (list (nth 0 row) (nth 1 row) (nth 2 row) (nth 3 row)))))

(defun rtn-extra-add-db (file pos end-pos content icon)
  (rtn-extra--ensure-icon-column)
  (emacsql (rtn-db)
		   "INSERT OR REPLACE INTO annotations
			  (id, file, pos, end_pos, content, icon, created_at, updated_at)
			VALUES
			  (NULL, $s1, $s2, $s3, $s4, $s5, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)"
		   file pos end-pos content icon))

(defun rtn-extra-update-db (file pos content icon)
  (rtn-extra--ensure-icon-column)
  (emacsql (rtn-db)
		   [:update annotations
			:set [(= content $s1)
				  (= icon $s2)
				  (= updated_at :CURRENT_TIMESTAMP)]
			:where (and (= file $s3) (= pos $s4))]
		   content icon file pos))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Overlay display with movement support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rtn-extra--get-file (file)
  (emacsql (rtn-db)
		   [:select [pos end_pos content icon]
			:from annotations
			:where (= file $s1)
			:order-by pos]
		   file))

(defun rtn--truncate-content-for-help-echo (content)
  "Truncate CONTENT for help-echo display."
  (let* ((lines (split-string content "\n" t))
		 (first-line (car lines)))
	(truncate-string-to-width
	 (if (> (length lines) 1)
		 (concat first-line " …")
	   first-line)
	 50
	 nil nil "...")))

(defun rtn-extra--display-annotations ()
  "Display annotations with icons."
  (let ((file (rtn-file)))
	(when file
	  (rtn-clear-overlays file)
	  (let ((notes (rtn-extra--get-file file)))
		(dolist (note notes)
		  (let* ((pos (nth 0 note))
				 (end-pos (nth 1 note))
				 (content (nth 2 note))
				 (icon (nth 3 note))
				 (ov (ov pos end-pos
						 'face 'rtn-annotation-face
						 'rtn-content content
						 'evaporate nil
						 'modification-hooks '(rtn-overlay-modified)
						 'before-string
						 (propertize (or icon "📝")
									 'mouse-face 'highlight
									 'help-echo (rtn--truncate-content-for-help-echo content)))))
			(overlay-put ov 'rtn-original-pos pos)
			(overlay-put ov 'rtn-original-end-pos end-pos)
			(let ((overlays (gethash file rtn-annotations-overlays '())))
			  (puthash file (cons ov overlays) rtn-annotations-overlays))))))))

(defadvice rtn-display-annotations (around rtn-extra-display activate)
  (rtn-extra--display-annotations))

(defadvice rtn-get-file (around rtn-extra-get-file activate)
  (setq ad-return-value (rtn-extra--get-file (ad-get-arg 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Edit command with focus management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar rtn-edit-original-buffer nil)
(defvar rtn-edit-file nil)
(defvar rtn-edit-pos nil)
(defvar rtn-edit-end-pos nil)
(defvar rtn-edit-icon nil)

;;;###autoload
(defun rtn-add-edit-with-icon ()
  "Add or edit annotation at point with custom icon selection."
  (interactive)
  (let ((file (rtn-file)))
	(if (not file)
		(message "⚠️ Save buffer to file first")
	  (let* ((pos (point))
			 (annotation (rtn-extra-get-annotation file pos))
			 (old-content (or (and annotation (nth 2 annotation)) ""))
			 (old-icon (or (and annotation (nth 3 annotation)) "📝"))
			 (icon (rtn-icon-read))
			 (edit-buf (get-buffer-create "*RTN Edit*")))
		(setq rtn-edit-original-buffer (current-buffer))
		(setq rtn-edit-file file)
		(setq rtn-edit-pos pos)
		(setq rtn-edit-end-pos (if annotation (nth 1 annotation) pos))
		(setq rtn-edit-icon icon)
		(split-window-right (- (window-width) rtn-popup-width))
		(select-window (next-window))
		(switch-to-buffer edit-buf)
		(erase-buffer)
		(insert old-content)
		(rtn-edit-mode)
		(local-set-key (kbd "C-c C-c") #'rtn-extra-save-edit)
		(message "📝 Edit annotation | C-c C-c save | C-c C-k cancel")))))

(defun rtn-extra-save-edit ()
  "Save annotation and return to source buffer."
  (interactive)
  (let ((content (string-trim (buffer-string)))
		(file rtn-edit-file)
		(pos rtn-edit-pos)
		(end-pos rtn-edit-end-pos)
		(icon rtn-edit-icon)
		(source-buf rtn-edit-original-buffer))
	(cond
	 ((string-empty-p content)
	  (message "⚠️ Annotation cannot be empty"))
	 (t
	  ;; Save to DB
	  (if (rtn-extra-get-annotation file pos)
		  (rtn-extra-update-db file pos content icon)
		(rtn-extra-add-db file pos end-pos content icon))

	  ;; Close edit window
	  (delete-window)
	  (when (get-buffer "*RTN Edit*")
		(kill-buffer "*RTN Edit*"))

	  (message "✅ Annotation saved with icon %s" icon)

	  ;; Refresh annotation display in current file
	  (let ((file-buf (find-buffer-visiting file)))
		(when file-buf
		  (with-current-buffer file-buf
			(rtn-display-annotations))))

	  ;; Return to source buffer
	  (when (and source-buf (buffer-live-p source-buf))
		(let ((win (get-buffer-window source-buf 0)))
		  (if win
			  (select-window win)
			(pop-to-buffer source-buf))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Auto-sync positions on save
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rtn--sync-on-save ()
  "Sync annotation positions to DB after saving a file."
  (when (and (buffer-file-name)
			 (rtn-file))
	(rtn-sync-overlays-to-db)))

(add-hook 'after-save-hook #'rtn--sync-on-save)

(provide 'rtn-extra)
;;; rtn-extra.el ends here
