;; -*- lexical-binding: t -*-

;; MIT License
;; 
;; Copyright (c) 2020 ifritJP (emacs-reviewboard-front)
;; 
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;; 
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;; 
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.


(require 'rbfront-lib)

(defvar rb/front-submit-and-publish-p t
  "nil 以外の場合、submit と同時に publish する。")
(defvar rb/front-display-comment-p t
  "nil 以外の場合 review comments を表示する。")

(defvar rb/front-edit-info-default
  '(:title "new title"
	   :description "input description"
	   :test "input test")
  "新規 review request のデフォルト情報")

(defvar rb/front-list-buf "*rb/front/list*")
(defvar rb/front-edit-buf "*rb/front/edit*")
(defvar rb/front-text-buf "*rb/front/text*")

(defvar rb/front-prev-basedir nil)

(defvar rb/front-face-message 'font-lock-doc-face)
(defvar rb/front-face-item 'font-lock-comment-face)
(defvar rb/front-face-emphasis 'font-lock-builtin-face)
(defvar rb/front-face-warning 'font-lock-warning-face)
(defvar rb/front-face-bold 'bold)



(defun rb/front-insert-readOnlyText-multi (text-face-list)
  "編集禁止文字列を挿入する。

text-face-list は \(text face\) を要素にもつリスト。"
  (let ((start (point)) (inhibit-read-only t))
    (dolist (text-face text-face-list)
      (let ((text (nth 0 text-face))
	    (face (nth 1 text-face)))
	(if face
	    (insert (propertize text 'face face))
	  (insert text))
	)
      (put-text-property (if (< 1 start)
			     (1- start)
			   start)
			 (1- (point)) 'read-only t))))


(defun rb/front-insert-readOnlyText (text face &optional annex)
  "禁止文字列 text を face で修飾して挿入する。

annex は、text に付加する禁止文字列。 face はデフォルト。"
  (let ((alist (list (list text face))))
    (when annex
      (add-to-list 'alist (list annex nil) t))
    (rb/front-insert-readOnlyText-multi alist)))


(defun rb/front-insert-key-bind-doc (bind-message-list)
  "key-bind を説明する文字列を挿入する。

bind-message-list は、 \(message bind\) を要素にもつリスト。
"
  (let (alist)
    (setq alist (delq nil (apply 'append
				 (mapcar (lambda (X)
					   (list (list (car X) rb/front-face-message)
						 (when (cadr X)
						   (list (cadr X) rb/front-face-item))))
					 bind-message-list))))
    (rb/front-insert-readOnlyText-multi alist)
    ))

(defun rb/front-list-get-rrq-txt (rrq)
  "review request を示す文字列を返す。"
  (if rrq
      (let* ((id (plist-get rrq :id))
	     (draft (if (rb/front-has-draft id)
			(propertize "draft " 'face rb/front-face-warning)
		      (propertize "public" 'face rb/front-face-emphasis))))
	(format "%6d: %s: %s: %s"
		id
		draft
		(propertize
		 (plist-get rrq :last_updated)
		 'face rb/front-face-item)
		(plist-get rrq :summary)))
    ""
    ))

(defvar rb/front-list-rrq-list nil)

(defun rb/front-list-update-rrq-list ()
  (setq rb/front-list-rrq-list
	(sort (rb/front-get-my-reported-rrq-list)
	      (lambda (val1 val2)
		(> (plist-get val1 :id)
		   (plist-get val2 :id))))))

(defun rb/front-list-redraw-view ()
  (interactive)
  (let ((inhibit-read-only t)
	(buf (get-buffer-create rb/front-list-buf))
	info backup-lineno)
    (rb/front-list-update-rrq-list)
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (setq backup-lineno (line-number-at-pos))
      (erase-buffer)

      (rb/front-insert-key-bind-doc
       '(("reload: " "(g)" )
	 (", edit: " "(RET)")
	 ("\nupload-diff: " "(u)" )
	 (", publish: " "(p)" )
	 ( ", close: " "(c)" )
	 (", discard: " "(d)" )
	 ("\ncommit: " "(C)" )
	 ("\n----\n" ) ) )
      (rb/front-insert-readOnlyText-multi
       (list (list "last-update: " rb/front-face-item)
	     (list (format-time-string "%T\n") rb/front-face-emphasis)
	     (list "----\n" rb/front-face-message)
	     ))
      
      (dolist (rrq rb/front-list-rrq-list)
	(insert (rb/front-list-get-rrq-txt rrq))
	(insert "\n")
	)

      (goto-line backup-lineno)

      (setq buffer-read-only t)
      )
    ))


(defun rb/front-switch-to-buffer-other-window (buf)
  "buf を別 window に表示する。

buf が既にいずれかの window に表示されている場合は、その window に切り替える。"
  (if (not (get-buffer-window buf))
      (switch-to-buffer-other-window buf)
    (select-window (get-buffer-window buf))
    ))

(defvar rb/front-list-mode-hook nil
  "Hooks called when rb/front-list mode fires up.")

(define-derived-mode rb/front-list-mode
  text-mode "rb/front-list"
  "Major mode for Review Board.
          \\{rb/front-list-mode-map}"
  (rb/front-list-redraw-view)
  
  (rb/front-switch-to-buffer-other-window (get-buffer-create rb/front-list-buf))  
  )

(define-key rb/front-list-mode-map (kbd "C") 'rb/front-list-commit-current)
(define-key rb/front-list-mode-map (kbd "c") 'rb/front-list-close-current)
(define-key rb/front-list-mode-map (kbd "d") 'rb/front-list-discard-current)
(define-key rb/front-list-mode-map (kbd "p") 'rb/front-list-publish-current)
(define-key rb/front-list-mode-map (kbd "u") 'rb/front-list-upload-current)
(define-key rb/front-list-mode-map (kbd "g") 'rb/front-list-redraw-view)
(define-key rb/front-list-mode-map (kbd "RET") 'rb/front-list-edit-current)




(defun rb/front-list ()
  (interactive)
  (with-current-buffer (get-buffer-create rb/front-list-buf)
    (rb/front-list-mode))
  )

(defun rb/front-list-current-rrq ()
  (let ((index (- (line-number-at-pos) 7)))
    (if (and (>= index 0)
	     (< index (length rb/front-list-rrq-list)))
	(nth index rb/front-list-rrq-list)
      nil
    )
    ))

(defun rb/front-list-current-do (callback)
  (let ((rrq (rb/front-list-current-rrq)))
    (when rrq
      (funcall callback rrq))))


(defun rb/front-list-commit-current ()
  (interactive)
  (rb/front-list-current-do
   (lambda (rrq)
     (rb/front-commit-view (rb/front-edit-convert-rrq-to-info rrq))
    )))

(defun rb/front-list-edit-current ()
  (interactive)
  (rb/front-list-current-do
   (lambda (rrq)
     (rb/front-edit-view (plist-get rrq :id))
    )))
(defun rb/front-list-upload-current ()
  (interactive)
  (rb/front-list-current-do
   (lambda (rrq)
     (rb/front-edit-update-diff-for (rb/front-edit-convert-rrq-to-info rrq) t)
    )))
(defun rb/front-list-publish-current ()
  (interactive)
  (rb/front-list-current-do
   (lambda (rrq)
     (when (y-or-n-p "publish?: ")
       (rb/front-draft-publish (rb/front-access rrq :id))
       (rb/front-list-redraw-view)
       )))
  )
(defun rb/front-list-close-current ()
  (interactive)
  (rb/front-list-current-do
   (lambda (rrq)
     (when (y-or-n-p "close?: ")
       (rb/front-close-rrq rrq)
       (sleep-for 1)
       (rb/front-list-redraw-view)
       )))
  )
(defun rb/front-list-discard-current ()
  (interactive)
  (rb/front-list-current-do
   (lambda (rrq)
     (when (y-or-n-p "discard?: ")
       (rb/front-discard-rrq (rb/front-access rrq :id))
       (sleep-for 1)
       (rb/front-list-redraw-view)
       )))
  )

(defun rb/front-file-info-normalize (file-info)
  "file-info を正規化する。

file-info は次の値を持つ。

\(:tool TOOL
  :top-dir TOP-DIR
  :base-dir BASE-DIR
  :file-list FILE-LIST\)

FILE-LIST は、ローカルの BASE-DIR からの相対パスになっているので、
reviewboard に登録されるパスに変換する。
TOP-DIR に、ローカルのワークディレクトリのトップディレクトリを設定する。
 "
  (let* ((tool (rb/front-file-info-get-tool file-info))
	 (base-dir (rb/front-file-info-get-base-dir file-info))
	 (file-list (rb/front-file-info-get-file-list file-info))
	 rb-path-list conv-info work-root)
    (setq conv-info (rb/front-tool-get-work-info tool base-dir))
    (setq work-root (plist-get conv-info :root-path))
    (setq rb-path-list
	  (mapcar (lambda (X)
		    (let ((full-path (expand-file-name X base-dir)))
		      (when (file-directory-p full-path)
			;; ディレクトリはエラー
			(error (format "not support directory -- %s" X))
			)
		      (rb/front-path-join
		       (plist-get conv-info :root-url-relative)
		       (file-relative-name full-path work-root))
		       ))
		  file-list))
    (plist-put file-info :file-list rb-path-list)
    (plist-put file-info :top-dir work-root)
    file-info
    ))


(defun rb/front-file-info-create (tool base-dir file-list)
  (rb/front-file-info-normalize (list :tool tool
				      :top-dir nil
				      :base-dir (expand-file-name base-dir)
				      :file-list file-list))
  )

(defun rb/front-file-info-get-tool (file-info)
  (plist-get file-info :tool))
(defun rb/front-file-info-get-top-dir (file-info)
  (plist-get file-info :top-dir))
(defun rb/front-file-info-get-base-dir (file-info)
  (plist-get file-info :base-dir))
(defun rb/front-file-info-get-file-list (file-info)
  (plist-get file-info :file-list))

(defun rb/front-edit-insert-file-info (path mark-p)
  (rb/front-insert-readOnlyText
   (concat (if mark-p
	       " [X]: "
	     " [ ]: ")
	   path)
   rb/front-face-warning "\n")
  )



(defvar rb/front-edit-info-keys
  '(:tool :top-dir :base-dir :id :state :title :description :test :file-list
	  :unmark-files))

(defvar rb/front-edit-during-to-add-file nil)

(defun rb/front-edit-add-file (file-info)
  (if (not rb/front-edit-during-to-add-file)
      ;; rb/front-edit-during-to-add-file が nil の場合、新規投稿
      (rb/front-edit-view nil file-info)
    ;; 既存の rrq に追加登録
    (setq rb/front-edit-during-to-add-file nil)
    (with-current-buffer (get-buffer-create rb/front-edit-buf)
      (let ((info (copy-sequence rb/front-edit-info))
	    (file-list (plist-get rb/front-edit-info :file-list))
	    )

	;; 被ってないファイルを追加する
	(dolist (file (rb/front-file-info-get-file-list file-info))
	  (when (not (member file file-list))
	    (add-to-list 'file-list file t)))
	(plist-put info :file-list file-list)
	(plist-put info :top-dir (rb/front-file-info-get-top-dir file-info))
	(plist-put info :base-dir (rb/front-file-info-get-base-dir file-info))
	(rb/front-edit-redraw info)
	(when (not (eq (plist-get rb/front-edit-info :state) "new"))
	  (rb/front-edit-update-diff-for info rb/front-submit-and-publish-p))
	))
    (rb/front-switch-to-buffer-other-window rb/front-edit-buf)
    ))

(defun rb/front-edit-commit ()
  (interactive)
  (rb/front-edit-apply-info)
  (rb/front-commit-view rb/front-edit-info)
)

(defun rb/front-edit-redraw (&optional info)
  (interactive)
  (with-current-buffer (get-buffer-create rb/front-edit-buf)
    (if info
	(rb/front-edit-apply-info-2-list info)
      (rb/front-edit-apply-info))
    (setq info rb/front-edit-info)
    (let ((inhibit-read-only t)
	  (id (plist-get info :id))
	  (modified-p (buffer-modified-p))
	  (backup-lineno (line-number-at-pos))
	  (unmarked-list (plist-get info :unmark-files))
	  reply-hash)
      (erase-buffer)
      (rb/front-insert-key-bind-doc
       '(("submit: " "(C-c C-c)" )
	 (", publish: " "(C-c C-p)")
	 (", cancel: " "(kill-buffer)")
	 ("\nsave to draft: " "(C-x C-s)" )
	 (", discard draft: " "(C-c D)" )
	 (", toggle publish mode: " "(C-c C-t)" )
	 ("\nredraw: " "(C-c R)" )
	 ("\n------\n" )))
      (rb/front-insert-readOnlyText-multi
       `(("submit-mode: " ,rb/front-face-item)
	 (,(if rb/front-submit-and-publish-p
	       "submit and publish"
	     "only submit")
	  ,rb/front-face-warning)
	 ("\n" nil)))
      (rb/front-insert-readOnlyText-multi
       (list (list "url: " rb/front-face-item)
	     (list (if id (format "%sr/%d/\n" rb/front-rb-url id)
		     "--\n")
		   rb/front-face-warning)))
      (rb/front-insert-readOnlyText (format "tool: %s\n" (plist-get info :tool))
				    rb/front-face-item)
      (rb/front-insert-readOnlyText (format "top-dir: %s\n"
					    (or (plist-get info :top-dir)
						""))
				    rb/front-face-item)
      (rb/front-insert-readOnlyText (format "base-dir: %s\n"
					    (or (plist-get info :base-dir)
						""))
				    rb/front-face-item)
      (rb/front-insert-readOnlyText (format "id: %s\n" (or id "--"))
				    rb/front-face-item)
      (rb/front-insert-readOnlyText-multi
       `(("state: " ,rb/front-face-item)
	 (,(plist-get info :state) ,rb/front-face-warning)
	 ("\n" nil)))
      (rb/front-insert-readOnlyText "title (only 1 line):"
				    rb/front-face-emphasis " ")
      (insert (or (plist-get info :title)
		  (plist-get rb/front-edit-info-default :title)))
      (insert "\n")
      (rb/front-insert-readOnlyText "description: --->"
				    rb/front-face-emphasis "\n")
      (insert (or (plist-get info :description)
		  (plist-get rb/front-edit-info-default :description)))
      (insert "\n")
      (rb/front-insert-readOnlyText "<---\n"
				    rb/front-face-emphasis)
      (rb/front-insert-readOnlyText "test: --->"
				    rb/front-face-emphasis "\n")
      (insert (or (plist-get info :test)
		  (plist-get rb/front-edit-info-default :test)))
      (insert "\n")
      (rb/front-insert-readOnlyText "<---\n"
				    rb/front-face-emphasis)
      (rb/front-insert-readOnlyText "files: --->"
				    rb/front-face-emphasis "\n")

      (rb/front-insert-key-bind-doc
       '(("toggle mark: " "(C-c C-SPC)")
	 (", update diff: " "(C-c C-u)" )
	 (", add file: " "(C-c C-a)")
	 ("\ncommit: " "(C-c C)")
	 ("\n")))
      (dolist (file (plist-get info :file-list))
	(rb/front-edit-insert-file-info file
					(not (member file unmarked-list))))
      (rb/front-insert-readOnlyText "<---\n"
				    rb/front-face-emphasis)


      (when id
	;; レビューコメント表示
	(rb/front-insert-readOnlyText "review comments: --->"
				      rb/front-face-emphasis "\n")
	(rb/front-insert-key-bind-doc
	 '(("toggle display comments: " "(C-c C-d)")
	   (", nadd reply: " "(C-c C-r)")
	   ("\n")))

	(if (not rb/front-display-comment-p)
	    (rb/front-insert-readOnlyText
	     "Please hit key 'C-c C-d' to display review comments"
	     rb/front-face-warning)
	  (setq reply-hash (rb/front-get-reply-hash id))

	  (if (not (rb/front-get-review-list id))
	      (rb/front-insert-readOnlyText
	       "No review comment\n" rb/front-face-warning)
	    (dolist (review (rb/front-get-review-list id))
	      (rb/front-insert-readOnlyText-multi
	       (list (list (format "(%d:%d:" (plist-get review :review-id)
				   (plist-get review :id) ) rb/front-face-bold)
		     (if (equal (plist-get review :issue_status) "open")
			 (list "open" rb/front-face-warning)
		       (list "close" rb/front-face-bold))
		     (list (format "):%s:%s\n"
				   (rb/front-repstr 
				    (rb/front-access review :links :filediff :title)
				    " ([^()]+) ->.*" "" t)
				   (plist-get review :first_line))
			   rb/front-face-bold)))
	      (rb/front-insert-readOnlyText (plist-get review :text)
					    rb/front-face-item "\n")
	      ;; レスポンス表示
	      (let ((reply-list (gethash (plist-get review :id) reply-hash)))
		(dolist (reply reply-list)
		  (rb/front-insert-readOnlyText
		   (format "--> %s" (plist-get reply :text))
		   rb/front-face-message "\n")))
	      ))
	  (rb/front-insert-readOnlyText "<---"
					rb/front-face-emphasis "\n"))
	)

      (goto-line backup-lineno)
      
      (set-buffer-modified-p modified-p)
      )
    ))


(defvar rb/front-edit-mode-hook nil
  "Hooks called when rb/front-edit mode fires up.")

(defvar rb/front-edit-info nil)

(define-derived-mode rb/front-edit-mode
  text-mode "rb/front-edit"
  "Major mode for Review Board.
          \\{rb/front-edit-mode-map}"

  (when (not (boundp 'rb/front-edit-info))
    (set (make-local-variable 'rb/front-edit-info) nil))
  )

(define-key rb/front-edit-mode-map (kbd "C-c C-a") 'rb/front-edit-add-file-to)
(define-key rb/front-edit-mode-map (kbd "C-c C-SPC") 'rb/front-edit-toggle-marking-file)
(define-key rb/front-edit-mode-map (kbd "C-c C-c") 'rb/front-edit-submit)
(define-key rb/front-edit-mode-map (kbd "C-c D") 'rb/front-edit-discard-draft)
(define-key rb/front-edit-mode-map (kbd "C-c C-p") 'rb/front-edit-publish)
(define-key rb/front-edit-mode-map (kbd "C-c C-t") 'rb/front-edit-toggle-publish-mode)
(define-key rb/front-edit-mode-map (kbd "C-c C-u") 'rb/front-edit-update-diff)
(define-key rb/front-edit-mode-map (kbd "C-c R") 'rb/front-edit-redraw)
(define-key rb/front-edit-mode-map (kbd "C-c C-d") 'rb/front-edit-toggle-display-review)
(define-key rb/front-edit-mode-map (kbd "C-c C") 'rb/front-edit-commit)
(define-key rb/front-edit-mode-map (kbd "C-c C-r") 'rb/front-edit-set-reply)
(define-key rb/front-edit-mode-map (kbd "C-x C-s") 'rb/front-edit-save-to-draft)

(defun rb/front-edit-get-info-at (id)
  (rb/front-edit-convert-rrq-to-info (rb/front-get-request id)))

(defun rb/front-edit-view (id &optional file-info)
  "指定 id の review request を編集する。

新規の場合は id に nil を指定し、 file-info を指定しなければならない。
file-info は rb/front-file-info-create で生成する。
"
  (let* ((buf (get-buffer-create rb/front-edit-buf))
	 info tool base-dir file-list rrq)
    (with-current-buffer buf
      (rb/front-edit-mode)
      (setq rb/front-edit-info (copy-sequence '(:dummy nil :unmark-files (nil))))
      (if id
	  (progn
	    (setq rrq (rb/front-get-request id))
	    (setq info (rb/front-edit-convert-rrq-to-info rrq))
	    (setq tool (rb/front-get-repository-tool-at-request rrq)))
	;; 規新登録の場合
	(setq tool (rb/front-file-info-get-tool file-info)
	      base-dir (rb/front-file-info-get-base-dir file-info)
	      info (list :tool tool
			 :state "new"
			 :top-dir (rb/front-file-info-get-top-dir file-info)
			 :base-dir base-dir
			 :file-list (rb/front-file-info-get-file-list file-info))
	      )
	(setq rb/front-prev-basedir base-dir)
	)
      (when base-dir
	(rb/front-set-default-dir base-dir))

      (rb/front-edit-redraw info)
      )
    (switch-to-buffer buf)
    (beginning-of-buffer)
    ;;(perform-replace "\r" "" nil nil nil)  
    (beginning-of-buffer)
    (re-search-forward "title.*: ")
    
    (if id
	;; id を指定している場合、既存 rrq の変数なので、変更なしで初期化
	(set-buffer-modified-p nil)
      ;; id を指定していない場合、新規 rrq 投稿なので、変更ありで初期化
      (set-buffer-modified-p t))
    )
  )

(defun rb/front-get-work-dir (id)
  (let (dir default-dir)
    (when id
      (setq default-dir (plist-get (rb/front-setting-get-work-info id) :base-dir)))
    (when (not default-dir)
      (setq default-dir rb/front-prev-basedir))
    (setq dir (read-file-name "work-dir?: " "/" default-dir t default-dir))
    (setq rb/front-prev-basedir (expand-file-name dir))
    rb/front-prev-basedir
  ))

(defun rb/front-edit-get-top-dir (info)
  (let ((top-dir (plist-get info :top-dir))
	dir)
    (setq dir (read-file-name "top-dir?: " "/" top-dir t top-dir))
    (when (not (equal dir top-dir))
      (plist-put info :top-dir dir)
      (plist-put info :base-dir dir)
      (setq rb/front-prev-basedir (expand-file-name dir))
      )
  ))



(defun rb/front-edit-add-file-to ()
  (interactive)
  (setq rb/front-edit-during-to-add-file t)
  (let* ((info rb/front-edit-info)
	 (tool (plist-get info :tool))
	 (work-dir (rb/front-get-work-dir (plist-get info :id)))
	 (id (plist-get info :id))
	 (top-dir (plist-get rb/front-edit-info :top-dir))
	 )

    (when id
      (rb/front-setting-set-work-info
       id (list :tool tool :id id :base-dir work-dir :top-dir top-dir)))
    
    (setq rb/front-prev-basedir work-dir)
    (rb/front-tool-get-setup-to-add-files tool work-dir)
  ))


(defun rb/front-edit-get-text-region (pattern)
  "現在の位置から pattern のある場所に移動し、
そこから <--- の文字列がある場所までの領域の文字列を取得する。"
  (re-search-forward pattern)
  (forward-line 1)
  (setq pos (point))
  (re-search-forward "^<---$")
  (beginning-of-line)
  (buffer-substring-no-properties pos (1- (point)))
  )

(defun rb/front-edit-convert-rrq-to-info (rrq)
  (let ((work-info (rb/front-setting-get-work-info (plist-get rrq :id))))
    (list :tool (rb/front-get-repository-tool-at-request rrq)
	  :top-dir (or (plist-get work-info :top-dir) "")
	  :base-dir (or (plist-get work-info :base-dir) "")
	  :id (plist-get rrq :id)
	  :state (if (rb/front-private-p rrq) "private" "public")
	  :title (plist-get rrq :summary)
	  :description (plist-get rrq :description)
	  :test (plist-get rrq :testing_done)
	  :file-list (rb/front-get-file-list rrq)))
  )

(defun rb/front-edit-get-text-line (pattern)
  (re-search-forward pattern)
  (buffer-substring-no-properties (point) (point-at-eol))
  )

(defun rb/front-edit-apply-info-2-list (info &optional key-list)
  (dolist (key (or key-list rb/front-edit-info-keys))
    (when (plist-get info key)
      (plist-put rb/front-edit-info key (plist-get info key)))))
    

(defun rb/front-edit-apply-info ()
  (rb/front-edit-apply-info-2-list (rb/front-edit-get-info)
				   '(:title :description :test))
  )

(defun rb/front-edit-get-info ()
  (let (tool top-dir base-dir id state title description test
	     file-list modified-file-list-p)
    (save-excursion
      (beginning-of-buffer)
      (setq tool (rb/front-edit-get-text-line "tool: "))
      (setq top-dir (rb/front-edit-get-text-line "top-dir: "))
      (setq base-dir (rb/front-edit-get-text-line "base-dir: "))
      (setq id (string-to-number (rb/front-edit-get-text-line "id: ")))
      (when (eq id 0)
	(setq id nil))
      (re-search-forward "state: ")
      (setq state (buffer-substring-no-properties (point) (point-at-eol)))
      (re-search-forward "title.*: ")
      (setq title (buffer-substring-no-properties (point) (point-at-eol)))
      
      (forward-line 1)
      (setq description (rb/front-edit-get-text-region "description:"))

      (forward-line 1)
      (setq test (rb/front-edit-get-text-region "test:"))
      
      (forward-line 1)
      (re-search-forward "files: --->")
      (forward-line 2)
      (beginning-of-line)
      (while (looking-at "^ \\[\\([ X]\\)\\]: \\(.*\\)$")
	(let ((mark (match-string 1))
	      (path (match-string 2)))
	  (if (equal mark "X")
	      (add-to-list 'file-list path t)
	    (setq modified-file-list-p t))
	  (forward-line 1)
	  ))
      )
    (list :tool tool :top-dir top-dir :base-dir base-dir
	  :id id :state state :title title :description description :test test
	  :file-list file-list :modified-file-list-p modified-file-list-p)
    ))

(defun rb/front-edit-toggle-marking-file ()
  (interactive)
  (let ((inhibit-read-only t)
	(pos (point))
	(modified-p (buffer-modified-p))
	(unmarked-list (plist-get rb/front-edit-info :unmark-files))
	)
    (save-excursion
      (beginning-of-line)
      (when (looking-at "^ \\[\\([ X]\\)\\]: \\(.*\\)$")
	(let ((mark (match-string-no-properties 1))
	      (path (match-string-no-properties 2)))
	  (delete-region (point-at-bol) (1+ (point-at-eol)))
	  (rb/front-edit-insert-file-info path (not (equal mark "X")))
	  (if (equal mark "X")
	      ;; アンマークリストに追加
	      (add-to-list 'unmarked-list path t)
	    ;; アンマークリストから除外
	    (setq unmarked-list (delete path unmarked-list))
	    )
	  ))
      )
    (plist-put rb/front-edit-info :unmark-files unmarked-list)
    (set-buffer-modified-p modified-p)
    (goto-char pos)
    ))

(defun rb/front-edit-get-current-review ()
  (let (limit-pos review-id comment-id)
    (save-excursion
      (beginning-of-buffer)
      (re-search-forward "^review comments: --->$" nil t)
      (setq limit-pos (point)))
    (save-excursion
      (end-of-line)
      (if (not (re-search-backward "^([0-9]+:[0-9]+:" limit-pos t ))
	  (message "illegal position")
	(looking-at "(\\([0-9]+\\):\\([0-9]+\\):\\([a-z]+\\))")
	(setq review-id (list :review-id (string-to-number (match-string 1))
			      :comment-id (string-to-number (match-string 2))
			      :open-p (equal (match-string 3) "open")))))
    review-id
  ))

(defun rb/front-edit-set-reply ()
  (interactive)
  (let* ((info rb/front-edit-info)
	 (id (plist-get info :id))
	 (review-info (rb/front-edit-get-current-review))
	 (reply-hash (rb/front-get-reply-hash (plist-get info :id)))
	 (reply-list (gethash (plist-get review-info :comment-id) reply-hash))
	 (last-reply (nth (1- (length reply-list)) reply-list))
	 )
    (rb/front-text-view
     (lambda ()
       (rb/front-insert-readOnlyText
	(plist-get (rb/front-get-review-comment
		    id
		    (plist-get review-info :review-id)
		    (plist-get review-info :comment-id))
		   :text)
	rb/front-face-item
	"\n"
	))
     (lambda ()
       (when last-reply
	 (insert (plist-get last-reply :text)))
       )
     (lambda (text)
       (rb/front-add-reply-comment id
				   (plist-get review-info :review-id)
				   (plist-get review-info :comment-id)
				   text)))
    )
  )

(defun rb/front-edit-toggle-display-review ()
  (interactive)
  (setq rb/front-display-comment-p (not rb/front-display-comment-p))
  ;;(rb/front-edit-redraw (rb/front-edit-get-info)))
  (rb/front-edit-redraw))

(defun rb/front-edit-toggle-publish-mode ()
  (interactive)
  (setq rb/front-submit-and-publish-p (not rb/front-submit-and-publish-p))
;;  (rb/front-edit-redraw (rb/front-edit-get-info)))
  (rb/front-edit-redraw))

(defun rb/front-edit-discard-draft ()
  (interactive)
  (let ((info rb/front-edit-info))
    (rb/front-draft-discard (plist-get info :id))
    (kill-buffer rb/front-edit-buf)
    (rb/front-list-redraw-view)
  ))

(defun rb/front-edit-publish ()
  (interactive)
  (let ((rb/front-submit-and-publish-p t))
    ;; submit の publish が動くように modified-p しておく
    (set-buffer-modified-p t)
    (rb/front-edit-submit)
    ))

(defun rb/front-edit-save-to-draft ()
  (interactive)
  ;; publish せずに submit する。
  (let ((rb/front-submit-and-publish-p nil))
    (rb/front-edit-submit t))
  (rb/front-edit-redraw)
  )

(defun rb/front-edit-submit (&optional no-interactive)
  (interactive)
  (rb/front-edit-apply-info)
  (let* ((info rb/front-edit-info)
	 (id (plist-get info :id)))
    (when (and id (plist-get info :modified-file-list-p))
      ;; 既存 rrq の submit で、ファイルリストが編集されている場合
      ;; diff を更新する
      (when (or no-interactive
		(y-or-n-p "upload diff?: "))
	(rb/front-edit-update-diff-for info rb/front-submit-and-publish-p)))
    (when (buffer-modified-p)
      (when (or no-interactive
		(y-or-n-p "submit?: "))
	(rb/front-edit-submit-direct rb/front-submit-and-publish-p)
	(when (not no-interactive)
	  (kill-buffer)
	  (rb/front-list-redraw-view)
	  )))
    ))

(defun rb/front-edit-submit-direct (publish-p)
  "現在の edit buffer の 情報を submit する。

publish-p が nil 以外の場合 publish する。
"
  (rb/front-edit-apply-info)
  (let ((info rb/front-edit-info)
	id)
    (setq id (rb/front-register-rrq (plist-get info :id)
				    (plist-get info :title)
				    (plist-get info :description)
				    (plist-get info :test)))
    (when id
      (plist-put rb/front-edit-info :id id)
      (if (equal (plist-get info :state) "new")
	  ;; new の場合、file-list も登録する。
	  (rb/front-exec-rbt id default-directory
			     (rb/front-normalize-file-list id
							   (plist-get info :tool)
							   default-directory
							   (plist-get info :file-list)
							   (plist-get info :unmark-files)
							   )
			     (lambda (success)
			       (when (and success publish-p)
				 (rb/front-draft-publish id))))
	;; 登録済みの場合、 publish する
	(when publish-p
	  (rb/front-draft-publish id)))
      ))
  (set-buffer-modified-p nil)
  )

(defun rb/front-edit-update-diff ()
  (interactive)
  (rb/front-edit-update-diff-for rb/front-edit-info
				 rb/front-submit-and-publish-p)
  )


(defun rb/front-edit-update-diff-for (info publish-p)
  (let* ((id (plist-get info :id))
	 (file-list (plist-get info :file-list))
	 (top-dir (plist-get info :top-dir))
	 repo-path-info)
    (if (not id)
	(message "This review request is not submit yet.")
      (when (equal top-dir "")
	(setq top-dir (rb/front-edit-get-top-dir info)))
      (rb/front-exec-rbt id top-dir
			 (rb/front-normalize-file-list id
						       (plist-get info :tool)
						       top-dir file-list
						       (plist-get info :unmark-files))
			 (lambda (success)
			   (when (and success
				      publish-p
				      (equal (plist-get info :state) "public"))
			     ;; diff を登録すると draft になるので public に戻す。
			     (rb/front-draft-publish id))
			   ))
      
      )))




(defvar rb/front-text-mode-hook nil
  "Hooks called when rb/front-edit mode fires up.")

(define-derived-mode rb/front-text-mode
  text-mode "rb/front-text"
  "Major mode for Review Board.
          \\{rb/front-text-mode-map}"
  )

(define-key rb/front-text-mode-map (kbd "C-c C-c") 'rb/front-text-submit)

(defun rb/front-text-view (header-draw body-draw submit)
  "file-info のコミットメッセージを編集するバッファを表示。
"
  (let* ((buf (get-buffer-create rb/front-text-buf))
	 pos)
    (with-current-buffer buf
      (rb/front-text-mode)

      (set (make-local-variable 'rb/front-text-info)
	   (list :submit submit))

      
      (let ((inhibit-read-only t))
	(erase-buffer)
	(funcall header-draw)
	(rb/front-insert-readOnlyText "----" rb/front-face-message "\n")
	
	(setq pos (point))
	(plist-put rb/front-text-info :start-pos pos)

	(funcall body-draw)
	)

      )
    
    (rb/front-switch-to-buffer-other-window buf)
    (goto-char pos)
    ))

(defun rb/front-text-submit ()
  (interactive)
  (when (y-or-n-p "submit?: ")
    (let ((pos (plist-get rb/front-text-info :start-pos))
	  (buf (current-buffer)))
      (funcall (plist-get rb/front-text-info :submit)
	       (buffer-substring-no-properties pos (point-max)))
      (kill-buffer buf)
  )))


(defun rb/front-commit-view (edit-info)
  "file-info のコミットメッセージを編集するバッファを表示。
"
  (let* ((id (plist-get edit-info :id))
	 (tool (plist-get edit-info :tool))
	 work-dir file-list conv-info)
    (setq work-dir (rb/front-get-work-dir id))
    (setq conv-info (rb/front-tool-get-work-info tool work-dir))

    (setq file-list (rb/front-normalize-file-list id
						  tool
						  (plist-get conv-info :root-path)
						  (plist-get edit-info :file-list)
						  (plist-get edit-info :unmark-files)
						  ))
    (rb/front-text-view
     (lambda ()
       (rb/front-set-default-dir (plist-get conv-info :root-path))
       (rb/front-insert-key-bind-doc
	'(("commit: " "(C-c C-c)" )
	  (", cancel: " "(kill-buffer)")
	  ("\n----\n")))

       (rb/front-insert-readOnlyText-multi
	`(("work-directory: " ,rb/front-face-item)
	  (,default-directory ,rb/front-face-emphasis)
	  ("\n----\n" rb/front-face-message)))
       
       (dolist (file file-list)
	 (rb/front-insert-readOnlyText (concat file "\n") rb/front-face-warning)
	 )
       
       (rb/front-insert-readOnlyText "----\n" rb/front-face-message)
       (plist-put rb/front-text-info :edit-info edit-info)
       )
     (lambda ()
       (when id
	 (insert (format "%sr/%d/\n" rb/front-rb-url id)))
       (insert (plist-get edit-info :description))
       )
     'rb/front-commit-submit
     )
  ))



(defun rb/front-commit-submit (message)
  (interactive)
  (let ((pos (plist-get rb/front-text-info :start-pos))
	(edit-info (plist-get rb/front-text-info :edit-info)))
    (rb/front-tool-commit-files
     (plist-get edit-info :tool)
     message
     (plist-get edit-info :top-dir)
     (rb/front-normalize-file-list (plist-get edit-info :id)
				   (plist-get edit-info :tool)
				   (plist-get edit-info :top-dir)
				   (plist-get edit-info :file-list))
     )
    ))

(provide 'rbfront-mode)
