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

(defvar rb/front-edit-info-default
  '(:title "new title"
    :description "input description"
    :test "input test")
  "新規 review request のデフォルト情報")

(defvar rb/front-list-buf "*rb/front/list*")
(defvar rb/front-edit-buf "*rb/front/edit*")
(defvar rb/front-prev-basedir nil)

(defun rb/front-insert-readOnlyText (text face &optional annex)
  (let ((start (point)) (inhibit-read-only t))
    (if face
	(insert (propertize text 'face face))
      (insert text))
    (when annex
	(insert annex))
    (put-text-property (if (< 1 start)
			   (1- start)
			 start)
		       (1- (point)) 'read-only t)))

(defun rb/front-list-get-rrq-txt (rrq)
  (if rrq
      (format "%6d: %s: %s: %s"
	      (plist-get rrq :id)
	      (propertize
	       (if (rb/front-public-p rrq)
		   "pub"
		 "pri")
	       'face
	       (if (rb/front-public-p rrq)
		   font-lock-keyword-face
		 font-lock-warning-face)
	       )
	      (propertize
	       (plist-get rrq :last_updated)
	       'face font-lock-comment-face)
	      (plist-get rrq :summary))
    ""
    ))

(defvar rb/front-list-rrq-list nil)

(defun rb/front-list-update-rrq-list ()
  (setq rb/front-list-rrq-list (rb/front-get-my-reported-rrq-list)))

(defun rb/front-list-redraw-view ()
  (interactive)
  (let ((inhibit-read-only t)
	(buf (get-buffer-create rb/front-list-buf))
	info)
    (rb/front-list-update-rrq-list)
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)

      (rb/front-insert-readOnlyText
       "reload: (g), edit: (RET), upload-file: (u), publish: (p), close: (c), discard: (d)\n"
       'font-lock-doc-face)
      (dolist (rrq rb/front-list-rrq-list)
	(insert (rb/front-list-get-rrq-txt rrq))
	(insert "\n")
	)

      (setq buffer-read-only t)
      (local-set-key (kbd "c") 'rb/front-list-close-current)
      (local-set-key (kbd "d") 'rb/front-list-discard-current)
      (local-set-key (kbd "p") 'rb/front-list-publish-current)
      (local-set-key (kbd "u") 'rb/front-list-upload-current)
      (local-set-key (kbd "g") 'rb/front-list-redraw-view)
      (local-set-key (kbd "RET") 'rb/front-list-edit-current)
      )
    ))


(defun rb/front-switch-to-buffer-other-window (buf)
  "buf を別 window に表示する。

buf が既にいずれかの window に表示されている場合は、その window に切り替える。"
  (if (not (get-buffer-window buf))
      (switch-to-buffer-other-window buf)
    (select-window (get-buffer-window buf))
    ))


(defun rb/front-list ()
  (interactive)
  (rb/front-list-redraw-view)
  (rb/front-switch-to-buffer-other-window (get-buffer-create rb/front-list-buf))
)

(defun rb/front-list-current-rrq ()
  (nth (- (line-number-at-pos) 2) rb/front-list-rrq-list)
  )

(defun rb/front-list-edit-current ()
  (interactive)
  (let ((rrq (rb/front-list-current-rrq)))
    (rb/front-edit-view (plist-get rrq :id))
    ))
(defun rb/front-list-upload-current ()
  (interactive)
  (let ((rrq (rb/front-list-current-rrq)))
    (rb/front-edit-update-diff-for (rb/front-edit-convert-rrq-to-info rrq))
    ))
(defun rb/front-list-publish-current ()
  (interactive)
  (when (y-or-n-p "publish?: ")
    (let ((rrq (rb/front-list-current-rrq)))
      (rb/front-draft-publish (rb/front-access rrq :id))
      ))
  )
(defun rb/front-list-close-current ()
  (interactive)
  (when (y-or-n-p "close?: ")
    (let ((rrq (rb/front-list-current-rrq)))
      (rb/front-close-rrq rrq)
      (sleep-for 1)
      (rb/front-list-redraw-view)
      ))
  )
(defun rb/front-list-discard-current ()
  (interactive)
  (when (y-or-n-p "discard?: ")
    (let ((rrq (rb/front-list-current-rrq)))
      (rb/front-discard-rrq (rb/front-access rrq :id))
      (sleep-for 1)
      (rb/front-list-redraw-view)
      ))
  )

(defun rb/front-file-info-normalize (file-info)
    "file-info を正規化する。

- file-list を reviewboard に登録するパス形式に変換。
- top-dir を work ディレクトリのルートに変換。
 "
  (let* ((tool (rb/front-file-info-get-tool file-info))
	 (base-dir (rb/front-file-info-get-base-dir file-info))
	 (file-list (rb/front-file-info-get-file-list file-info))
	 rb-path-list conv-info work-root)
      (cond ((equal tool "Subversion")
	     (setq conv-info (rb/front-svn-info base-dir))
	     )
	    (t
	     (error (concat "not support -- " tool)))
	    )
      (setq work-root (plist-get conv-info :root-path))
      (setq rb-path-list
	    (mapcar (lambda (X)
		      (rb/front-path-join
		       (plist-get conv-info :root-url)
		       (file-relative-name (expand-file-name X base-dir)
					   work-root)
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
   'font-lock-warning-face "\n")
  )


(defvar rb/front-edit-during-to-add-file nil)
    
(defun rb/front-edit-add-file (file-info)
  (if (not rb/front-edit-during-to-add-file)
      ;; rb/front-edit-during-to-add-file が nil の場合、新規投稿
      (rb/front-edit-view nil file-info)
    (setq rb/front-edit-during-to-add-file nil)
    (with-current-buffer (get-buffer-create rb/front-edit-buf)
      (let ((info (rb/front-edit-get-info)))
	(plist-put info :file-list
		   (append (plist-get info :file-list)
			   (rb/front-file-info-get-file-list file-info)))
	(plist-put info :top-dir
		   (rb/front-file-info-get-top-dir file-info))
	(plist-put info :base-dir
		   (rb/front-file-info-get-base-dir file-info))
	(rb/front-edit-redraw info)
	(when (not (eq (plist-get info :state) "new"))
	  (rb/front-edit-update-diff-for info))
	))
    (switch-to-buffer (get-buffer-create rb/front-edit-buf))
    ))

(defun rb/front-edit-redraw (info)
  (with-current-buffer (get-buffer-create rb/front-edit-buf)
    (let ((inhibit-read-only t)
	  (id (plist-get info :id))
	  (modified-p (buffer-modified-p)))
      (erase-buffer)
      (rb/front-insert-readOnlyText
       (concat "submit: (C-c C-c), "
	       "update diff: (C-c C-u), "
	       ;;"publish: (C-c C-p), "
	       "cancel: (kill-buffer)\n")
       'font-lock-doc-face)
      (rb/front-insert-readOnlyText (format "tool: %s\n" (plist-get info :tool))
				    'font-lock-doc-face)
      (rb/front-insert-readOnlyText (format "top-dir: %s\n"
					    (or (plist-get info :top-dir)
						""))
				    'font-lock-doc-face)
      (rb/front-insert-readOnlyText (format "base-dir: %s\n"
					    (or (plist-get info :base-dir)
						""))
				    'font-lock-doc-face)
      (rb/front-insert-readOnlyText (format "id: %s\n" (or id "--"))
				    'font-lock-doc-face)
      (rb/front-insert-readOnlyText (format "state: %s\n"
					    (plist-get info :state))
				    'font-lock-doc-face)
      (rb/front-insert-readOnlyText "title (only 1 line):"
				    'font-lock-warning-face " ")
      (insert (or (plist-get info :title)
		  (plist-get rb/front-edit-info-default :title)))
      (insert "\n")
      (rb/front-insert-readOnlyText "description: --->"
				    'font-lock-warning-face "\n")
      (insert (or (plist-get info :description)
		  (plist-get rb/front-edit-info-default :description)))
      (insert "\n")
      (rb/front-insert-readOnlyText "<---\n"
				    'font-lock-warning-face)
      (rb/front-insert-readOnlyText "test: --->"
				    'font-lock-warning-face "\n")
      (insert (or (plist-get info :test)
		  (plist-get rb/front-edit-info-default :test)))
      (insert "\n")
      (rb/front-insert-readOnlyText "<---\n"
				    'font-lock-warning-face)
      (rb/front-insert-readOnlyText "files: --->"
				    'font-lock-warning-face "\n")
      (rb/front-insert-readOnlyText "toggle mark: (C-c C-SPC), add file: (C-c C-a)"
				    'font-lock-doc-face "\n")
      
      (dolist (file (plist-get info :file-list))
	(rb/front-edit-insert-file-info file t))
      (rb/front-insert-readOnlyText "<---\n"
				    'font-lock-warning-face)


      (when id
	(rb/front-insert-readOnlyText "review comments: --->"
				      'font-lock-warning-face "\n")
	(dolist (review (rb/front-get-review-list id))
	  (rb/front-insert-readOnlyText
	   (format "%s:%s\n"
		   (rb/front-repstr 
		    (rb/front-access review :links :filediff :title)
		    " ([0-9]+).*" "" t)
		   (plist-get review :first_line))
	   'font-lock-comment-face)
	  (rb/front-insert-readOnlyText (plist-get review :text)
					'font-lock-keyword-face "\n")
	  
	  )
	(rb/front-insert-readOnlyText "<---"
				      'font-lock-warning-face "\n")
	)
      
      (set-buffer-modified-p modified-p)
      )
    ))


(defun rb/front-edit-view (id &optional file-info)
  "指定 id の review request を編集する。

新規の場合は id に nil を指定し、 file-info を指定しなければならない。
file-info は rb/front-file-info-create で生成する。
"
  (let* ((buf (get-buffer-create rb/front-edit-buf))
	 info tool base-dir file-list rrq)
    (with-current-buffer buf
      (if id
	  (progn
	    (setq rrq (rb/front-get-request id))
	    (setq info (rb/front-edit-convert-rrq-to-info rrq))
	    (setq tool (rb/front-get-repository-tool-at-request rrq)))
	;; 規新登録の場合
	(setq tool (rb/front-file-info-get-tool file-info)
	      base-dir (rb/front-file-info-get-base-dir file-info)
	      info (list :tool tool :state "new"
			 :top-dir (rb/front-file-info-get-top-dir file-info)
			 :base-dir base-dir
			 :file-list (rb/front-file-info-get-file-list file-info))
	      )
	(setq rb/front-prev-basedir base-dir)
	)
      (when base-dir
	(setq default-directory (expand-file-name base-dir)))

      (rb/front-edit-redraw info)
      )
    (switch-to-buffer buf)
    (beginning-of-buffer)
    (perform-replace "\r" "" nil nil nil)  
    (beginning-of-buffer)
    (re-search-forward "title.*: ")
    
    (if id
	;; id を指定している場合、既存 rrq の変数なので、変更なしで初期化
	(set-buffer-modified-p nil)
      ;; id を指定していない場合、新規 rrq 投稿なので、変更ありで初期化
      (set-buffer-modified-p t))

    (local-set-key (kbd "C-c C-a") 'rb/front-edit-add-file-to)
    (local-set-key (kbd "C-c C-SPC") 'rb/front-edit-toggle-marking-file)
    (local-set-key (kbd "C-c C-c") 'rb/front-edit-submit)
    (local-set-key (kbd "C-c C-u") 'rb/front-edit-update-diff)
    )
  )



(defun rb/front-edit-add-file-to ()
  (interactive)
  (rb/front-edit-check-submit)
  (let* ((info (rb/front-edit-get-info))
	 (tool (plist-get info :tool))
	 (work-dir (expand-file-name
		    (read-file-name "work-dir?: " "/"
				    rb/front-prev-basedir t rb/front-prev-basedir)))
	 )
    (setq rb/front-prev-basedir work-dir)
    (setq rb/front-edit-during-to-add-file t)
    (cond ((equal tool "Subversion")
	   (when (get-buffer svn-status-buffer-name)
	     (rb/front-switch-to-buffer-other-window
	      (get-buffer svn-status-buffer-name)))
	   (svn-status work-dir)))
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
  (list :tool (rb/front-get-repository-tool-at-request rrq)
	:top-dir ""
	:base-dir ""
	:id (plist-get rrq :id)
	:state (if (rb/front-private-p rrq) "private" "public")
	:title (plist-get rrq :summary)
	:description (plist-get rrq :description)
	:test (plist-get rrq :testing_done)
	:file-list (rb/front-get-file-list rrq))
  )

(defun rb/front-edit-get-text-line (pattern)
  (re-search-forward pattern)
  (buffer-substring-no-properties (point) (point-at-eol))
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
	(modified-p (buffer-modified-p)))
    (save-excursion
      (beginning-of-line)
      (when (looking-at "^ \\[\\([ X]\\)\\]: \\(.*\\)$")
	(let ((mark (match-string 1))
	      (path (match-string 2)))
	  (delete-region (point-at-bol) (1+ (point-at-eol)))
	  (rb/front-edit-insert-file-info path (not (equal mark "X")))
	  ))
      )
    (set-buffer-modified-p modified-p)
    (goto-char pos)
    ))

(defun rb/front-edit-submit ()
  (interactive)
  (let* ((info (rb/front-edit-get-info))
	 (id (plist-get info :id)))
    (when (and id (plist-get info :modified-file-list-p))
      ;; 既存 rrq の submit で、ファイルリストが編集されている場合
      ;; diff を更新する
      (when (y-or-n-p "upload diff?: ")
	(rb/front-edit-update-diff-for info)))
    (when (buffer-modified-p)
      (when (y-or-n-p "submit?: ")
	;; 新規 rrq の場合、登録と同時に publish される。
	(rb/front-edit-submit-direct)
	))
    (kill-buffer)
    (rb/front-list-redraw-view)
    (message "ok")
    ))

(defun rb/front-edit-check-submit ()
  (let ((info (rb/front-edit-get-info)))
    (when (and
	   (buffer-modified-p)
	   ;; new の場合 submit で diff を含めて登録するので、
	   ;; new 以外の時のみ submit する。
	   (not (equal (plist-get info :state) "new")))
      (when (y-or-n-p "buffer is modified. submit?: " )
	(rb/front-edit-submit-direct)
	))))


(defun rb/front-edit-submit-direct ()
  (let ((info (rb/front-edit-get-info)))
    (if (equal (plist-get info :state) "new")
	(rb/front-new-request default-directory
			      (rb/front-normalize-file-list (plist-get info :tool)
							    default-directory
							    (plist-get info :file-list))
			      (plist-get info :title)
			      (plist-get info :description)
			      (plist-get info :test))
      
      (rb/front-edit-rrq (plist-get info :id)
			    nil
			    (plist-get info :title)
			    (plist-get info :description)
			    (plist-get info :test))
      )
    (set-buffer-modified-p nil)
  ))

(defun rb/front-edit-update-diff ()
  (interactive)
  (rb/front-edit-check-submit)
  (rb/front-edit-update-diff-for (rb/front-edit-get-info)))
  

(defun rb/front-edit-update-diff-for (info)
  (let* ((id (plist-get info :id))
	 (file-list (plist-get info :file-list))
	 (base-dir (plist-get info :base-dir))
	 repo-path-info)
    (if (not id)
	(message "This review request is not submit yet.")
      (when (equal base-dir "")
	(setq base-dir
	      (expand-file-name (read-file-name "basedir?: " "/"
						(when rb/front-prev-basedir
						  rb/front-prev-basedir) t
					      rb/front-prev-basedir))))
      (setq rb/front-prev-basedir base-dir)
      (rb/front-exec-rbt id base-dir
			 (rb/front-normalize-file-list (plist-get info :tool)
						       base-dir file-list)
			 (lambda (success)
			   (when (and success
				      (equal (plist-get info :state) "public"))
			     ;; diff を登録すると draft になるので public に戻す。
			     (rb/front-draft-publish id))
			 ))
      
      )))



(add-hook 'svn-status-mode-hook 'rb/front-svn-hook)
(defun rb/front-svn-hook ()
  (local-set-key (kbd "j") 'rb/front-new-request-from-svn-status))
(defun rb/front-new-request-from-svn-status ()
  (interactive)
  (rb/front-edit-add-file
   (rb/front-file-info-create "Subversion"
			      default-directory
			      (svn-status-marked-file-names))))


(provide 'rbfront-mode)
