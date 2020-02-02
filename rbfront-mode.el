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

(defun rb/front-list-get-review-txt (review)
  (if review
      (format "%6d: %s: %s: %s"
	      (plist-get review :id)
	      (propertize
	       (if (rb/front-public-p review)
		   "pub"
		 "pri")
	       'face
	       (if (rb/front-public-p review)
		   font-lock-keyword-face
		 font-lock-warning-face)
	       )
	      (propertize
	       (plist-get review :last_updated)
	       'face font-lock-comment-face)
	      (plist-get review :summary))
    ""
    ))

(defvar rb/front-list-review-list nil)

(defun rb/front-list-update-review-list ()
  (setq rb/front-list-review-list (rb/front-get-my-reported-review-list)))

(defun rb/front-list-redraw-view ()
  (interactive)
  (let ((inhibit-read-only t)
	(buf (get-buffer-create rb/front-list-buf))
	info)
    (rb/front-list-update-review-list)
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)

      (rb/front-insert-readOnlyText
       "reload: (g), edit: (RET), upload-file: (u), publish: (p), close: (c), discard: (d)\n"
       'font-lock-doc-face)
      (dolist (review rb/front-list-review-list)
	(insert (rb/front-list-get-review-txt review))
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


(defun rb/front-list-view ()
  (interactive)
  (rb/front-list-redraw-view)
  (switch-to-buffer-other-window (get-buffer-create rb/front-list-buf))
)

(defun rb/front-list-current-review ()
  (nth (- (line-number-at-pos) 2) rb/front-list-review-list)
  )

(defun rb/front-list-edit-current ()
  (interactive)
  (let ((review (rb/front-list-current-review)))
    (rb/front-edit-view (plist-get review :id))
    ))
(defun rb/front-list-upload-current ()
  (interactive)
  (let ((review (rb/front-list-current-review)))
    (rb/front-edit-update-diff-for (rb/front-edit-convert-review-to-info review))
    ))
(defun rb/front-list-publish-current ()
  (interactive)
  (when (y-or-n-p "publish?: ")
    (let ((review (rb/front-list-current-review)))
      (rb/front-draft-publish (rb/front-access review :id))
      ))
  )
(defun rb/front-list-close-current ()
  (interactive)
  (when (y-or-n-p "close?: ")
    (let ((review (rb/front-list-current-review)))
      (rb/front-close-review review)
      (sleep-for 1)
      (rb/front-list-redraw-view)
      ))
  )
(defun rb/front-list-discard-current ()
  (interactive)
  (when (y-or-n-p "discard?: ")
    (let ((review (rb/front-list-current-review)))
      (rb/front-discard-review (rb/front-access review :id))
      (sleep-for 1)
      (rb/front-list-redraw-view)
      ))
  )

  


(defun rb/front-svn-info (base-dir)
  (let (root-path root-url)
    (with-temp-buffer
      (call-process "svn" nil (current-buffer) nil "info" base-dir)
      (beginning-of-buffer)
      (re-search-forward "Working Copy Root Path: ")
      (setq root-path (buffer-substring-no-properties (point) (point-at-eol)))
      )
    (with-temp-buffer
      (call-process "svn" nil (current-buffer) nil "info" root-path)
      (beginning-of-buffer)
      (re-search-forward "Relative URL: ^/")
      (setq root-url (buffer-substring-no-properties (point) (point-at-eol)))
      )
    (list :root-path root-path :root-url root-url)
    )
  )

(defun rb/front-edit-insert-file-info (path mark-p)
  (if mark-p
      (insert " [X]: ")
    (insert " [ ]: "))
  (rb/front-insert-readOnlyText path 'font-lock-warning-face "\n")
  )


(defvar rb/front-edit-during-to-add-file nil)

(defun rb/front-edit-add-file (file-info)
  (if (not rb/front-edit-during-to-add-file)
      ;; rb/front-edit-during-to-add-file が nil の場合、新規投稿
      (rb/front-edit-view nil file-info))
  (setq rb/front-edit-during-to-add-file nil)
  (with-current-buffer (get-buffer-create rb/front-edit-buf)
    (let ((info (rb/front-edit-get-info)))
      (plist-put info :file-list
		 (append (plist-get info :file-list)
			 (plist-get file-info :file-list)))
      (plist-put info :base-dir
		 (plist-get file-info :base-dir))
      (rb/front-edit-redraw info)
      (when (not (eq (plist-get info :state) "new"))
	(rb/front-edit-update-diff-for info))
      ))
  (switch-to-buffer (get-buffer-create rb/front-edit-buf))
  )

(defun rb/front-edit-redraw (info)
  (with-current-buffer (get-buffer-create rb/front-edit-buf)
    (let ((inhibit-read-only t)
	  (id (plist-get info :id))
	  (file-list (plist-get info :file-list))
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
      (insert (or (plist-get info :title) "new title"))
      (insert "\n")
      (rb/front-insert-readOnlyText "description: --->"
				    'font-lock-warning-face "\n")
      (insert (or (plist-get info :description) "input description"))
      (insert "\n")
      (rb/front-insert-readOnlyText "<---\n"
				    'font-lock-warning-face)
      (rb/front-insert-readOnlyText "test: --->"
				    'font-lock-warning-face "\n")
      (insert (or (plist-get info :test) "input test"))
      (insert "\n")
      (rb/front-insert-readOnlyText "<---\n"
				    'font-lock-warning-face)
      (rb/front-insert-readOnlyText "files: --->"
				    'font-lock-warning-face "\n")
      (rb/front-insert-readOnlyText "toggle mark: (C-c C-SPC), add file: (C-c C-a)"
				    'font-lock-doc-face "\n")
      
      ;; (when id
      ;; 	(setq file-list (rb/front-draft-get-file-list-at-review id)))
      (dolist (file file-list)
	(rb/front-edit-insert-file-info file t))
      (rb/front-insert-readOnlyText "<---\n"
				    'font-lock-warning-face)
      (set-buffer-modified-p modified-p)
      )
    ))


(defun rb/front-edit-view (id &optional file-info)
  "指定 id の review request を編集する。

新規の場合は id に nil を指定し、 file-info を指定しなければならない。
file-info は次の値。
\(:tool TOOL :base-dir BASE-DIR :file-list FILE-LIST \)
TOOL は、ファイルのバージョン管理ツール。Subversion など。
reviewboard で管理している文字列を指定する。
request request に登録するファイルリストを FILE-LIST に指定する。
BASE-DIR は、 FILE-LIST の work directory。
"
  (let* ((buf (get-buffer-create rb/front-edit-buf))
	 info tool base-dir file-list review)
    (with-current-buffer buf
      (if id
	  (progn
	    (setq review (rb/front-get-request id))
	    (setq info (rb/front-edit-convert-review-to-info review))
	    (setq tool (rb/front-get-repository-tool-at-request review)))
	;; 規新登録の場合
	(setq tool (plist-get file-info :tool)
	      info `(:tool ,tool :state "new")
	      base-dir (expand-file-name (plist-get file-info :base-dir))
	      file-list (plist-get file-info :file-list))
	(when (or (not base-dir) (not file-list))
	  (error "base-dir or file-list is nil."))
	(setq file-list (rb/front-convert-file-list tool base-dir file-list))
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
	;; id を指定している場合、既存 review の変数なので、変更なしで初期化
	(set-buffer-modified-p nil)
      ;; id を指定していない場合、新規 review 投稿なので、変更ありで初期化
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
	 (base-dir (expand-file-name
		    (read-file-name "work-dir?: " "/"
				    rb/front-prev-basedir t rb/front-prev-basedir)))
	 )
    (setq rb/front-prev-basedir base-dir)
    (setq rb/front-edit-during-to-add-file t)
    (cond ((equal tool "Subversion")
	   (svn-status base-dir))
	  )
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

(defun rb/front-edit-convert-review-to-info (review)
  (list :tool (rb/front-get-repository-tool-at-request review)
	:base-dir ""
	:id (plist-get review :id)
	:state (if (rb/front-private-p review) "private" "public")
	:title (plist-get review :summary)
	:description (plist-get review :description)
	:test (plist-get review :testing_done)
	:file-list (rb/front-get-file-list review))
  )

(defun rb/front-edit-get-text-line (pattern)
  (re-search-forward pattern)
  (buffer-substring-no-properties (point) (point-at-eol))
  )

(defun rb/front-edit-get-info ()
  (let (tool base-dir id state title description test file-list modified-file-list-p)
  (save-excursion
    (beginning-of-buffer)
    (setq tool (rb/front-edit-get-text-line "tool: "))
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
  (list :tool tool :base-dir base-dir
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
      ;; 既存 review の submit で、ファイルリストが編集されている場合
      ;; diff を更新する
      (when (y-or-n-p "upload diff?: ")
	(rb/front-edit-update-diff-for info)))
    (when (buffer-modified-p)
      (when (y-or-n-p "submit?: ")
	;; 新規 review の場合、登録と同時に publish される。
	(rb/front-edit-submit-direct)
	))
    (kill-buffer)
    (rb/front-list-redraw-view)
    (message "ok")
    ))

(defun rb/front-edit-check-submit ()
  (when (buffer-modified-p)
    (when (y-or-n-p "buffer is modified. submit?: " )
      (rb/front-edit-submit-direct)
    )))


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
      
      (rb/front-edit-review (plist-get info :id)
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
  (rb/front-edit-add-file `(:tool "Subversion"
				  :base-dir ,default-directory
				  :file-list ,(svn-status-marked-file-names))))


(provide 'rbfront-mode)
