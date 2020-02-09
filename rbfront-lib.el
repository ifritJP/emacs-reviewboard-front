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

(require 'rbfront-svn)


(defvar rb/front-rb-api-token "")
(defvar rb/front-rb-url "http://localhost/rb")
(defvar rb/front-rbt "rbt")
(defvar rb/front-proxy "")
(defvar rb/front-rb-repository "")

(defvar rb/front-setting-file-path (concat user-emacs-directory "rbfront.json"))


(defvar rb/front-access-buf "*rb/front/access*")
(defvar rb/front-rbt-buf "*rb/front/rbt*")



(defvar rb/front-rbt-cache-reviewer-candidate-list nil)


(defvar rb/front-work-directory-hash (make-hash-table))

(defun rb/front-setting-load()
  (when (file-exists-p rb/front-setting-file-path)
    (with-temp-buffer
      (insert-file-contents rb/front-setting-file-path)
      (let ((json-object-type 'plist)
	    (json-array-type 'list)
	    obj)
	(setq obj (json-read-from-string
		   (buffer-substring-no-properties (point-min) (point-max))))
	(dolist (work-info (plist-get obj :work-directory))
	  (puthash (plist-get work-info :id) work-info rb/front-work-directory-hash)
	  )
	)
      ))
    )
(rb/front-setting-load)

(defun rb/front-setting-save ()
  (with-temp-file rb/front-setting-file-path
    (let ((json-object-type 'plist)
	  (json-array-type 'vector)
	  obj
	  )
      (setq obj (list :work-directory
		      (vconcat (hash-table-values rb/front-work-directory-hash))))
      (insert (json-encode obj))
      )))

(defun rb/front-setting-get-work-info (id)
  (gethash id rb/front-work-directory-hash))

(defun rb/front-setting-set-work-info (id work-info)
  (let ((prev (rb/front-setting-get-work-info id)))
    (if (and (equal (plist-get work-info :id) (plist-get prev :id))
	     (equal (plist-get work-info :tool) (plist-get prev :tool))
	     (equal (plist-get work-info :top-dir) (plist-get prev :top-dir))
	     (equal (plist-get work-info :base-dir) (plist-get prev :base-dir)))
	;; hash に登録済みの場合は、何もしない
	nil
      ;; hash に登録してあるものと異なる場合は、登録
      (puthash id work-info rb/front-work-directory-hash)
      (rb/front-setting-save)
      )))




(defun rb/front-get-send-data (alist)
  "
alist で指定したデータを MIME に変換する。
alist は (名前 値) のリスト。

変換した MIME の content-type と body のリスト (content-type body) を返す。
"
  (let ((mm-coding-system-priorities '(utf-8))
	(mml-content-disposition-parameters '(name))
	text mime)
    (setq text (concat "<#multipart type=form-data>\n"
		       (mapconcat (lambda (X)
				    (format "<#part disposition=form-data name=%s>%s<#/part>\n"
					    (car X) (cadr X)))
				  (delq nil alist) "")
		       "<#/multipart>"))
    (with-temp-buffer
      (insert text)
      (setq mime (mml-generate-mime)))
    (with-temp-buffer
      (insert mime)
      ;; 末尾の改行を削除
      (while (eq (char-before (point-max)) ?\n)
	(end-of-buffer)
	(delete-char -1))
      (beginning-of-buffer)
      (end-of-line)
      (setq content-type (buffer-substring 1 (point)))
      (forward-line 2)
      (beginning-of-line)
      (setq body (buffer-substring (point) (point-max)))
      (list content-type body)
      )
    ))

(defun rb/front-set-default-dir (dir)
  (setq default-directory (file-name-as-directory (expand-file-name dir))))
			  
			  

(defun rb/front-exec-rbt-sentinel (process event callback)
  (let ((status (process-status process)))
    (when (or (eq status 'exit)
	      (eq status 'signal))
      (with-current-buffer (get-buffer-create rb/front-rbt-buf)
	(insert (propertize "\n\nend!" 'face rb/front-face-warning)))
      (funcall callback (eq (process-exit-status process) 0)))))

(defun rb/front-call-process (dir name out-buf command &rest arg-list)
  (let (process)
    (setq arg-list
	  (apply 'append (mapcar (lambda (X)
				   (if (listp X)
				       X
				     (list X)))
				 arg-list)))
    (with-current-buffer out-buf
      (erase-buffer)
      (insert "processing...\n")
      (rb/front-set-default-dir dir)
      (insert (format "dir: %s\n" default-directory))
      (insert (format "command: %s\n" (cons command arg-list)))
      (insert "---------\n")
      (setq process (apply 'start-process name out-buf command arg-list))
      )
    (rb/front-switch-to-buffer-other-window out-buf)
    (end-of-buffer)
    (recenter -1)
    process))

(defun rb/front-exec-rbt (id basedir file-list &optional callback)
  "id の review request に file-list の diff を登録する。

basedir は、 rbt を実行するディレクトリパス。
file-list は、 basedir からの相対パスのリスト。
callback は、 rbt 終了時に実行するコールバック。
コールバックの引数には rbt が成功したかどうかを渡す。
"
  (with-current-buffer (get-buffer-create rb/front-rbt-buf)
    (erase-buffer)
    (insert "processing...\n")
    (rb/front-set-default-dir basedir)
    (let (process opt-list)
      (setq opt-list
	    (append
	     (list "post" "--repository" rb/front-rb-repository
		   "--server" rb/front-rb-url "--api-token" rb/front-rb-api-token
		   "-r" (format "%s" id))
	     (apply 'append (mapcar (lambda (X) (list "-I" X)) file-list))))
      (insert (format "dir: %s\n" basedir))
      (insert (format "command: %s\n" (cons rb/front-rbt opt-list)))
      (insert "---------\n")
      (setq process
      	    (apply 'start-process rb/front-rbt-buf rb/front-rbt-buf
      		   rb/front-rbt opt-list))
      (if (not (processp process))
	  (callback nil)
	(set-process-sentinel process
			      (lambda (proc event)
				(rb/front-exec-rbt-sentinel proc event callback)))
	(rb/front-switch-to-buffer-other-window rb/front-rbt-buf)
	(end-of-buffer)
	(recenter -1))
      )
    ))

(defun rb/front-access (obj &rest key-list)
  (dolist (key key-list)
    (setq obj (plist-get obj key))
    )
  obj
  )



(defun rb/front-access-web (url &optional method body-info &rest key-list)
  "
url の web API を実行する。

body-info は (content-type body)。 不要な場合は nil.
"
  ;; url が rb/front-rb-url から始まっている場合、 rb/front-rb-url を削除する
  (when (eq (string-match (regexp-quote rb/front-rb-url) url) 0)
    (setq url (substring url (length rb/front-rb-url))))

  (let ((buf (get-buffer-create rb/front-access-buf))
	(body (cadr body-info))
	(content-type (car body-info))
	cmd-list opt-list process input-file
	obj)
    (with-current-buffer buf
      (erase-buffer))
    (when content-type
      (setq opt-list (list "-H" content-type)))
    (when body
      ;; body がある場合, curl 入力用にテンポラリファイルを作成する
      (with-temp-buffer
	(set-buffer-file-coding-system 'no-conversion)
	(insert body)
	(beginning-of-buffer)
	(perform-replace "\n" "\r\n" nil nil nil)
	(setq input-file (make-temp-file "rbfront" nil nil))
	(write-region (point-min) (point-max) input-file)
	(setq opt-list (append opt-list (list "--data-binary"
					      (format "@%s" input-file)))))
      )

    (setq cmd-list (append (list "-s" "-X" (or method "GET")
				 "--proxy" rb/front-proxy
				 "-H" (format "Authorization: token %s" rb/front-rb-api-token)
				 (concat rb/front-rb-url url))
			   opt-list))
    (apply 'call-process "curl" nil rb/front-access-buf nil cmd-list)
    (when body
      (delete-file input-file))
    (with-current-buffer rb/front-access-buf
      (let ((json-object-type 'plist)
	    (json-array-type 'list))
	(if (equal method "DELETE")
	    (setq obj nil)
	  (setq obj (json-read-from-string
		     (rb/front-repstr (buffer-substring-no-properties (point-min)
								      (point-max))
				      "\\\\r\\\\n" "\\n"
				      ))))))
    (when key-list
      (setq obj (apply 'rb/front-access obj key-list)))
    obj
    ))

(defun rb/front-close-rrq (rrq)
  (let ((id (plist-get rrq :id)))
    (when (rb/front-private-p rrq)
      ;; public にしてからじゃないと close できないので
      (rb/front-draft-publish id t))
    (rb/front-access-web
     (format "/api/review-requests/%d/" id) "PUT"
     (rb/front-get-send-data `(("status" "submitted")))
     )
    ))

(defun rb/front-discard-rrq (id)
  (rb/front-access-web
   (format "/api/review-requests/%d/" id) "PUT"
   (rb/front-get-send-data `(("status" "discarded")))
   )
  )

(defun rb/front-update-rrq (id publish title description testing_done)
  "id のドラフト review request の情報を編集する"
  (rb/front-access-web
   (format "/api/review-requests/%d/draft/" id) "PUT"
   (rb/front-get-send-data `(("description" ,(or description ""))
			     ("summary" ,(or title "new review"))
			     ("testing_done" ,(or testing_done ""))
			     ,(when publish
				;; public でない場合、 public に切り替える
				(list "public" "true"))
			     ))
   )
  )

(defun rb/front-has-draft (id)
  "指定の id の review request が draft を持っているか？"
  (rb/front-access-web
   (format "/api/review-requests/%d/draft/" id)
   nil nil :draft))

(defun rb/front-public-p (rrq)
  (eq (plist-get rrq :public) t))

(defun rb/front-private-p (rrq)
  (not (rb/front-public-p rrq)))


(defun rb/front-conv-normalize (rrq)
  "rrq が draft の場合、
summary 等が draft 情報にしか入っていないので、draft から情報をとる。
"
  (let ((id (plist-get rrq :id)))
    (let ((obj (rb/front-access-web
		(format "/api/review-requests/%d/draft/" id)
		nil nil :draft)))
      (when obj
	;; draft の ID は draft_id として保持
	(plist-put rrq :draft_id (plist-get obj :id))
	;; summary, description, testing_done を上書き
	(plist-put rrq :summary (plist-get obj :summary))
	(plist-put rrq :description (plist-get obj :description))
	(plist-put rrq :testing_done (plist-get obj :testing_done)))
      rrq)
    )
  )

(defun rb/front-get-request (id)
  "id の review request の情報を取得する"
  (let ((obj (rb/front-access-web (format "/api/review-requests/%d/" id)
				  nil nil :review_request)))
    (rb/front-conv-normalize obj)))


(defun rb/front-get-repository-tool-at-request (rrq)
  "id の review request が登録している repository が使用しているツールを取得する。"
  (let ((url (rb/front-access rrq
			      :links :repository :href)))
    (rb/front-access-web url nil nil :repository :tool)))


(defun rb/front-register-rrq (&optional id title description testing_done)
  "review request を登録する。

id: rrq の ID。 新規登録の場合 nil を指定。

この関数は ID を返す。失敗した場合 nil。
"
  (let (new-resp)
    (when (not id)
      (setq new-resp
	    ;; 新規登録。 diff を登録できるように repository を設定。
	    (rb/front-access-web
	     "/api/review-requests/" "POST"
	     (rb/front-get-send-data `(("repository" ,rb/front-rb-repository)))
	     ))
      (when (equal (plist-get new-resp :stat) "ok")
	(setq id (rb/front-access new-resp :review_request :id)))
      )
    (when id
      (rb/front-update-rrq id nil title description testing_done))
    )
  id
  )


(defun rb/front-draft-discard (id)
  (rb/front-access-web
   (format "/api/review-requests/%d/draft/" id) "DELETE")
  )

;; 
(defun rb/front-draft-publish (id &optional force)
  (let ((rrq (rb/front-get-request id))
	(user-list (rb/front-get-reviewer-candidate-list))
	body user)
    (if (and (not force)
	     (eq (length (plist-get rrq :target_people)) 0)
	     (eq (length (plist-get rrq :target_groups)) 0))
	(progn
	  ;; レビュワーを指定していない場合は、レビュワーを指定する
	  (setq user
		(completing-read "select reviewer (Hit TAB key to complete): "
				 user-list nil t))
	  (setq user (car (member user user-list)))
	  (setq body (rb/front-get-send-data
		      (list '("public" "true")
			    (list (if (eq (get-text-property 0 :kind user) 'user)
				      "target_people"
				    "target_groups")
				  (get-text-property 0 :name user))))))
      ;; レビュワーを指定している場合は、 public にするだけ
      (setq body (rb/front-get-send-data '(("public" "true")))))
    (rb/front-access-web
     (format "/api/review-requests/%d/draft/" id) "PUT" body)
    ))

(defun rb/front-draft-get-file-list-at-rrq (id)
  "id で指定する review request の、 登録ファイルパス一覧を取得する。"
  (rb/front-get-file-list (rb/front-get-request id)))

(defun rb/front-get-file-list (rrq)
  "id で指定する review request の、 登録ファイルパス一覧を取得する。"
  (let ((id (plist-get rrq :id))
	(draft-p t)
	diffs url)
    ;; drafts の diff を取る
    (setq diffs (rb/front-access-web
		 (format "/api/review-requests/%d/draft/diffs/" id) nil nil :diffs))
    (when (not diffs)
      ;; drafts の diff がなければ、 main の diff にアクセス
      (setq diffs (rb/front-access-web
		   (format "/api/review-requests/%d/diffs/" id) nil nil :diffs))
      (setq draft-p nil))
    (setq url (rb/front-access (nth (1- (length diffs)) diffs) :links
			       (if draft-p
				   :draft_files
				 :files)
			       :href))
    (when url 
      (mapcar (lambda (X)
		(plist-get X :source_file))
	      (rb/front-access-web url nil nil :files)
	      ))
    ))


(defun rb/front-get-review-list (id)
  "レビューコメントリストを取得する"
  (apply 'append
	 (mapcar (lambda (review)
		   (let (comment-list)
		     (setq comment-list (rb/front-access-web
					 (rb/front-access review
							  :links :diff_comments :href)
					 nil nil :diff_comments))
		     (mapcar (lambda (X)
			       (plist-put X :review-id (plist-get review :id)))
			     comment-list)))
		 (rb/front-access-web (format "/api/review-requests/%d/reviews/" id)
				      nil nil :reviews))))

(defun rb/front-get-review-comment (id review-id comment-id)
  (rb/front-access-web
   (format "/api/review-requests/%d/reviews/%d/diff-comments/%d/"
	   id review-id comment-id)
   nil nil :diff_comment))

(defun rb/front-get-reply-hash (id)
  "rrq id に対する reply コメントの
hash (返信元の コメント id → reply コメント) を取得する。
"
  (let ((reply-hash (make-hash-table))
	reply-list reply-comment-list)
    (setq reply-list
	  (apply 'append
		  (mapcar (lambda (review)
			    (rb/front-access-web
			     (rb/front-access review :links :replies :href)
			     nil nil :replies))
			  (rb/front-access-web
			   (format "/api/review-requests/%d/reviews/" id)
			   nil nil :reviews))))
    (setq reply-comment-list
	  (apply 'append
		 (mapcar (lambda (reply)
			   (rb/front-access-web
			    (rb/front-access reply :links :diff_comments :href)
			    nil nil :diff_comments))
			 reply-list)))
    (dolist (reply reply-comment-list)
      (let ((work (rb/front-access reply :links :reply_to :href))
	    parent-id reply-list sub-reply-list)
	(setq parent-id (string-to-number
			 (rb/front-repstr work ".*/\\([0-9]+\\)/$" "\\1" t)))
	(setq sub-reply-list (gethash parent-id reply-hash))
	(add-to-list 'sub-reply-list reply t)
	(puthash parent-id sub-reply-list reply-hash)
	)
      )
    reply-hash
    ))

(defun rb/front-add-reply-comment (id review-id comment-id reply-comment)
  "rrq id の レビュー review-id の コメント comment-id に対する
リプライreply-comment を登録する。
リプライは reviewboard のシステム上、編集できないので要注意。
"
  (let ((info (rb/front-access-web
	       (format "/api/review-requests/%d/reviews/%d/replies/" id review-id)
	       "POST" nil :reply :links)))
    (rb/front-access-web
     (rb/front-access info :diff_comments :href) "POST"
     (rb/front-get-send-data `(("reply_to_id" ,comment-id)
			       ("text" ,reply-comment)
			       )))
    (rb/front-access-web (rb/front-access info :update :href) "PUT"
			 (rb/front-get-send-data `(("public" 1))))
    ))

(defun rb/front-get-reviewer-candidate-list (&optional force)
  "reviewer に登録するユーザリストを取得する。

`(:name name :fullname fullname) のリストを返す。
"
  (when (or force (not rb/front-rbt-cache-reviewer-candidate-list))
    (setq rb/front-rbt-cache-reviewer-candidate-list
	  (append
	   (mapcar (lambda (X)
		     (propertize (format "%s(%s)"
					 (plist-get X :username)
					 (plist-get X :fullname))
				 :name (plist-get X :username)
				 :fullname (plist-get X :fullname)
				 :kind 'user))
		   (rb/front-access-web "/api/users/?max-results=200" nil nil :users))
	   (mapcar (lambda (X)
		     (propertize (plist-get X :name)
				 :name (plist-get X :name)
				 :fullname (plist-get X :display_name)
				 :kind 'group))
		   (rb/front-access-web "/api/groups/" nil nil :groups)))))
  rb/front-rbt-cache-reviewer-candidate-list
  )

(defun rb/front-get-my-reported-rrq-list ()
  "自分が投稿したレビューリストを取得する"
  (let* ((user (rb/front-access-web "/api/session/" nil nil
				    :session :links :user :title))
	 (list (rb/front-access-web
		(format "/api/review-requests/?from-user=%s" user) nil nil
		:review_requests))
	 )
    (delq nil
	  (mapcar (lambda (X)
		    (rb/front-conv-normalize X))
		  list))
    ))

(defun rb/front-registered-path-to-local-path (conv-info base-dir path)
  "reviewboard に登録されている path から、
ローカルの base-dir からの相対パスに変換する。

conv-info は変換情報。
base-dir 変換先のローカルパス。
path reviewboard に登録されているパス。
"
  (file-relative-name
   (expand-file-name 
    (file-relative-name path (plist-get conv-info :root-url-relative))
    (plist-get conv-info :root-path))
   base-dir))

(defun rb/front-normalize-file-list (id tool base-dir file-list)
  "review board に登録されているパスを、 base-dir からのローカルパスに変換する"
  (let ((local-path-list file-list))
    (let (repo-path-info)
      (setq repo-path-info (rb/front-tool-get-work-info tool base-dir))
      (rb/front-setting-set-work-info
       id (list :tool tool :id id :base-dir base-dir
		:top-dir (plist-get repo-path-info :root-path)))
      (setq local-path-list
	    (mapcar (lambda (X)
		      (rb/front-registered-path-to-local-path repo-path-info
							      base-dir X))
		    file-list))
      )
    local-path-list
    ))




(defun rb/front-path-join (dir path)
  (concat (file-name-as-directory dir) path))

(defun rb/front-repstr (target-string source-string destination-string
				      &optional regexp )
  (while (string-match source-string target-string)
    (setq target-string (replace-match destination-string
				       t (not regexp) target-string)))
  target-string)



(defvar rb/front-tool-funcs-list `("Subversion" ,rb/front-svn-funcs))

(defun rb/front-tool-get-funcs (tool)
  (or (cadr (member tool rb/front-tool-funcs-list))
      (error (concat "not support tool -- " tool)))
  )

(defun rb/front-tool-get-work-info (tool base-dir)
  (funcall (plist-get (rb/front-tool-get-funcs tool) :work-info) base-dir)
  )

(defun rb/front-tool-get-setup-to-add-files (tool base-dir)
  (funcall (plist-get (rb/front-tool-get-funcs tool)
		      :setup-to-add-files)
	   base-dir)
  )
(defun rb/front-tool-commit-files (tool message work-dir file-list)
  (funcall (plist-get (rb/front-tool-get-funcs tool)
		      :commit-files)
	   message work-dir file-list)
  )

(provide 'rbfront-lib)
