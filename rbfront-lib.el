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


(defvar rb/front-rb-api-token "")
(defvar rb/front-rb-url "http://localhost/rb")
(defvar rb/front-rbt "rbt")
(defvar rb/front-proxy "http://localhost:8080/")
(defvar rb/front-rb-repository "")

(defvar rb/front-access-buf "*rb/front/access*")
(defvar rb/front-rbt-buf "*rb/front/rbt*")


(defvar rb/front-rbt-cache-reviewer-candidate-list nil)

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

(defun rb/front-exec-rbt (id basedir file-list)
  "id の review request に file-list の diff を登録する。

basedir は、 rbt を実行するディレクトリパス。
file-list は、 basedir からの相対パスのリスト。
"
  (with-current-buffer (get-buffer-create rb/front-rbt-buf)
    (erase-buffer)
    (setq default-directory (expand-file-name basedir))
    (apply 'start-process rb/front-rbt-buf rb/front-rbt-buf
	   rb/front-rbt "post" "--repository" rb/front-rb-repository
	   "--server" rb/front-rb-url "--api-token" rb/front-token
	   "-r" (format "%s" id)
	   (apply 'append (mapcar (lambda (X) (list "-I" X)) file-list)))
    (switch-to-buffer-other-window rb/front-rbt-buf)
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
	(setq obj (json-read-from-string
		   (buffer-substring-no-properties (point-min)
						   (point-max))))))
    (when key-list
      (setq obj (apply 'rb/front-access obj key-list)))
    obj
  ))

(defun rb/front-close-review (review)
  (let ((id (plist-get review :id)))
    (when (rb/front-private-p review)
      ;; public にしてからじゃないと close できないので
      (rb/front-draft-publish id t))
    (rb/front-access-web
     (format "/api/review-requests/%d/" id) "PUT"
     (rb/front-get-send-data `(("status" "submitted")))
     )
    ))

(defun rb/front-discard-review (id)
  (rb/front-access-web
   (format "/api/review-requests/%d/" id) "PUT"
   (rb/front-get-send-data `(("status" "discarded")))
   )
  )

(defun rb/front-edit-review (id private-p title description testing_done)
  "id のドラフト review request の情報を編集する"
  (rb/front-access-web
   (format "/api/review-requests/%d/draft/" id) "PUT"
   (rb/front-get-send-data `(("description" ,(or description ""))
			     ("summary" ,(or title "new review"))
			     ("testing_done" ,(or testing_done ""))
			     ,(if (not private-p)
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

(defun rb/front-public-p (review)
  (eq (plist-get review :public) t))

(defun rb/front-private-p (review)
  (not (rb/front-public-p review)))


(defun rb/front-conv-normalize (review)
  "review が draft の場合、
summary 等が draft 情報にしか入っていないので、draft から情報をとる。
"
  (let ((id (plist-get review :id)))
    (if (rb/front-public-p review)
	;; public ならそのまま返す
	review
      ;;; public でなければ draft を取る
      (let ((obj (rb/front-access-web
		  (format "/api/review-requests/%d/draft/" id)
		  nil nil :draft)))
	;; draft の ID は draft_id として保持
	(plist-put obj :draft_id (plist-get obj :id))
	;; draft の ID は request の ID と違うので、 request の ID で上書き
	(plist-put obj :id id)
	obj)
      )
    ))

(defun rb/front-get-request (id)
  "id の review request の情報を取得する"
  (let ((obj (rb/front-access-web (format "/api/review-requests/%d/" id)
				  nil nil :review_request)))
    (rb/front-conv-normalize obj)))


(defun rb/front-get-repository-tool-at-request (review)
  "id の review request が登録している repository が使用しているツールを取得する。"
  (let ((url (rb/front-access review
			      :links :repository :href)))
    (rb/front-access-web url nil nil :repository :tool)))


(defun rb/front-new-request (basedir file-list
				     &optional title description testing_done)
  "review request を新規に登録する。
同時に publish する。
"
  (let (new-resp id)
    (setq new-resp
	  ;; 新規登録。 diff を登録できるように repository を設定。
	  (rb/front-access-web
	   "/api/review-requests/" "POST"
	   (rb/front-get-send-data `(("repository" ,rb/front-rb-repository)))
	   ))
    (when (equal (plist-get new-resp :stat) "ok")
      (setq id (rb/front-access new-resp :review_request :id))
      (rb/front-edit-review id t title description testing_done)
      )
    (rb/front-exec-rbt id basedir file-list)
    (rb/front-draft-publish id)
    ))


;; 
(defun rb/front-draft-publish (id &optional force)
  (let ((review (rb/front-get-request id))
	(user-list (rb/front-get-reviewer-candidate-list))
	body user)
    (if (and (not force)
	     (eq (length (plist-get review :target_people)) 0)
	     (eq (length (plist-get review :target_groups)) 0))
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

(defun rb/front-draft-get-file-list-at-review (id)
  "id で指定する review request の、 登録ファイルパス一覧を取得する。"
  (rb/front-get-file-list (rb/front-get-request id)))

(defun rb/front-get-file-list (review)
  "id で指定する review request の、 登録ファイルパス一覧を取得する。"
  (let ((id (plist-get review :id))
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
  )))





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

(defun rb/front-get-my-reported-review-list ()
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
    (file-relative-name path (plist-get conv-info :root-url))
    (plist-get conv-info :root-path))
   base-dir))

(defun rb/front-normalize-file-list (tool base-dir file-list)
  "review board に登録されているパスを、 base-dir からのローカルパスに変換する"
  (let ((local-path-list file-list))
    (let (repo-path-info)
      (cond ((equal tool "Subversion")
	     (setq repo-path-info (rb/front-svn-info base-dir))
	     (setq local-path-list
		   (mapcar (lambda (X)
			     (rb/front-registered-path-to-local-path repo-path-info
								     base-dir X))
			   file-list))
	     )
	    (t
	     (error (concat "not support -- " tool)))
	    )
      )
    local-path-list
    ))


(defun rb/front-convert-file-list (tool base-dir file-list)
  "ローカルの file-list を review board に登録されるパスに変換する。"
  (let ((rb-path-list file-list)
	conv-info)
    (cond ((equal tool "Subversion")
	   (setq conv-info (rb/front-svn-info base-dir))
	   (setq rb-path-list
		 (mapcar (lambda (X)
			   (concat
			    (plist-get conv-info :root-url)
			    (file-relative-name (expand-file-name X base-dir)
						(plist-get conv-info :root-path))
			    ))
			 file-list))
	   )
	  (t
	   (error (concat "not support -- " tool)))
	  )
    rb-path-list
    ))



(provide 'rbfront-lib)
