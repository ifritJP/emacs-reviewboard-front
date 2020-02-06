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

(defvar rb/front-svn-buf "*rb/front/svn*")
(defvar rb/front-svn-path "svn")

(setq rb/front-svn-funcs
      '(:work-info rb/front-svn-info
		   :setup-to-add-files rb/front-svn-setup-to-add-files
		   :commit-files rb/front-svn-commit-files
		   ))

(defun rb/front-svn-commit-sentinel (process status input-file)
  (let ((status (process-status process)))
    (when (or (eq status 'exit)
	      (eq status 'signal))
      (delete-file input-file)
      )))

(defun rb/front-svn-commit-files (message work-dir file-list)
  (with-current-buffer (get-buffer-create rb/front-svn-buf)
    (rb/front-set-default-dir work-dir)
    (let (process input-file)
      (with-temp-buffer
	(setq input-file (make-temp-file "rbfront-svn" nil nil))
	(insert message)
	(write-region (point-min) (point-max) input-file)
	)
      (insert "processing...\n")
      
      (setq process
	    (apply 'start-process rb/front-svn-buf rb/front-svn-buf
		   rb/front-svn-path "commit" "-F" input-file file-list))
      (when (processp process)
	(set-process-sentinel process
			      (lambda (proc event)
				(rb/front-svn-commit-sentinel proc event input-file))))
      (rb/front-switch-to-buffer-other-window rb/front-svn-buf)
      (end-of-buffer)
      (recenter 0)
      )
    )
  )
      
(defun rb/front-svn-setup-to-add-files (work-dir)
  (when (get-buffer svn-status-buffer-name)
    (rb/front-switch-to-buffer-other-window
     (get-buffer svn-status-buffer-name)))
  (svn-status work-dir))

(defun rb/front-svn-info (base-dir)
  (let (root-path root-url)
    (with-temp-buffer
      (rb/front-set-default-dir base-dir)
      (call-process "svn" nil (current-buffer) nil "info" base-dir)
      (beginning-of-buffer)
      (re-search-forward "Working Copy Root Path: ")
      (setq root-path (buffer-substring-no-properties (point) (point-at-eol)))
      )
    (with-temp-buffer
      (rb/front-set-default-dir root-path)
      (call-process "svn" nil (current-buffer) nil "info" root-path)
      (beginning-of-buffer)
      (re-search-forward "Relative URL: ^/")
      (setq root-url (buffer-substring-no-properties (point) (point-at-eol)))
      )
    (list :root-path root-path :root-url-relative root-url)
    )
  )


(add-hook 'svn-status-mode-hook 'rb/front-svn-hook)
(defun rb/front-svn-hook ()
  (local-set-key (kbd "j") 'rb/front-new-request-from-svn-status))
(defun rb/front-new-request-from-svn-status ()
  (interactive)
  (rb/front-edit-add-file
   (rb/front-file-info-create "Subversion"
			      default-directory
			      (svn-status-marked-file-names))))



(provide 'rbfront-svn)
