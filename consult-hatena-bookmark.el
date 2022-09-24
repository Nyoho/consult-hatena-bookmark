;;; consult-hatena-bookmark.el --- Consult commands for the Hatena Bookmark -*- lexical-binding: t -*-

;; Copyright (C) 2021 Yukinori Kitadai

;; Author: Yukinori Kitadai
;; Package-Requires: ((emacs "27.1") (consult "0.9"))
;; Version: 0.1.0
;; URL: https://github.com/Nyoho/consult-hatena-bookmark

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Command: `consult-hatena-bookmark`

;;; Commentary:

;; Preparation: Set `consult-hatena-bookmark-hatena-username'
;; and `consult-hatena-bookmark-hatena-api-key'.

;; Run the function `consult-hatena-bookmark`

;;; Code:

(require 'subr-x)
(require 'consult)

(defgroup consult-hatena-bookmark nil
  "Consulting Hatena Bookmark."
  :group 'convenience)

(defcustom consult-hatena-bookmark-hatena-username nil
  "Your username of Hatena."
  :group 'consult-hatena-bookmark
  :type 'string)

(defcustom consult-hatena-bookmark-hatena-api-key nil
  "Your API key of Hatena.

Your API key is the string of the part of your submission mail address
 before `@'.
See:
https://www.hatena.ne.jp/my/config/mail/upload
https://developer.hatena.ne.jp/ja/documents/auth/apis/wsse ."
  :group 'consult-hatena-bookmark
  :type 'string)

(defvar consult--hatena-bookmark-history nil)

(defun consult--hatena-bookmark-position (cand &optional find-file)
  "Return the hatena-bookmark position marker for CAND.
FIND-FILE is the file open function, defaulting to `find-file'."
  (when cand
    (let* ((file-end (next-single-property-change 0 'face cand))
           (line-end (next-single-property-change (+ 1 file-end) 'face cand))
           (col (next-single-property-change (+ 1 line-end) 'face cand))
           (file (substring-no-properties cand 0 file-end))
           (line (string-to-number (substring-no-properties cand (+ 1 file-end) line-end))))
      (setq col (if col (- col line-end 1) 0))
      (consult--position-marker
       (funcall (or find-file #'find-file) file)
       line col))))

(defun consult--hatena-bookmark-state ()
  "Hatena-Bookmark preview state function."
  (let ((open (consult--temporary-files))
        (jump (consult--jump-state)))
    (lambda (cand restore)
      (when restore
        (funcall open))
      (funcall jump
               (consult--hatena-bookmark-position cand (and (not restore) open))
               restore))))


(defun consult--hatena-bookmark-builder (input)
  "Build command line given INPUT."
  (pcase-let ((`(,arg . ,opts) (consult--command-split input)))
    (unless (string-blank-p arg)
      (list :command (split-string-and-unquote (format "w3m -dump https://b.hatena.ne.jp/my/search/json?q=%s" (url-hexify-string input)))
            :highlight (cdr (consult--default-regexp-compiler input 'basic t))))))

(defun consult--hatena-bookmark-format (lines)
  "Format bookmark candidates from LINES."
  (let ((candidates nil))
    (save-match-data
      (dolist (str lines)
        (let* ((json (json-parse-string str))
               (meta (gethash "meta" json))
               (bookmarks (gethash "bookmarks" json)))
          (unless (eq (gethash "total" meta) 0)
            (setq candidates
                  (append candidates
                          (mapcar (lambda (item)
                                    (let* ((ts (gethash "timestamp" item))
                                           (date (format-time-string "%Y-%m-%d %a %H:%M:%S" (seconds-to-time ts)))
                                           (comment (gethash "comment" item))
                                           (entry (gethash "entry" item))
                                           (url (gethash "url" entry))
                                           (title (gethash "title" entry)))
                                      (add-face-text-property 0 (length url) 'consult-file nil url)
                                      (propertize
                                       (format "%-50.50s %-40.40s %-23.23s 💬%s"
                                               title url date comment)
                                       'consult--candidate url)))
                                  bookmarks)))))))
    (nreverse candidates)))

(defun consult--hatena-bookmark-string-to-list (str)
  "Parse string STR as JSON and construct candidates."
  (let (candidates)
    (save-match-data
      (if-let (json (json-parse-string str))
          (let* ((meta (gethash "meta" json))
                 (bookmarks (gethash "bookmarks" json)))
            (unless (eq (gethash "total" meta) 0)
              (setq candidates
                    (append candidates
                            (mapcar (lambda (item)
                                      (let* ((ts (gethash "timestamp" item))
                                             (date (format-time-string "%Y-%m-%d %a %H:%M:%S" (seconds-to-time ts)))
                                             (comment (gethash "comment" item))
                                             (entry (gethash "entry" item))
                                             (count (gethash "count" entry))
                                             (url (gethash "url" entry))
                                             (title (gethash "title" entry)))
                                        (add-face-text-property 0 (length url) 'consult-file nil url)
                                        (add-face-text-property 0 (length comment) 'marginalia-char nil comment)
                                        (propertize 
                                         (propertize
                                          (concat
                                           (format "%s %s" title url)
                                           (if (string= comment "") "" (format " %s" comment)))
                                          'hatena-bookmark-item
                                          `((date    . ,date)
                                            (comment . ,comment)
                                            (count . ,count)
                                            ))
                                         'consult--candidate
                                         url)))
                                    bookmarks)))))))
    (nreverse candidates)))

(defun consult--hatena-bookmark-get (callback input)
  "Access the Hatena Bookmark API with INPUT and pass the result to CALLBACK."
  (let* ((username consult-hatena-bookmark-hatena-username)
         (api-key consult-hatena-bookmark-hatena-api-key)
         (nonce
             (secure-hash 'sha1 (secure-hash 'sha1 (number-to-string (random 100000000000000)) nil nil t) nil nil t))
         (created (format-time-string "%Y-%m-%dT%H:%M:%SZ"))
         (digest
          (base64-encode-string
           (secure-hash 'sha1 (concat nonce created api-key) nil nil t)))
         (wsse-header
          (format
           "UsernameToken Username=\"%s\", PasswordDigest=\"%s\", Nonce=\"%s\", Created=\"%s\""
           username
           digest
           (base64-encode-string nonce)
           created))
         (url-request-method "GET")
         (url-request-extra-headers
          `(("Authorization" . "WSSE profile=\"UsernameToken\"")
            ("X-WSSE" . ,wsse-header)))
         (url
          (format
           "https://b.hatena.ne.jp/my/search/json?q=%s"
           (url-hexify-string input))))
    (url-retrieve url
                  (lambda (status)
                    (goto-char url-http-end-of-headers)
                    (funcall callback
                             (thread-first
                               (buffer-substring (point) (point-max))
                               (consult--hatena-bookmark-string-to-list)))))))

(defun consult--hatena-bookmark-search (callback &optional input)
  "Perform a search query for INPUT, receiving its results with CALLBACK."
  (consult--hatena-bookmark-get callback input))

(defun consult--hatena-bookmark-search-all (callback &optional input)
  "Perform a search query for INPUT, receiving its results with CALLBACK."
  (consult--hatena-bookmark-search (lambda (&rest items)
                                     (funcall callback (apply #'append items)))
                                   input))

(defun consult--hatena-bookmark--async-search (next input)
  "Async search with NEXT, INPUT."
  (let ((current ""))
    (lambda (action)
      (pcase action
        (""
         )
        ((pred stringp)
         (consult--hatena-bookmark-search-all
          (lambda (x)
            (funcall next 'flush)
            (funcall next x))
          action))
        (_ (funcall next action))))))

(defun consult-hatena-bookmark--search-generator (input)
  "Generate an async search closure for INPUT."
  (thread-first (consult--async-sink)
    (consult--async-refresh-immediate)
    (consult--hatena-bookmark--async-search input)
    (consult--async-throttle)
    (consult--async-split)))

;;;###autoload
(defun consult-hatena-bookmark (&optional initial)
  "Search for your Hatena Bookmark with INITIAL input.
The process fetching your Hatena bookmarks is started asynchronously."
  (interactive)
  (unless consult-hatena-bookmark-hatena-username
    (warn "Set consult-hatena-bookmark-hatena-username."))
  (unless consult-hatena-bookmark-hatena-api-key
    (warn "Set consult-hatena-bookmark-hatena-api-key."))
  (browse-url (consult--read
               (consult-hatena-bookmark--search-generator initial)
               :prompt "Hatena Bookmark: "
               :category 'hatena-bookmark-item
               :require-match t
               :lookup #'consult--lookup-candidate
               :initial (consult--async-split-initial initial)
               :add-history (consult--async-split-thingatpt 'symbol)
               :history '(:input consult--hatena-bookmark-history))))


(with-eval-after-load "marginalia"
  (defun consult-hatena-bookmark--annotate (cand)
    "Compute marginalia fields for candidate CAND."
    (when-let (x (get-text-property 0 'hatena-bookmark-item cand))
      (marginalia--fields
       ((if-let (d (alist-get 'count x))
            (format "%5d" d) "")
        :face 'marginalia-number :width 5)
       ((if-let (d (alist-get 'date x))
            (format "%s" d)
          "")
        :face 'marginalia-date :width -10))))

  (add-to-list 'marginalia-annotator-registry
               '(hatena-bookmark-item consult-hatena-bookmark--annotate)))

(provide 'consult-hatena-bookmark)

;;; consult-hatena-bookmark.el ends here
