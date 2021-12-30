;;; consult-hatena-bookmark.el --- Consult commands for the Hatena Bookmark -*- lexical-binding: t -*-

;; Copyright (C) 2021 Yukinori Kitadai

;; Author: Yukinori Kitadai
;; Version: 0.0.0

;;; Command: `consult-hatena-bookmark`

;;; Commentary:

;; Preparation: Sign in Hatena Bookmark https://b.hatena.ne.jp in `w3m`.
;;
;; Run the function `consult-hatena-bookmark`

;;; Code:

(require 'consult)

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
            :highlight (cdr (consult--default-regexp-compiler input 'basic))))))

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
                                       (format "%-50.50s %-40.40s 💬%s %s"
                                               title url comment date)
                                       'consult--candidate url)))
                                  bookmarks)))))))
    (nreverse candidates)))

;;;###autoload
(defun consult-hatena-bookmark (&optional initial)
  "Search for your Hatena Bookmark with INITIAL input.
The process fetching your Hatena bookmarks is started asynchronously."
  (interactive)
  (unless (executable-find "w3m")
    (warn "The command w3m not found."))
  (browse-url (consult--read
               (consult--async-command #'consult--hatena-bookmark-builder
                 (consult--async-transform consult--hatena-bookmark-format)
                 (consult--async-highlight #'consult--hatena-bookmark-builder))
               :prompt "Hatena Bookmark: "
               :category 'url
               :require-match t
               :lookup #'consult--lookup-candidate
               :initial initial
               :add-history (consult--async-split-thingatpt 'symbol)
               :history '(:input consult--hatena-bookmark-history))))

(provide 'consult-hatena-bookmark)
