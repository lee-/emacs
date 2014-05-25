;;; multisearch.el --- automatically re-search multiple files
;;
;; Copyright 2014 lee@yun.yagibdah.de
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.
;;
;;TAG:CREATED 2014-03-18
;;
;TAG:LAST-MODIFIED 2014-04-02 20:18:35 CEST
;TAG:FILENAME multisearch.el


;;; Commentary
;;
;; Multisearch is a collection of functions that provide an easy way
;; to automatically search for a regexp in multiple files.  Files
;; relevant for the search can either be specified through key-value
;; pairs as described in the docstring of
;; `multisearch-make-directory-files-list', by listing file names in a
;; buffer, by combination thereof, or by searching through all files
;; in a directory, optionally including all files in all
;; subdirectories of the directory.  When searching through files in a
;; directory, the selection of relevant files can optionally be
;; limited to files the names of which match a particular regexp.
;;
;; The search results are presented nicely in a separate buffer.  All
;; buffers created during the search can be automatically killed on
;; demand, unless the very buffer which was current when the search
;; was performed is killed.  This makes it possible to perform
;; multiple searches on a set of files without visiting the files
;; again.
;;
;; The files listed in the search results can be conveniently visited
;; by calling `multisearch-show-file-at-point'.  (This gets locally
;; bound to <kp-enter> with the buffer showing the search results.)
;; In case point is near a line number of a search result, point is
;; placed at the beginning of the line in which the regexp that was
;; searched for was found.
;;
;; Default keybindings for the search functions utilize the F7 key ---
;; please see at the end of this file and adjust to your liking.
;;
;; Setup:
;;
;; M-x byte-compile-file multisearch.el
;;
;; Then load with a line like
;;
;; (load "multisearch")
;;
;; in your ~/.emacs.


;;; Code:

(defvar-local multisearch-buffer-list nil
  "This variable stores a list of the buffers created by the last
multisearch operation.  The buffers that are in this list can be
killed with `multisearch-kill-created-buffers'.

Do not change.")


(defun multisearch-show-file-at-point ()
  "Attempt to find a file name and a line number from around
point and show the file the name of which was found, with point
at the beginning of the line the number of which was found.

This works with lines inserted by `multisearch-some-files'."
  (interactive)
  (let ((line (buffer-substring (line-beginning-position) (line-end-position)))
	(line-number nil)
	(thing nil))
    (if (string-match "^\\s-*[0-9]*:\\s-" line)
	(progn
	  (setq line-number (string-to-number (replace-regexp-in-string "[^[0-9]]\\|:" "" line)))
	  (message "line %d" line-number)
	  (save-excursion
	    (re-search-backward "^/" (point-min) t)
	    (setq thing (thing-at-point 'filename t))))
      (setq thing (thing-at-point 'filename t)))
    (let ((buffer (if thing
		      (find-buffer-visiting thing)
		    nil)))
      (if buffer
	  (progn
	    (switch-to-buffer buffer)
	    (when (> line-number 0)
	      (goto-char (point-min))
	      (forward-line (- line-number 1))))
	(error "No buffer is visiting %s" thing)))))


(defsubst multisearch-remember-buffer (for-buffer previous-buffers-list buffer-to-remember)
  "Remember BUFFER-TO-REMEMBER in `multisearch-buffer-list' for
FOR-BUFFER when BUFFER-TO-REMEMBER has been created by a
multisearch operation."
  (unless (member buffer-to-remember previous-buffers-list)
    (with-current-buffer for-buffer
      (add-to-list 'multisearch-buffer-list buffer-to-remember))))


(defsubst multisearch-trim-string (my-string)
  "Trim leading and trailing whitespace from the string
MY-STRING."
  (replace-regexp-in-string "^\\s-*\\|\\s-*$" "" my-string))


(defun multisearch-some-files (files &optional for result-buffer-name)
  "Search for a regex in multiple files and create a buffer to show
the results.

The name of a buffer to show the search results in can be given
in RESULT-BUFFER-NAME.  When RESULTS-BUFFER-NAME is nil, the user
is queried for the name of a buffer.

The contents of the buffer are erased when the default buffer
name is used before output is inserted into the buffer.  When
another buffer name is used, the contents of the buffer are not
erased, and the output is inserted into the buffer at point.

The regexp to search for, if not given in FOR, is queried via the
minibuffer."
  (when files
    (let ((orig-buffer (current-buffer))
	  (buffer-provided result-buffer-name))
      (unless buffer-provided
	(setq result-buffer-name (read-from-minibuffer "Show results in buffer (*multisearch-results*): ")))
      (when (string-equal result-buffer-name "")
	(setq result-buffer-name "*multisearch-results*"))
      (unless for
	(setq for (read-from-minibuffer "Search regex: ")))
      (message "searching ...")
      (let ((buffer-list-before-new-buffer (buffer-list))
	    (results-buffer (get-buffer-create result-buffer-name)))
	(multisearch-remember-buffer orig-buffer buffer-list-before-new-buffer results-buffer)
	(setq buffer-list-before-new-buffer (cons results-buffer buffer-list-before-new-buffer))
	(with-current-buffer results-buffer
	  (when (or buffer-provided
		    (string-equal result-buffer-name "*multisearch-results*"))
	    (erase-buffer)))
	(dolist (foo files)
	  (when (file-readable-p foo)
	    (let ((buffer (find-file-noselect foo))
		  (scope nil))
	      (multisearch-remember-buffer orig-buffer buffer-list-before-new-buffer buffer)
	      (setq buffer-list-before-new-buffer (cons buffer buffer-list-before-new-buffer))
	      (with-current-buffer buffer
		(save-excursion
		  (save-restriction
		    (widen)
		    (goto-char (point-min))
		    (while (re-search-forward for (point-max) t)
		      (let ((lineno nil)
			    (linestart nil)
			    (linestring nil))
			(save-excursion
			  (beginning-of-line)
			  (setq linestart (point))
			  (end-of-line)
			  (setq linestring (buffer-substring linestart (point)))
			  (setq lineno (line-number-at-pos)))
			(when linestring
			  (with-current-buffer results-buffer
			    (unless scope
			      (insert (format "%s\n{\n" (buffer-file-name buffer)))
			      (setq scope t))
			    (insert (concat
				     (format "\t%6d:"
					     lineno)
				     (format "\t%s\n"
					     (multisearch-trim-string linestring))))))))
		    (when scope
		      (with-current-buffer results-buffer
			(insert (format "}\n\n"))))))))))
	(let ((created-buffers (copy-sequence multisearch-buffer-list)))
	  (with-current-buffer results-buffer
	    (insert (concat "The following files were searched for '" for "':\n\n"))
	    (dolist (this files)
	      (insert (format "%s\n" this)))
	    (insert (format
		     "\n\nThe following buffers were created by multisearch operations on %s:\n\n"
		     (buffer-name orig-buffer)))
	    (dolist (this created-buffers)
	      (insert (format "%s\n" (buffer-name this))))
	    (goto-char (point-min))
	    (local-set-key (kbd "<kp-enter>") 'multisearch-show-file-at-point)))
	(switch-to-buffer-other-window results-buffer)))))


(defsubst multisearch-get-thing ()
  "Since `thing-at-point' does not always return the value of a
key-value pair, this function attemtps to do a better job for the
purpose of multisearch by trying to return the string after the
first double-quote after point.

When no double quote is found, nil is returned.  The search is
limited to the current line."
  (save-excursion
    (backward-char)
    (if (re-search-forward "\"\\|<" (line-end-position) t)
	(let ((start-pos (point))
	      (delta (skip-chars-forward "^\\(\"\\|>\\)" (line-end-position))))
	  (buffer-substring-no-properties start-pos (+ start-pos delta)))
      (message (format "Multisearch syntax error in %s, %s" (buffer-name) (what-line)))
      nil)))


(defun multisearch-make-token-list (buffer token-rx &optional is-directory)
  "Re-search BUFFER for key-value pairs and return a list of values
  found from keys.  Each value is listed only once.

Keys are what matches TOKEN-RX.  Values are expected to be file names,
unless IS-DIRECTORY is non-nil, in which case they are expected to be
directory names."
  (with-current-buffer buffer
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(let ((token-list
	       (if (not is-directory)
		   (list (buffer-file-name))
		 nil)))
	  (while (re-search-forward token-rx (point-max) t)
	    (let ((token (multisearch-get-thing)))
	      (when token
		(when (and is-directory (not (string-match "/$" token)))
		  (setq token (concat token "/")))
		(setq token-list (cons token token-list)))))
	  token-list)))))


(defsubst multisearch-directory-ref-p (dots)
  "Return t when the string DOTS ends in a directory reference."
  (or
   (string-match "\\.\\'" dots)
   (string-match "\\.\\.\\'" dots)))


(defun multisearch-make-directory-list (of-directory)
  "Return a list of all subdirectories of OF-DIRECTORY.  The list
includes OF-DIRECTORY."
  (let ((new-dir-list (list (expand-file-name of-directory)))
	(this-dir (directory-files of-directory t nil t)))
    (dolist (entry this-dir new-dir-list)
      (when (and
	     (not (multisearch-directory-ref-p entry))
	     (file-directory-p entry)
	     (file-readable-p entry))
	(setq new-dir-list (append
			    new-dir-list
			    (multisearch-make-directory-list entry)))))))


(defun multisearch-make-files-list (directory &optional match)
  "Return a list of files in DIRECTORY, with directory references and
directories removed.

When MATCH is non-nil, only files that match the regexp MATCH are
included in the list."
  (let ((files-list (directory-files directory t match t))
	(clean-list nil))
    (dolist (entry files-list clean-list)
      (when (and
	     (not (multisearch-directory-ref-p entry))
	     (not (file-directory-p entry))
	     (file-readable-p entry))
	(setq clean-list (cons entry clean-list))))))


(defun multisearch-cleanup-directory-files-list (directory-files-list)
"Return a clean list of file names from (a messy)
DIRECTORY-FILES-LIST.

'Clean' means that only the names of regular files which are readable
are on the list."
(let ((clean-files-list nil))
  (dolist (this directory-files-list (delete-dups clean-files-list))
    (when (and
	   (not (multisearch-directory-ref-p this))
	   (not (file-directory-p this))
	   (file-readable-p this)
	   (file-regular-p this))
      (setq clean-files-list (cons (expand-file-name this) clean-files-list))))))


(defun multisearch-make-directory-files-list (buffer)
  "Return a list of file names created from key-value pairs to be
found in BUFFER.  This list is suited for
`multisearch-some-files'.

BUFFER is searched for the following keys:


multisearch-with-dir:
  This key specifies a directory in which relevant files might be
  found.

multisearch-with-file:
  This key specifies a particular file to include in the search.

multisearch-with-files-matching:
  This key specifies a regexp: Files the names of which match the
  regexp are included in the search.  Such files can be within
  any of the relevant directories.

#include
  This key is an alias of 'multisearch-with-file:'.  Both forms
  of #include typically found in source files are recognized,
  i. e. '#include <file>' and '#include \"file\"'.

multisearch-with-subdirs-of:
  This key pecifies a directory which potentially has directories
  within it.  Relevant files may be found within the specified
  directory and all directories within the specified directory.


With the exception of '#include', the value of `comment-start'
must precede the keys.  When `comment-start' is nil, '# ' is
assumed for it instead.

The names of files and directories should be enclosed in double
quotes, unless specified with #include.  This is to make sure
`multisearch-get-thing' reliably returns them."
  (message "building files list ...")
  (with-current-buffer buffer
    (let ((directory-list
	   (multisearch-make-token-list buffer
					(concat "^" (or comment-start "# ")
						"\\s-*multisearch-with-dir: [^[:space:]]") t))
	  (files-list
	   (append
	    (multisearch-make-token-list buffer
					 (concat "^" (or comment-start "# ")
						 "\\s-*multisearch-with-file: [^[:space:]]"))
	    (multisearch-make-token-list buffer
					 "^#include\\s-*[\"<]")))
	  (subdir-list
	   (multisearch-make-token-list buffer
					(concat "^" (or comment-start "# ")
						"\\s-*multisearch-with-subdirs-of: [^[:space:]]") t))
	  (matching-files-list
	   (multisearch-make-token-list buffer
					(concat "^" (or comment-start "# ")
						"\\s-*multisearch-with-files-matching: [^[:space:]]")))
	  (directory-files-list
	   nil))
      ;; put 'directory/file' on directory-files-list
      (dolist (directory directory-list)
	(when (file-directory-p directory)
	  (dolist (file files-list)
	    (let ((this (expand-file-name (concat directory file))))
	      (setq directory-files-list (cons this directory-files-list)))))
	(dolist (matching-file matching-files-list)
	  ;; put 'directory/matching-file' on directory-files-list
	  (setq directory-files-list (append
				      (multisearch-make-files-list directory matching-file)
				      directory-files-list))))
      ;; put 'subdirectory/file' on directory-files-list
      (let ((all-subdirs nil))
	(dolist (subdir subdir-list)
	  (when (string-match "/$" subdir)
	    (setq subdir (substring subdir 0 -1)))
	  (setq subdir (expand-file-name subdir))
	  (when (and
		 (file-directory-p subdir)
		 (file-readable-p subdir))
	    (setq all-subdirs (append (multisearch-make-directory-list subdir) all-subdirs))))
	(dolist (subdir all-subdirs)
	  (dolist (file files-list)
	    ;; // subdirs are already expanded file names
	    (setq directory-files-list (cons (concat subdir "/" file) directory-files-list)))
	  (dolist (matching-file matching-files-list)
	    ;; put 'subdir/matching-file' on directory-files-list
	    (setq directory-files-list (append
					(multisearch-make-files-list subdir matching-file)
					directory-files-list)))))
      ;; put 'file' on directory-files-list
      (dolist (this files-list)
	(setq directory-files-list (cons (expand-file-name this) directory-files-list)))
      (multisearch-cleanup-directory-files-list directory-files-list))))


(defun multisearch-estimate-buffer-creation (with-files)
  "Return an integer which is an estimation of how many
additional buffers need to be created to search the files listed
in WITH-FILES."
  (let ((buffers (copy-sequence (buffer-list)))
	(buffer-estimate 0)
	(file-name-list nil))
    (dolist (this buffers)
      (setq file-name-list (cons (buffer-file-name this) file-name-list)))
    (dolist (file-name with-files buffer-estimate)
      (unless (member file-name file-name-list)
	(setq buffer-estimate (+ buffer-estimate 1))))))


(defun multisearch-make-files-list-from-buffer (buffer)
  "Return a list of file names generated from lines read from BUFFER.

The list is cleaned with
`multisearch-cleanup-directory-files-list'."
  (with-current-buffer buffer
    (save-excursion
      (save-restriction
	(widen)
	(let ((lines (split-string (buffer-string)))
	      (file-names nil))
	  (dolist (this lines)
	    (setq file-names (cons (expand-file-name this) file-names)))
	  (multisearch-cleanup-directory-files-list file-names))))))


;; visible functions


(defun multisearch-kill-remembered-buffers ()
  "Kill all buffers that are remembered in
`multisearch-buffer-list' to have been created by a multisearch
operation with the current buffer."
  (interactive)
  (if multisearch-buffer-list
      (if (y-or-n-p
	   (format "Kill %d buffers created by multisearch? "
		   (length multisearch-buffer-list)))
	  (progn
	    (dolist (this multisearch-buffer-list)
	      (unless (kill-buffer this)
		(message "Couldn`t kill buffer %s" (buffer-name this))))
	    (setq multisearch-buffer-list nil)
	    (message "multisearch buffers killed"))
	(if (y-or-n-p
	     (format "Forget about %d buffers created by multisearch? "
		     (length multisearch-buffer-list)))
	    (progn
	      (setq multisearch-buffer-list nil)
	      (message "multisearch buffers forgotten about"))
	  (message "continuing to remember multisearch buffers")))
    (message "no multisearch buffers to kill or to forget about")))


(defun multisearch-re-search-some-files ()
  "Search for a regex in multiple files a list of which is
created with `multisearch-make-directory-files-list', which see.
The files are searched with `multisearch-some-files', which also
see."
  (interactive)
  (let ((files (multisearch-make-directory-files-list (current-buffer))))
    (if files
	(if (y-or-n-p
	     (format "This search may create about %d buffers.  Continue? "
		     (multisearch-estimate-buffer-creation files)))
	    (multisearch-some-files files)
	  (message "search canceled"))
      (message "no files to search"))))


(defun multisearch-re-search-files-in-current-directory (&optional match)
  "Search for a regex in files in the current directory.  When MATCH
is not nil, include only those files in the search the names of which
match the regexp MATCH.

Unlike `multisearch-re-search-files-in-current-directory-with-subs',
this search does not include files in sub-directories."
  (interactive)
  (let ((files (multisearch-make-files-list "." match)))
    (if files
	(if (y-or-n-p
	     (format "This search may create about %d buffers.  Continue? "
		     (multisearch-estimate-buffer-creation files)))
	    (multisearch-some-files files)
	  (message "search canceled"))
      (message "no files to search"))))


(defun multisearch-re-search-files-in-current-directory-with-subs (&optional match)
  "Search for a regexp in files in the current directory and the
directories within the current directory.  When MATCH is not nil,
include only those files in the search the names of which match
the regexp MATCH."
  (interactive)
  (let ((directory-list (multisearch-make-directory-list "."))
	(files (multisearch-make-files-list "." match)))
    (dolist (directory directory-list)
      (setq files (append (multisearch-make-files-list directory match) files)))
    (if files
	(if (y-or-n-p
	     (format "This search may create about %d buffers.  Continue? "
		     (multisearch-estimate-buffer-creation files)))
	    (multisearch-some-files files)
	  (message "search canceled"))
      (message "no files to search"))))


(defun multisearch-re-search-matching-files-in-current-directory ()
  "Wrapper for `multisearch-re-search-files-in-current-directory'
to read the regexp the names of the files to search through must
match from the minibuffer."
  (interactive)
  (let ((match (read-from-minibuffer "Regex files have to match (nil): ")))
    (when (string-equal "" match)
      (setq match nil))
    (multisearch-re-search-files-in-current-directory match)))


(defun multisearch-re-search-matching-files-in-current-directory-with-subs ()
  "Wrapper for
`multisearch-re-search-files-in-current-directory-with-subs' to
read the regexp the names of the files to search through from the
minibuffer."
  (interactive)
  (let ((match (read-from-minibuffer "Regex files have to match (.*): ")))
    (when (string-equal "" match)
      (setq match nil))
    (multisearch-re-search-files-in-current-directory-with-subs match)))


(defun multisearch-re-search-some-files-with-listed-within ()
  "Search for a regex in multiple files a list of which is
created with `multisearch-make-directory-files-list' and
`multisearch-make-files-list-from-buffer', which see.

The files are searched with `multisearch-some-files', which also
see."
  (interactive)
  (let ((files (append (multisearch-make-files-list-from-buffer (current-buffer))
		       (multisearch-make-directory-files-list (current-buffer)))))
    (if files
	(if (y-or-n-p
	     (format "This search may create about %d buffers.  Continue? "
		     (multisearch-estimate-buffer-creation files)))
	    (multisearch-some-files files)
	  (message "search canceled"))
      (message "no files to search"))))


;; suggested key bindings


(defvar multisearch-map (make-sparse-keymap)
  "Keymap for multisearch.")

(define-prefix-command 'multisearch-map)
(define-key multisearch-map (kbd "s") 'multisearch-re-search-some-files)
(define-key multisearch-map (kbd "k") 'multisearch-kill-remembered-buffers)
(define-key multisearch-map (kbd "d") 'multisearch-re-search-files-in-current-directory)
(define-key multisearch-map (kbd "D") 'multisearch-re-search-files-in-current-directory-with-subs)
(define-key multisearch-map (kbd "m") 'multisearch-re-search-matching-files-in-current-directory)
(define-key multisearch-map (kbd "M") 'multisearch-re-search-matching-files-in-current-directory-with-subs)
(define-key multisearch-map (kbd "b") 'multisearch-re-search-some-files-with-listed-within)

(global-set-key [(f7)] 'multisearch-map)
