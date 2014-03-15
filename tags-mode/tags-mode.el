;; -*- eval: (tags-mode) -*-
;;
;; copyright: lee@yun.yagibdah.de
;;
;TAG:LAST-MODIFIED 2014-03-15 09:27:53 CET
;TAG:FILENAME tags-mode.el
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.
;;
;; TAG:CREATED 2014-03-15
;; TAG:FILENAME tags-mode.el


;; TAGS


(defsubst tags-comment-start-protected ()
  "Since `comment-start' can sometimes be `nil', return a default
for such instances, otherwise return `comment-start'."
  (or comment-start "# "))


(defun tags-find (tag &optional not-at-begin)
  "Find TAG in the current buffer and return the beginning of the
line the tag was found in.  When TAG is not found, return `nil'.

When the optional argument NOT-AT-BEGIN is `nil', start searching for
TAG at the beginning of the buffer.  Otherwise, start at point.

All tags are expected to be at the beginning of a line and to be
prepended by `comment-start'.  For instances when `comment-start'
is `nil', '# ' is expected to be prepended."
  (save-excursion
    (save-restriction
      (widen)
      (let ((tag-rx
	     (concat "^"
		     (tags-comment-start-protected)
		     "TAG:"
		     tag)))
	(unless not-at-begin
	  (goto-char (point-min)))
	(if (re-search-forward tag-rx (point-max) t)
	    (line-beginning-position)
	  ;; else
	  nil)))))


(defsubst tags-remove-line (at-character-position &optional more)
  "Kill a line or two at the given character position in the current
buffer, from the beginning of the line.

When only one line is killed, that line will be filled with an updated
tag.  When the line isn`t going to be filled again, the whole line
must be killed for not to leave blank lines randomly about the buffer."
  (when at-character-position
    ;; nil character positions are unreachable
    (goto-char at-character-position)
    (beginning-of-line)
    (if more
	;; kill one more line
	(kill-line 1)
      (kill-line))))


(defun tags-delete (tag &optional all-of-them)
  "Delete lines in the current buffer containing the tag given as
argument.

When the optional argument ALL-OF-THEM is `nil', delete the first line
that contains the tag, starting at the beginning of the buffer.
Otherwise, delete all lines after point that contain the tag."
  (save-excursion
    (save-restriction
      (widen)
      (let ((found (tags-find tag all-of-them)))
	(tags-remove-line found all-of-them)
	(when all-of-them
	  (let ((so-many 0))
	    (setq found (tags-find tag t))
	    (while found
	      (tags-remove-line found t)
	      (setq so-many (+ 1 so-many))
	      (setq found (tags-find tag t)))
	    ;; let the user decide whether the result looks unreal or not
	    (message "tags-mode --- multiple occurances removed: %d"
		     so-many)))))))


(defsubst tags-reasonable-insert-position (tag)
  "Find a reasonable place at the beginning of the current buffer to
insert a tag line.  Return the beginning of the line of this
reasonable place.

As a reasonable position is considered either the occurance of the
tag, or the first blank line in the buffer.  When there is no blank
line, or when the first blank line is too far away from the beginning
of the buffer, the third line of the buffer is considered as
reasonable."
  (save-excursion
    (save-restriction
      (widen)
      (or
       (tags-find tag)
       (if (re-search-forward "^$" (/ (point-max) 3) t)
	   (line-beginning-position)
	 (goto-char (point-min))
	 (forward-line 2)
	 (point))))))


(defun tags-insert-last-modified (&optional delete-before all-of-them position)
  "Insert a tag into the current buffer, saying that it was last
modified at the current local time.

When the two optional arguments DELETE-BEFORE and ALL-OF-THEM are both
not `nil', the first tag is replaced, and all following tags are
deleted.

When DELETE-BEFORE is `nil', the first tag is not deleted, but a new
one is added.  Unless ALL-OF-THEM is also `nil', the following tags
are not deleted, either.  Otherwise, they are deleted.

The optional third argument POSITION specifies the character position
at which the tag should be inserted, unless POSITION is `nil'.  This
character position should be at the beginning of a line.

All operations begin either at point, or at POSITION, and they are
limited to tags of the same kind."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (when position
	(goto-char position))
      (when delete-before
	(tags-delete
	 ;; like: ";LAST-MODIFIED 2014-03-14 17:35:46 CET"
	 "LAST-MODIFIED [0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\} [^[:space:]]"))
      (when (not (looking-at "$"))
	(insert "\n")
	(forward-line -1))
      (insert (format "%sTAG:LAST-MODIFIED %s"
		      comment-start
		      (format-time-string "%Y-%m-%d %H:%M:%S %Z")))
      (when all-of-them
	(forward-line)
	(tags-delete
	 ;; like: ";LAST-MODIFIED 2014-03-14 17:35:46 CET"
	 "LAST-MODIFIED [0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\} [^[:space:]]" t)))))


(defun tags-insert-file-name (&optional delete-before all-of-them position)
  "Insert into the current buffer the name of the file the buffer is
visiting.

When the two optional arguments DELETE-BEFORE and ALL-OF-THEM are both
not `nil', the first tag is replaced, and all following tags are
deleted.

When DELETE-BEFORE is `nil', the first tag is not deleted, but a new
one is added.  Unless ALL-OF-THEM is also `nil', the following tags
are not deleted, either.  Otherwise, they are deleted.

The optional third argument POSITION specifies the character position
at which the tag should be inserted, unless POSITION is `nil'.  This
character position should be at the beginning of a line.

All operations begin either at point, or at POSITION, and they are
limited to tags of the same kind."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (when position
	(goto-char position))
      (when delete-before
	(tags-delete "FILENAME [^[:space:]]"))
      (when (not (looking-at "$"))
	(insert "\n")
	(forward-line -1))
      (insert (format "%sTAG:FILENAME %s"
		      comment-start
		      (file-name-nondirectory (buffer-file-name))))
      (when all-of-them
	(forward-line)
	(tags-delete "FILENAME [^[:space:]]" t)))))


(defun tags-insert-file-name-with-last-modified (&optional delete-before all-of-them position)
  "Use `tags-insert-file-name' and `tags-insert-last-modified' to
insert such tags."
  (interactive)
  (tags-insert-file-name delete-before all-of-them position)
  (tags-insert-last-modified delete-before all-of-them position))


(defun tags-update-file-name-with-last-modified ()
  "Use `tags-insert-file-name' and `tags-insert-last-modified' to
update such tags.

This updates one of each type."
  (interactive)
  (tags-insert-file-name t)
  (tags-insert-last-modified t))


;; in case more tags are added, 'tags-update-all should update them
;;
(defun tags-update-all ()
  "Use `tags-insert-file-name' and `tags-insert-last-modified' to
update such tags.

This deletes all occurances of each tag, regardless of
visibility, and puts new ones somewhere at the beginning of the
buffer.

This function is intended to be called from a hook, like
`before-save-hook'."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (tags-insert-file-name t t
			     (tags-reasonable-insert-position
			      "FILENAME [^[:space:]]"))
      (goto-char (point-min))
      (tags-insert-last-modified t t
				 (tags-reasonable-insert-position
				  "LAST-MODIFIED [0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\} [^[:space:]]"))))
  (set-buffer-modified-p t))


(define-minor-mode tags-mode
  "This minor mode provides some functionality to handle tags:


`tags-insert-last-modified'

`tags-insert-file-name'

`tags-insert-file-name-with-last-modified'

`tags-update-file-name-with-last-modified'

`tags-update-all'


This mode doesn`t come with key bindings.  It locally adds
`tags-update-all' to `after-save-hook' when enabled and removes
the hook when disabled.


WARNING:

You may want to add `tags-update-all' to `before-save-hook'
instead.  The advantage would be that the tags are updated before
the buffer is saved to a file.  The obvious disadvantage would be
that the buffer is modified by functions of this mode right
before you save it:

In case something goes wrong and buffer contents get messed up or
are deleted, you have saved the messed up version before you
could do anything about it.  You can use `undo', but you might
not notice before it`s too late.

When you have the unmodified version saved, you can check it and
save it again to get the tags updated in the file."
  :lighter " tags"
  (if tags-mode
      (add-hook 'after-save-hook 'tags-update-all t t)
    (remove-hook 'after-save-hook 'tags-update-all t)))
