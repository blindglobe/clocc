;;- Reverse the page order in the `pstotext' output.

;; Note: This is a batch mode command!

;; Fairly ad-hoc.  It could be generalized, but I need more knowledge of postscript, I think.
(defun clx-man-add-page-comments-to-ps ()
  (unless (noninteractive)
    (error "Only for -batch mode."))
  (while command-line-args-left
    (find-file (car command-line-args-left))
    (goto-char (point-min))
    (let* ((n-pages (loop with n = 0
			 ;; I know that in these files, the `bop' is at the start of line.
			 while (re-search-forward "^bop" nil t)
			 do (incf n)
			 finally return n))
	   (title (file-name-nondirectory (replace-in-string (buffer-file-name)
							     "\.orig" "" t)))
	   (rev (get-buffer-create title))
	   (buf (current-buffer))
	   trailer beg end)
      (goto-char (point-min))
      (insert (concat "%!PS-Adobe-2.0\n"
		      "%%Title: " title "\n"
		      "%%Pages: " n-pages "\n"
		      "%%PageOrder: Ascend\n" ; after they are reversed, at any rate.
		      "%%EndComments\n"
		      "%%%%% "))
      (re-search-forward "^bop")
      (beginning-of-line)
      (previous-line 1)
      (end-of-line)
      (insert "\n%%EndProlog\n")
      (previous-line 2)
      ;; number the pages.
      (while (re-search-forward "^bop" nil t)
	(beginning-of-line)
	(insert (concat "\n%%Page: " n-pages "\n"))
	(forward-line 1)
	(setq n-pages (1- n-pages)))
      ;; Now physically reverse them, so BuildLectern will see them in
      ;; the correct unshuffled order.
      (goto-char (point-min))
      (goto-char (setq end (re-search-forward "^%%EndProlog\n")
		       beg (point-min)))
      (with-current-buffer rev
	(insert (buffer-substring beg end buf)))
      (delete-region beg end)		; get it out of our way.
      (goto-char (point-max))
      (re-search-backward "^eop")
      (end-of-line)
      (forward-char 1)
      (setq trailer (buffer-substring (point) (point-max))
	    end (point))
      (while (re-search-backward "^%%Page:" nil t)
	(setq beg (point))
	(with-current-buffer rev
	  (insert (buffer-substring beg end buf)))
	(setq end beg))
      (with-current-buffer rev
	(insert "\n%%Trailer\n" trailer "\n%%EOF\n")
	(let ((buffer-file-name title))
	  (save-buffer))
	(kill-buffer (current-buffer))))
    (kill-buffer (current-buffer))
    (setq command-line-args-left (cdr command-line-args-left)))
  (kill-emacs))
