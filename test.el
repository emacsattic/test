;;; Unit test framework for Emacs lisp program

(require 'cl)

(defvar test-cases '()
  "All case in all tags")

(defvar test-tags '()
  "All tags from all test cases")

(defun test-completing-read (prompt choices dummy require-match)
  (let ((iswitchb-make-buflist-hook
	 (lambda ()
	   (setq iswitchb-temp-buflist choices))))
    (iswitchb-read-buffer prompt nil require-match)))

(defvar test-completing-read-function
  (if (fboundp 'iswitchb-read-buffer) 'test-completing-read 'completing-read))

(defvar test-assert-method-prefix "test-assert-"
  "Prefix of test-assert methods.")

(defun test-assert-p (test)
  (let ((method-name (symbol-name (car test))))
    (string-equal
     test-assert-method-prefix
     (substring method-name
		0
		(min (length test-assert-method-prefix)
		     (length method-name))))))

(defun test-report-error (test error)
  (princ "#  ")
  (princ test)
  (princ "\n")
  (let* ((msg (error-message-string error))
	 (from-here (string-equal
		     "#    " (substring msg 0 5))))
    (unless from-here
      (princ "#    "))
    (princ msg)
    (unless from-here)
    (princ "\n"))
  (princ "#  \n"))

(defun test-run-and-print (test)
  (condition-case err
      (progn
	;; run
	(eval test)
	;; increase count of success if it's a test
	(when (test-assert-p test)
	  (incf succ)))
    ;; increase count of failure and report error
    (error (incf fail)
	   (test-report-error test err))))

(defmacro defcase (case-name tags setup &rest body)
  `(progn
     (if (and (listp ',tags)
	      (or (null ',tags)
		  (every 'symbolp ',tags)))
	 (progn
	   (put ',case-name 'tags ',tags)
	   (dolist (tag ',tags)
	     (when tag
	       (add-to-list 'test-tags tag)
	       (unless (boundp tag)
		 (set tag '()))
	       (add-to-list tag ',case-name))))
       (error "Tags must be nil or a list of symbols."))
     (put ',case-name 'cases
	  (lambda ()
	    (with-temp-buffer
	      (when ,setup
		(funcall ,setup))
	      (let ((fail 0)
		    (succ 0))
		(dolist (test ',body)
		  (test-run-and-print test))
		(put ',case-name 'succ succ)
		(put ',case-name 'fail fail)
		(princ (format "%s: %d pass, %d fail."
			       (symbol-name ',case-name)
			       succ fail))
		(princ "\n")))))
     (add-to-list 'test-cases ',case-name)))

(defun test-princ-current-time ()
  (princ "#  ")
  (princ (current-time-string))
  (princ "\n"))

(defmacro test-report (&rest body)
  `(progn
     (with-output-to-temp-buffer "*test-result*"
       (test-princ-current-time)
       ,@body)
     (with-current-buffer "*test-result*"
       (test-result-mode))))

(defun test-run (cases)
  (dolist (test-case (test-args-to-list cases))
    (funcall (get test-case 'cases))))

(defun test-summarize (cases)
  (let ((total-succ 0)
	(total-fail 0))
    (dolist (test-case cases)
      (incf total-succ (get test-case 'succ))
      (incf total-fail (get test-case 'fail)))
    (princ "#  ")
    (princ (format "Total: %d pass, %d fail." total-succ total-fail))))

(defun test-run-and-summarize (cases)
  (test-run cases)
  (test-summarize cases))

(defun test-args-to-list (args)
  (if (listp args)
      args
    (list args)))

(defun test-find-all-cases (tags)
  (let ((tag-list (test-args-to-list tags))
	(cases '()))
    (dolist (tag tag-list)
      (dolist (test-case (symbol-value tag))
	(add-to-list 'cases test-case)))
    cases))

(defun test-run-one-tag (tag-name)
  (interactive (list
		(intern
		 (funcall test-completing-read-function
			  "Tag: "
			  (mapcar 'symbol-name test-tags)
			  nil
			  t))))
  (test-report (princ "#  Tag: ") (princ tag-name) (princ "\n")
	       (test-run-and-summarize (test-find-all-cases tag-name))))

(defun test-run-tags (&rest tags)
  (test-report (princ "#  Tags: ")
	       (princ (mapconcat 'symbol-name tags " "))
	       (princ "\n")
	       (test-run-and-summarize (test-find-all-cases tags))))

(defun test-run-one-case (case-name)
  (test-report (test-run case-name)))

(defun test-run-all-cases ()
  (test-report (test-run-and-summarize test-cases)))

(defmacro test-motion-target (&rest body)
  `(progn
     ,@body
    (point)))

(defun test-assert-ok (form)
  (assert form))

(defun test-assert-compare (fn got expected)
  (assert (funcall fn got expected)
	  t
	  (with-output-to-string
	    (princ "#    Got: ")
	    (prin1 got)
	    (princ "\n")
	    (princ "#    Expected: ")
	    (prin1 expected))))

(defun test-assert-eq (got expected)
  (test-assert-compare 'eq got expected))

(defun test-assert-string-equal (got expected)
  (test-assert-compare 'string-equal got expected))

(defun test-assert-memq (object list)
  (assert (memq object list)
	  t
	  (with-output-to-string
	    (princ "#    ")
	    (princ object)
	    (princ " is not in ")
	    (princ list))))

;;; `test-result-mode'

(defvar test-result-font-lock-keywords
  `(("^#    .*$" . font-lock-warning-face)
    ;; be careful about the order
    ("^#  .*$" . font-lock-preprocessor-face)
    ("^\\(.*\\): \\([0-9]+\\) pass, \\([0-9]+\\) fail.$"
     (1 font-lock-function-name-face) (2 font-lock-type-face) (3 font-lock-warning-face))))

(defconst test-result-font-lock-defaults
  '(test-result-font-lock-keywords t nil nil nil (font-lock-multiline . nil)))

(define-derived-mode test-result-mode nil "Test-Result"
  (set (make-local-variable 'font-lock-defaults) test-result-font-lock-defaults))

(provide 'test)
