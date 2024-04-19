;;; Shortened syntax for lambda functions inspired by Clojure.
;;; Examples:
;;; (lfn (+ % %))         => (lambda (%1) (+ %1 %1))
;;; (lfn (+ %2 (* 3 %4))) => (lambda (%1 %2 %3 %4) (+ %2 (* 3 %4)))
;;; (lfn (+ 2 3))         => (lambda () (+ 2 3))

;;; Known issues:
;;; 1. Does not work well with nested lfn macros.
;;; 2. May behave weirdly when using `%' function. For example, this works:
;;;      (funcall (lfn (% % 2)) 3) => 1
;;;    But this doesn't:
;;;      (funcall (lfn (% 3 2))) => ERROR!

;;; Both of those are also present in Clojure, so I probably won't fix them here.

(defconst lfn--percentarg-pattern
  "^%[0-9]*$"
  "Regexp to match percentage-encoded arguments.")

(defun lfn--percentarg-num (sym)
  "Return position of a percentage-encoded argument reference SYM.

(lfn--percentarg-num '%42) => 42
(lfn--percentarg-num '%) => 1
(lfn--percentarg-num 'abc) => nil"
  (let ((name (symbol-name sym)))
    (when (string-match-p lfn--percentarg-pattern name)
      (let ((arity (string-to-number (substring name 1))))
        (if (zerop arity)
            1 ;; equate % to %1
          arity)))))

(defun lfn--percent-func-ref-p (obj)
  "Return t if OBJ is a reference to function starting with `%'"
  (and (eq (car obj) 'function)
       (symbolp (cadr obj))
       (string-match-p lfn--percentarg-pattern (symbol-name (cadr obj)))))

(defun lfn--quoted-p (obj)
  "Return t if OBJ is a quoted form."
  (eq (car obj) 'quote))

(defun lfn--backquoted-p (obj)
  "Return t if OBJ is a backquoted form."
  (eq (car obj) '\`))

(defun lfn--escaped-p (obj)
  "Return t if OBJ is a backquote-escaped form."
  (eq (car obj) '\,))

(defun lfn--arity-backquoted (sexp)
  "Return the number of percentage-encoded argument references in backquoted SEXP."
  (cond ((not (consp sexp)) 0)
        ((lfn--escaped-p sexp) (lfn--arity (cdr sexp)))
        (t (apply #'max (mapcar #'lfn--arity-backquoted sexp)))))

(defun lfn--arity (sexp)
  "Return the number of percentage-encoded argument references in SEXP."
  (let ((this-arity (and (symbolp sexp)
                         (lfn--percentarg-num sexp))))
    (cond (this-arity this-arity)
          ((and (consp sexp)
                (not (lfn--quoted-p sexp)) ;;ignore quoted forms
                (not (lfn--percent-func-ref-p sexp))) ;;ignore function references to `%'
           (let ((arity-fn (if (lfn--backquoted-p sexp) #'lfn--arity-backquoted #'lfn--arity)))
             (apply #'max (mapcar arity-fn sexp))))
          (t 0))))

;;;###autoload
(defmacro lfn (&rest body)
  "Shortened syntax for lambda functions inspired by Clojure.

(lfn (+ % %)) => (lambda (%1) (+ %1 %1))
(lfn (+ %2 (* 3 %4))) => (lambda (%1 %2 %3 %4) (+ %2 (* 3 %4)))
(lfn (+ 2 3)) => (lambda () (+ 2 3))
"
  (let ((arity (lfn--arity body)))
    (if (zerop arity)
        `(lambda () ,@body)
      `(lambda ,(mapcar (lambda (i) (intern (format "%%%d" i)))
                        (number-sequence 1 arity))
         (let ((% %1))
           ,@body)))))

(provide 'lfn)
