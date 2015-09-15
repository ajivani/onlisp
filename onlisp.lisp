;;;;ON lisp


;;tail recursion - val of recursive call immediately returned
(defun triangle (n)
  (labels ((tri (c n)
	     (declare (type fixnum n c))
	     (if (zerop n)
		 c
		 (tri (the fixnum (+ n c));bindng and tail recursion
		      (the fixnum (- n 1))))))
    (tri 0 n)))

;;;2.9 complation
(defun foo (x) (1+ x))

(compiled-function-p #'foo)

(compile 'foo)

(progn (compile 'bar '(lambda (x) (* x 3)))
       (compiled-function-p #'bar))
(bar 2); 6

(defun bad-reverse (lst)
  (let* ((len (length lst))
	 (ilimit (truncate (/ len 2))))
    (do ((i 0  (1+ i))
	 (j (1- len) (1- j)))
	((>= i ilimit))
      (rotatef (nth i lst) (nth j lst)))))

(setq lst '(a b c))
(bad-reverse lst); changes lst see how it's reversed

(setq lst '(a b c))

(defun good-reverse (lst)
  (labels ((rev (lst acc)
	     (if (null lst)
		 acc
		 (rev (cdr lst) (cons (car lst) acc)))))
    (rev lst nil)))

;;now when we call it see how it creats a list to return so it has no side effects
(good-reverse lst); rets a reversed lst
;;lst will return the orig list unchanged

(truncate 23.12131)
(= (truncate 123.138483) 123)

(multiple-value-bind (int frac) (truncate 26.21875)
  (list int frac))

(defun powers (x)
  (values x (sqrt x) (expt x 2)))

(multiple-value-bind (base root square) (powers 4)
  (list base root square))
