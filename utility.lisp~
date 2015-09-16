;;;;Utility

;;;more important why we do this then how we do this

(defun mklist (x)
  (if (listp x) x (list x)))

(defun longer (x y)
  (labels ((compare (x y)
	     (and (consp x);if t then x is smaller
		  (or (null y);if t then y is smaller
		      (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
	(compare x y)
	(> (length x) (length y)))))

(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
	(if val (cons val lst))))
    (nreverse acc)))

(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
	     (let ((rest (nthcdr n source)))
	       (if (consp rest)
		   (rec rest (cons (subseq source 0 n) acc));step and work - and add it to acc
		   (nreverse (cons source acc))))));base- add the remainder and return the acc
    (if source (rec source nil) nil)))

;;;;Figure 4.3
(defun flatten (x)
  (labels ((rec (x acc)
	     (cond ((null x) acc);;base case
		   ((atom x) (cons x acc));do something 
		   (t (rec (car x)
			   (rec (cdr x) acc))))));step and do something
    (rec x nil)))




;;;;how to get to each stage
;;error note the failure - mapcan already picks the car of each element - and we call car again it messes up 
(defun flatten1 (lst)
  (mapcan #'(lambda (x)
	      (if (atom x) (mklist x) (flatten1 (car x)))) ;;fails because we call the car of the list (flatten a) instead of (flatten (a))
	  lst))

;;step 2 - but still slow
(defun flatten2 (x) 
  (mapcan #'(lambda (x)
	      (if (atom x) (mklist x) (flatten2 x))) ;;it's already car so we don't need to do it again
	    x))

;;make it faster - final version of flatten
(defun flatten3 (lst)
  (labels ((rec (x acc)
	     (cond ((null x) acc) ;don't have to wrry about '(nil (a b) c) since mapcan will take care of going through each element
		   ((atom x) (cons x acc))
		   (t (rec (car x)
			   (rec (cdr x) acc))))))
    (rec lst nil)))


(defun filter1 (fn lst)
  (delete nil (mapcar fn lst)))

(delete 2 (mapcar #'(lambda (x) (if (evenp x) x))
		'(1 2 3 4 5 6 7))); (nil nil 4 nil 6 nil) see how the 2 was deleted and how we can just delete the nil values (but this is 2 passes so there is a better solution

;using mapcan and switch it to mapcar
(defun filter2 (fn lst)
  (mapcar #'(lambda (x)
	      (let ((val (funcall fn x)))
		(if val val)))
	  lst))

(filter2 #'(lambda (x) (if (evenp x) x)) '(1 2 3 4 5 6 8))


(defun prune1 (test tree)
  (if (atom tree)
      tree;will fail if we try to pass tree to mapcar and it's NOT a list
      (mapcar #'(lambda (x)
		  (if (consp x) (prune1 test x)))
	      (remove-if #'(lambda (y)
			     (and (atom y)
				  (funcall test y)))
			 tree))))
		      
