;;;;Utility functions from onlisp
 
(mapcan #'nicknames people); this to give a list of people and return it as a list
 

;;towns - func that returns a list of nearby towns sorted from nearest to furthest
;;bookshops - func that returns a list of bookshops in the city
;;to find nearest town which has any bookshops, and the bookshops in that town we start with
 
(let ((town (find-if #'bookshops towns)));find-if rets first elem which bookshops returns a non-nil value
 (values town (bookshops town))
 
;;so bookshops runs a function and then the result is just wasted if it's an expensive process
;;then bookshops is again called on the town
 
(find-if #'(lambda (x) (if (> x 5) x nil))
         '(1 2 3  4 5 6 7 8 9 10))
 
;;if bookhops is expensive we have wasted a lot of time since bookshops gets called throws away the result and then bookshops is called again
 
(defun find-books (towns)
  (if (null towns)
      nil
      (let ((shops (bookshops (car towns))));;if the value is not nil it means we have saved the shops list
        (if shops
            (values (car towns) shops)
            (find-books (cdr towns))))))
 
 
;;in the future we want to do the same query again -- basially a "find-if" with a "some". something that
;;returns the successful element and the value returned by the test function
 
(defun find2 (fn lst)
  (if (null lst)
      nil
      (let ((val (funcall fn (car lst))))
        (if val
            (values (car lst) val)
            (find2 fn (cdr lst))))))
 
 
;;now if we use this we can achieve our original functionality like so
 
(find2 #'bookshops towns)
 
(mapcan #'nicknames people)
 
;;mapcan and find2 are general functions and we pass specific things to them
 
;;;;brevity is the soul of wit, and the essence of good software
 
(> (length x) (length y));with out utility this is bad
;but if we want to map a func over several lists
(mapcar fn (append x y z));;maybe we join the lists
;;once again a utility would have been useful
 
 
;;;;FIGURE 4.1
(proclaim '(inline last1 single append1 conc1 mklist))
 
;;look up last element
(defun last1 (lst)
  (car (last lst)))
 
;;see if list has one element - only need to look and see past one element so this is very efficient
(defun single (lst)
  (and (consp lst) (not (cdr lst)))) ;(= (length lst) 1) is super inefficient since we're traversing an entire list
 
;;appends to the end of a list - not destructive
(defun append1 (lst obj)
  (append lst (list obj))) ;;(append '(a b) 'c) => (A B . C)
 
;;destructively add eleemnt to end of a  list
(defun conc1 (lst obj)
  (nconc lst (list obj)))
 
;;make an object a list if it's not already one
(defun mklist (obj)
  (if (consp obj) obj (list obj)))
;;;;
 
 
 
 
;;say lookup is a function that single val or list of vals
;;say we want to collect the results of calling it
;;on a set of data
(mapcan #'(lambda (d) (mklist (lookup d)))
        data); see how we can return a list no matter what
;;even if it's a list we can return it, if not turn it into a list
 
 
 
;;;;FIGURE 4.2
;;traverse both lists at the same time and return true if x is longer
(defun longer (x y)
  (labels ((compare (x y)
             (and (consp x) ;nil if x is smaller than y
                  (or (null y); t if x is longer than y
                      (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
        (compare x y)
        (> (length x) (length y))))) ;the internal function only works for two lists but length could work for anything so we'll do a regular comparison of both objects aren't lists
 
;;like a find-if that doesn't stop at the first item it finds
(defun filter (fn lst)
  (let ((acc nil)) ;;make an accumulator
    (dolist (x lst)
      (let ((val (funcall fn x)));;get the value you want to apply to each item of the list
        (if val (push val acc))));;push that value to the accumulator
    (nreverse acc)));; get the order correct since it'll be
 
(defun filter1 (fn lst)
  (delete nil (mapcar fn lst)))
 
(defun filter2 (fn lst)
  (mapcan #'(lambda (x)
              (let ((val (funcall fn x)))
                (if val (list val))))
          lst))
 
 
(mapcar #'(lambda (x) (if (evenp x) x)) '(1 2 3 4)); (nil 2 nil 4)
(delete nil (mapcar #'(lambda (x) (if (evenp x) x)) '(1 2 3 4))); (2 4)
(filter2 #'(lambda (x) (if (evenp x) x)) '(1 2 3 4 5 6)); (2 4 6)
 
;;group lists into sublists - tail recursive
(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest
                        (cons (subseq source 0 n) acc)); will cons the first n elemeents on to a list
                   (nreverse (cons source acc)))))) ;;put remainder in a final sublist
    (if source (rec source nil) nil)))
 
;;original group - not tail recursive
(defun group1 (source n)
  (if (endp source) ;source could technally could be nil so we can't do a (null source) check here - example (2 nil nil 3 5 6)
      nil
      (let ((rest (nthcdr n source)))
        (cons (if (consp rest) (subseq source 0 n) source);;put either the subseq or the remainder to be consed
              (group1 rest n)))));will eventually be the base case (nil)
 
 
(defun flatten (lst)
  (labels ((rec (x acc)
             (cond
               ((null x) acc)
               ((atom x) (cons x acc))
               (t (rec (car x)
                       (rec (cdr x) acc))))))
    (rec lst nil)))
 
 
(defun rec1 (x acc)
  (cond
    ((null x) acc) ; return accumulator
    ((atom x) (cons x acc)) ;only add if it's the element is an atom
    (t (rec1 (car x)
             (cdr x))))) ;for testing changed this to just cdr x and watch how it fails
(defun flatten1 (lst)
  (rec1 lst nil))
 
 
(defun flatten2 (lst) 
  (mapcan #'(lambda (x) (if (atom x)
                            (mklist x) ;turn everything into a list if it's an atom
                            (flatten x)));go deeper and get each nested list - see how it calls itself again
          lst))
 
;;Prune is a remove if that recurses down to sublists
(defun prune (test tree)
  (labels ((rec (tree acc)
             (cond ((null tree) (nreverse acc))
                   ((consp (car tree));;if we need to go further
                    (rec (cdr tree)
                         (cons (rec (car tree) nil) acc)))
                   (t (rec (cdr tree);;if item is an atom
                           (if (funcall test (car tree))
                               acc ;don't add elem if it passes the test (ie it gets removed)
                               (cons (car tree) acc)))))))
    (rec tree nil)))
 
 
(defun prune1 (test tree)
  (if (atom tree)
      tree
      (mapcar #'(lambda (x)
                  (prune1 test x)); will grab all the args that are left behind from the remove-if
              (remove-if #'(lambda (y)
                             (and (atom y)
                                  (funcall test y)))
                         tree))))
 
(prune1 #'evenp '(1 (2) 3 (4 5) 6 (7 8 9) 10 (11 (12) 13) 14 15)); (1 NIL 3 (5) (7 9) (11 NIL 13) 15
 
;;remove if takes a list and calls the function for every atom in the list
(remove-if #'(lambda (y)
               (and (atom y)
                    (funcall #'evenp y)))
           '( 1 (2) 3 (4 5) 6 (7 8 9) 10 (11 (12) 13) 14 15)); (1 (2) 3 (4 5) (7 8 9) (11 (12) 13) 15) ;
 
;;see how mapcar takes (1 (2) 3 (4 5) (7 8 9) (11 (12) 13) 15)
(mapcar #'(lambda (x)(print x)) ;;mapcar will just grab all the args that are left behind and instead of printing it will call itself
        (remove-if #'(lambda (y)
                       (and (atom y)
                            (funcall #'evenp y)))
                   '( 1 (2) 3 (4 5) 6 (7 8 9) 10 (11 (12) 13) 14 15)))
 
;;look what happens if we only print the args that are lists
(mapcar #'(lambda (x)(if (consp x) (print x))) ;;mapcar will just grab all the args that are left behind and instead of printing it will call itself
        (remove-if #'(lambda (y)
                       (and (atom y)
                            (funcall #'evenp y)))
                   '( 1 (2) 3 (4 5) 6 (7 8 9) 10 (11 (12) 13) 14 15)))
 
;;see what happens when we take only the car
(mapcar #'(lambda (x)(if (consp x) (print (car x)))) ;;mapcar will just grab all the args that are left behind and instead of printing it will call itself
        (remove-if #'(lambda (y)
                       (and (atom y)
                            (funcall #'evenp y)))
                   '( 1 (2) 3 (4 5) 6 (7 8 9) 10 (11 (12) 13) 14 15)))
 
 
(defun prune1-tester (test tree)
  (if (atom tree)
      tree
      (mapcar #'(lambda (x)
                  (if (consp x) (prune1-tester test x)))
              (remove-if #'(lambda (y)
                             (and (atom y)
                                  (funcall test y)))
                         tree))))
 
(defun prune3 (test tree)
  (labels ((rec (tree acc)
             (cond ((null tree) acc ) ;if null return acc
                   ((consp (car tree)) ;if it's a list we need to traverse each leaf
                    (rec (cdr tree)
                         (cons (rec (car tree) nil) acc)))
                   (t ;if it's an atom
                    (rec (cdr tree) ;tree and an accumulator
                         (if (funcall test (car tree)) ;if it passes the test it gets pruned (we don't count it and just return the accumulator
                             acc
                             (cons (car tree) acc)))))))
    (nreverse (rec tree nil))))
 
(prune3 #'evenp '(1 2 (3 ((4 41) 5) 6) 7 8 (9) (10)))
 
 
;;;;Figure 4.4 which search lists
 
 
       
;;make before to see if an element came before another
(< (position 'b '(a b c d)) (position 'd '(a b c d))); will return true but a bad and lazy way to define before
 
(defun before (x y lst &key (test #'eql))
  (and lst
       (let ((first (car lst)))
         (cond ((funcall test y first) nil)
               ((funcall test x first) lst)
               (t (before x y (cdr lst) :test test))))))
 
(before 'a 'b '(z b c a d)); nil
(before 'b 'a '(z b c a d)); (B A D)
 
(defun after (x y lst &key (test #'eql))
 (let ((rest (before y x lst :test test)))
    (and rest (member x rest :test test)))) ;will return the cdr beginning with x ; the and macro will return the
 
(after 'a 'b '(z b c a d)); (A D)
(after 'b 'a '(z b c a d)); ;nil
 
(member 'a '( z b c a d)); (A D) ; so (member o l) returns the cdr beginning with o
 
;;cool way to use the above to see if we have a duplicate - see if the object is in the cdr of the list
(defun duplicate (obj lst &key (test #'eql))
  (member obj (cdr (member obj lst :test test))
          :test test))
 
(duplicate 'a '(c a d d a l l a c k)); (A L L A C K)
 
(defun split-if (fn lst)
  (let ((acc nil))
    (do ((src lst (cdr src))) ;variable - var initial update
        ((or (null src) (funcall fn (car src))); stopping condition
         (values (nreverse acc) src)) ;return values
      (push (car src) acc)))); what to do after each step of the accumulator
 
(defun split-if2 (fn lst)
  (let ((acc nil))
    (labels ((rec (lst acc)
               (cond ((null lst) acc)
                     ((funcall fn (car lst))
                      (values (nreverse acc) lst))
                     (t (rec (cdr lst) (push (car lst) acc))))))
      (rec lst acc))))
 
 
(split-if2 #'(lambda (x) (> x 4))
          '(1 2 3 4 5 6 7 8 9 10 11 12))
 
;;most compares elements against eaach other - list plus scoring function returns list with hightest score
(most #'length '((a b) (a b c) (a) (e f g h)))
 
(defun most (fn lst)
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
             (max (funcall fn wins)))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (when (> score max)
              (setq max score
                    wins obj))))
        (values wins max))))
 
;;takes a fn and lst and and function must be a predicate of 2 args, returns the element  which according to the predicate, beats all others
(defun best (fn lst)
  (if (null lst)
      nil
      (let ((wins (car lst)))
        (dolist (obj (cdr lst))
          (if (funcall fn obj wins)
              (setq wins obj)))
        wins)))
 
(best #'(lambda (x y) (if (< x y) nil)) '(5 6 17 2 1)); see how that messes up since it has 5 args
(best #'< '(5 6 17 2 1)); see how that doesn't mess up becuase of the function <
 
;;takes fn and a list for which the function yields the highest score (along with the score itself)
(defun mostn (fn lst)
  (if (null lst)
      (values nil nil)
      (let ((result (list (car lst)))
            (max (funcall fn (car lst))))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (cond ((> score max)
                   (setq max    score
                         result (list obj)))
                  ((eql score max)
                   (push obj result)))))
      (values (nreverse result) max))))
       
(mostn #'length '((a b) (a b c) (a) (e f g))); ((A B C) (E F G)) 3 - so list of results and 3
 
 
(defun map0-n (fn n)
  (mapa-b fn 0 n))
 
(defun map1-n (fn n)
  (mapa-b fn 1 n))
 
;;works with integers
(defun mapa-b (fn a b &optional (step 1))
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (nreverse result))
    (push (funcall fn i) result)))
 
;;works with sequences of objects of any kind
(defun map-> (fn start test-fn succ-fn)
  (do ((i start (funcall succ-fn i)) ;the successors are the fourth function
       (result nil))
      ((funcall test-fn i) (nreverse result))
    (push (funcall fn i) result)))
 
 
;;the nconc makes this destrutive
(defun our-mapcan (fn &rest lsts)
  (apply #'nconc (apply #'mapcar fn lsts)));;the defualt mapcan is destructive
 
;;so here's a non-destrutive version
(defun mappend (fn &rest lsts)
  (apply #'append (apply #'mapcar fn lsts)))
 
;;see how there's no consing if we want to like square root multiple lsts
(defun mapcars (fn &rest lsts)
  (let ((result nil))
    (dolist (lst lsts)
      (dolist (obj lst)
        (push (funcall fn obj) result)))
    (nreverse result)))
 
;;;mapcar for trees - recursive mapcar
;;notice the &rest args will turn even 'a into '(a) and '(1) to '((1))
(defun rmapcar (fn &rest args)
  (if (some #'atom args)
      (apply fn args) ;return something
      (apply #'mapcar
             #'(lambda (&rest args)
                 (apply #'rmapcar fn args)) ;go deeper
             args)))
 
 
;;;we can define mapa-b this way as well
;;;see how it seq begins with the object in the second arg.
;;the end is defined by the sequence given in the third
(defun mapa-b-2 (fn a b &optional (step 1))
  (map-> fn
         a
         (lambda (x) (> x b))
         (lambda (x) (+ x step)))) ; the successors are the fourth function
 
;;use
(map0-n #'1+ 20)
(map1-n #'1+ 20)
(mapa-b #'1+ -2 0 .5)
(mapa-b-2 #'1+ -2 0 .3)
(mappend #'+ '(3)'(1))
(mapcars #'sqrt '(1 2 3) '(3 54 5) '(10 231 100))
(mapcar #'sqrt (append '(1 2 3) '(3 54 5) '(10 231 100)))
(apply #'mapcar #'+ '((1 2) (3 4) (5 6) ))
(rmapcar #'(lambda (x) (+ 100 x))  '( 1 2 (3 4 5 (6)) 7 (8 (9) 10)))
 
 
;;want users to type without paraenthesis
(defun readlist (&rest args)
  (values (read-from-string ;so it only returns one value
           (concatenate 'string "("
                                (apply #'read-line args)
                                ")"))))
 
 
;;(readlist) type some arguments like this "or even this"
 
(defun prompt (&rest args)
  (apply #'format *query-io* args)
  (read *query-io*))
 
;(prompt "enter a number between ~a and ~a. ~%>> " 1 10)
(defun break-loop (fn quit &rest args)
  (format *query-io* "Entering break-loop. ~%")
  (loop
       (let ((in (apply #'prompt args)))
         (if (funcall quit in)
             (return)
             (format *query-io* "~a~%" (funcall fn in))))))
 
;;can simulate an actual top level
(break-loop #'eval #'(lambda (x) (eq x :q)) ">> ")
 
(defun rmapcar1 (fn &rest args)
  (progn
    (print "The args at the start of function")
    (princ args)
    (if (some #'atom args)
        (progn
          (print "some atoms")
          (princ args)
          (apply fn args)) ;(apply #'(lambda (x) (+ 100 x)) '(1))       
        (progn
          (print "NIL SOME ATOM - RETURNS NIL")
          (princ args)                
          (apply #'mapcar
                 #'(lambda (&rest args)
                     (progn
                       (print "before calling apply with args - in the apply loop")
                       (princ args)
                     (apply #'rmapcar1 fn args); (apply #'rmapcar1 #'(lambda (x) (+ 100 x)) '(1)
                     (format t "~%after calling apply with args ~a~%" args)))
                 args)))))
 
;;call during the call this is what happens
(apply #'mapcar #'(lambda (x) (print x)) '((1 2 (3 (4) 5) 6)))
(apply #'mapcar #'(lambda (x) (print x)) '((3 (4) 5)))
(apply #'mapcar #'(lambda (x) (print x)) '((4)))
 
 
(apply #'mapcar #'(lambda (x) (print x)) '((1 2 (3 4) 5)))
(mapcar #'(lambda (x) (print x)) '((1 2 (3 4) 5)))
(mapcar #'(lambda (x) (print x)) '(1 2 (3 4) 5))
 
 
;;;4.7 Symbols and strings
;;;Figure 4.8
(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))
 
(mkstr pi " pieces of " 'pi " and this is e " e)
 
(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))
 
(symb 'ar "Madi" #\L #\L 0 #\space 's)
 
(let ((s (symb '(a b))))
  (and (eq s '|(A B)|) (eq s '\(A\ B\))))
 
(defun re-read (&rest args)
  (values (read-from-string (apply #'mkstr args))))
 
(defun explode (sym)
  (map 'list #'(lambda (c)
                 (intern (make-string 1
                                      :initial-element c)))
             (symbol-name sym)))
 
(explode 'bomb)
(symbol-name 'bomb); "bomb"
(intern (make-string 1 :initial-element #\c))
 
 
 
;;fn1 is the car of (reverse fns)
;;rest gets binded to the cdr of (reverse fns)
(defun compose (&rest fns)
  (destructuring-bind (fn1 . rest) (reverse fns) ;(let ((fn1 (car (reverse fns))) (rest (cdr (reverse fns)))) ... rest of function)
    #'(lambda (&rest args)
        (reduce #'(lambda (v f) (funcall f v)) ;the other function takes a
                rest
                :initial-value (apply fn1 args))))) ;first function can take multiple arguments
 
;use
(mapcar (compose #'list #'round #'sqrt)
        '(4 9 16 25))
 
 
;how it works
(reduce #'(lambda (v f) (funcall f v))
                 '(sqrt round list)
                :initial-value 9)
;where the initial value of 9 comes from
(reduce #'(lambda (v f) (funcall f v))
                 '(round list)
                 :initial-value (apply #'sqrt '(9))); will be '(9) instead of just 9 because of the &rest args
 
(mapcar #'(lambda (x)
            (reduce #'(lambda (v f) (funcall f v))
                    '(round list)
                    :initial-value (apply #'sqrt x)))
        '((4) (9))); see how that works
 
(mapcar #'(lambda (&rest x)
            (reduce #'(lambda (v f) (funcall f v))
                    '(round list)
                    :initial-value (apply #'sqrt x)))
        '(4 9 48)); see how that works because of the &rest arg at the top
 
(reduce #'(lambda (v f) (format t "f=~a v=~a ~%" f v))
                 '(#'round #'list)
                 :initial-value 2)
 
(reduce #'list '(1 2 3 5 6) :initial-value nil :from-end t)
(reduce #'list '(1 2 3 5 6) :initial-value nil)
 
 
 
;;;;chapter 5 returning functions!
(remove-if #'evenp '(1 2 3 4 5 5 6 7 8)) ;removes all the ones that satisfy the predicate function
(remove-if (complement #'evenp) '(1 2 3 4 5 6 7 8 9));like a remove-if-not
 
;;complement is a function that returns a function
(defun our-complement (fn)
  #'(lambda (&rest args) (not (apply fn args))))
 
;;this works with dynmaic or lexical scope
(defun joiner1 (obj)
  (typecase obj
    (cons #'append)
    (number #'+)))
 
(defun join1 (&rest args)
  (apply (joiner1 (car args)) args))
 
(join1 1 2 3); 6
(join1 '(1 2 3) '(4 6)); '(1 2 3 4 5 6)
 
;;so remove and remove-if-not are complements and the complement function shows that we
;;we don't need 2 sets of functions
;;well append and nconc are the same but nconc is destructive - same goes for remove-if and delete-if where delete-if is destuctive
 
;;;;5.1 returning destructive equivalents
(defvar *!equivs* (make-hash-table)) ;;will be global hash that maps functions to their destuctive equivalents
 
;;the ! (bang) is from scheme and returns the destructive counterpart from the hash
(defun ! (fn)
  (or (gethash fn *!equivs*) fn))
 
;;sets destuctive equivalent
(defun def! (fn fn!)
  (setf (gethash fn *!equivs*) fn!))
 
 
;;set the functions
(def! #'remove-if #'delete-if)
(def! #'append #'nconc)
 
(defvar *lst* '(1 2 3 4 5 6))
(funcall (! #'remove-if) #'oddp *lst*)
 
;;closure containing a hash-table in which to store results of previous calls
(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
        (multiple-value-bind (val win) (gethash args cache)
          (if win
              val
              (setf (gethash args cache)
                    (apply fn args)))))))
 
;;with memoized functions, repeated calls are just hash table lookups
;;if we expect to make the same call more than once then then each time it's about to be called
;;just check the hash to see if it's already been called
(setq slowid (memoize #'(lambda (x) (sleep 5) x)))
 
(time (funcall slowid 1)) ;Elapsed time = 5.0000 seconds
(time (funcall slowid 1)) ;Elapsed time = 0.0000 seconds
 
 
;;if f and g are functions;
;then f of g of x is also a function. like saying  f(g(x)) equals FoG(x) equals F of g of x is a function
;;takes any number of functions and returns their composition
(defun compose (&rest fns)
  (if fns
      (let ((fns1 (car (last fns)))
            (fns  (butlast fns)))
        #'(lambda (&rest args)
            (reduce #'funcall fns
                    :from-end t
                    :initial-value (apply fns1 args))))
      #'identity))
 
(funcall (compose #'list #'1+) 3)
(funcall #'(lambda (x) (list (1+ x))) 3)
 
(funcall (compose #'1+ #'find-if) #'oddp '(2 34 4)); errors out since nil
(funcall (compose #'1+ #'find-if) #'oddp '(2 33 4 5)); doesn't error out
 
(apply #'find-if #'oddp '((2 33 4 15))); apply takes a function and the last arg as a list
(list (1+ (apply #'find-if #'oddp '((2 33 4 15))))); (34)
 
(defun our-complement1 (pred)
  (compose #'not pred))
 
(remove-if (our-complement1 #'evenp) '(1 2 3 4 5 6 7 8 9 10)); (2 4 6 8 10)
 
;all functions given as args to compose must be functions of one arg except the last one
;;on the last function there are no restrictions and whatever args it takes
 
 
(defun fif (if then &optional else)
  #'(lambda (x)
      (if (funcall if x)
          (funcall then x)
          (if else (funcall else x)))))
 
;;see this is a way to combine functions other than composing them
(mapcar #'(lambda (x)
            (if (slave x)
                (owner x)
                (employer x)))
        people)
 
(mapcar (fif #'slave #'owner #'employer)
        people)
 
;;function intersection
(defun fint (fn &rest fns)
  (if (null fns)
      fn
      (let ((chain (apply #'fint fns)))
        #'(lambda (x)
            (and (funcall fn x) (funcall chain x))))))
 
;;how to use it
(find-if #'(lambda (x)
             (and (signed x) (sealed x) (delivered x)))
         docs)
 
(find-if (fint #'signed #'sealed #'delivered) docs)
 
;;function union
(defun fun (fn &rest fns)
  (if (null fn)
      fn
      (let ((chain (apply #'fun fns)))
        #'(lambda (x)
            (or (funcall fn x) (funcall chain x))))))
 
;;use
(find-if #'(lambda (x)
             (or (signed x) (guardian-sig x) (grandparent-sig x)))
         docs)
 
(find-if (fun #'signed #'guardian-sig #'grandparent-sig) docs)
 
;;will always be in this form
(apply #'(lambda (fn &rest fns) (print fn)(print fns)) '(#'a #'b #'c))
(apply #'(lambda (fn &rest fns) (print fn)(print fns)) '(a b c)); args that get taken A (B C)
 
;;;;5.5 Recusion on cdrs
 
;;see how the pattern is the same in both
;;base case and some function that deals with the cdr of the list, and eval the same expression
;;on each step
(defun our-length (lst)
  (if (null lst)
      0
      (1+ (our-length (cdr lst)))))
 
(defun our-every (fn lst)
  (if (null lst)
      t
      (and (funcall fn (car lst))
           (our-every fn (cdr lst)))))
 
;;list recurser - generate most functions that recurse on successive cdrs of lists
;;lrec's first arg is is a function that
;; - 1) the current car of the list
;; - 2) something to recurse over (function that can be called to continue the recursion
;;lrec's second arg is the base case (obvious)
(defun lrec (rec &optional base)
  (labels ((self (lst)
             (if (null lst)
                 (if (functionp base)
                     (funcall base)
                     base)
                 (funcall rec (car lst) ;;rec takes the car of the list
                              #'(lambda ()
                                  (self (cdr lst))))))) ;;rec takes a function that recurses
    #'self))
 
;;so our length
(lrec #'(lambda (x f) (1+ (funcall f))) 0) ;the car of the list is always ignored x is always ignored
(funcall (lrec #'(lambda (x f) (1+ (funcall f))) 0)
         '(1 2 3 4)); returns 4
;;our every
(lrec #'(lambda (x f) (and (oddp x) (funcall f))) t)
(funcall (lrec #'(lambda (x f) (and (oddp x) (funcall f))) t) '(1 3 5 7 9 1))

;;a shitty version of our-member
(funcall (lrec #'(lambda (x f)
		   (if (eql 'a x)
		       'a ;really hard to return rest of the list 
		       (funcall f)))
	       nil)
	 '(b c a d))

;;see the patern is close but it's hard to return the list
(defun our-member (obj lst)
  (if (null lst)
      nil
      (if (eql obj (car lst))
	  lst ;really hard to return 
	  (our-member obj (cdr lst)))))

;;an example of copy list
(defun our-copy-list (lst)
  (if (atom lst)
      lst
      (cons (car lst) ;assumes that it's single list and not nested (nested == tree)
	    (our-copy-list (cdr lst)))))

;;with lrec - our copy list
(funcall (lrec #'(lambda (x f) (cons x (funcall f)))) '(1 2 3))

;;remove duplicates 
(funcall (lrec #'(lambda (x f) (adjoin x (funcall f)))) '( a b a c a d a a b b a a a e))

;adjoin only adds it if it isn't in the list very cool way to do it
(adjoin 'a '(a b c)); (A B C)
(adjoin 'a '(b c d)); (A B C D)

;find-if - for some function fn. like oddp 
(lrec #'(lambda (x v) (if (fn x) x (funcall f))))

;some - for some functon fn. like oddp 
(lrec #'(lambda (x f) (or (fn x) (funcall f))))
