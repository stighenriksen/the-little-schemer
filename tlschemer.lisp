(defun one? (a)
               (cond
                ((eq 1 a) t)
                (t nil)))


(defun atom? (x)
  (not (listp x)))

(defun lat? (x)
  (cond 
   ((null x) t)
   ((atom (car x)) (lat? (cdr x)))
   (t nil)))

(defun rember (x lat)
  (cond
   ((null x) (quote ()))
   ((eq (car lat) x) (cdr lat))
   (t (cons (car lat) (rember x (cdr lat))))))

(defun firsts (lat)
  (cond
   ((null lat) ())
   ((cons (car (car lat)) (cdr lat)))))

(defun insertR(new old lat)
	       (cond
		((null lat) ())
		((eq (car lat) old) (cons (car lat) (cons new (cdr lat))))

		(t (cons (car lat) (firstR new old (cdr lat))))))

(defun insertL(new old lat)
	       (cond
		((null lat) ())
		((eq old (car lat)) (cons new (cons (car lat) (insertL new old (cdr lat)))))
		(t (cons (car lat) (insertL new old (cdr lat))))))


(defun subst2 (new o1 o2 lat)
	       (cond 
		((null lat) ())
		((or (eq o1 (car lat)) (eq o2 (car lat))) (cons new (cdr lat)))
		(t (cons (car lat) (subst2 new o1 o2 (cdr lat))))))

(defun multirember (a lat)
	       (cond
		((null lat) ())
		((eq (car lat) a) (multirember a (cdr lat)))
		((cons (car lat) (multirember a (cdr lat))))))

(defun multiinsertR (new old lat)
	       (cond
		((null lat) ())
		((eq (car lat) old) (cons old (cons new (multiinsertR new old (cdr lat))))) 
		((cons (car lat) (multiinsertR new old (cdr lat))))))

(defun multisubst (old new lat)
		 (cond
		  ((null lat) ())
		  ((eq (car lat) old) (cons new (multisubst old new (cdr lat))))
		  (t (cons (car lat) (multisubst old new (cdr lat))))))

(defun multiinsertL (new old lat)
		(cond 
		 ((null lat) ())
		 ((eq old (car lat)) (cons new (cons old (multiinsertL new old (cdr lat)))))
		 (t (cons (car lat) (multinsertL new old (cdr lat))))))

(defun stigplus (num1 num2)
	       (cond
		((zerop num2) num1)
		((1+ (stigplus num1 (1- num2))))))

(defun stigminus (num1 num2)
	       (cond
		((zerop num2) num1)
		(t (1- (stigminus num1 (1- num2))))))

(defun addtup (tup)
	       (cond
		((null tup) 0)
		(t (stigplus (car tup) (addtup (cdr tup))))))

(defun x (n m)
		 (cond 
		  ((zerop m) 0)
		  ((stigplus n (x n (1- m))))))


(defun tup+ (tup1 tup2)
		 (cond
		  ((null tup1) tup2)
		  ((null tup2) tup1)
		  (t (cons (stigplus (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2))))))

(defun addtup (tup)
	       (cond
		((null tup) 0)
		(t (stigplus (car tup) (addtup (cdr tup))))))


(defun largerthan (n m)
		 (cond
		  ((zerop n) nil)
		  ((zerop m) t)
		  (t (lessthan (1- n) (1- m)))))

(defun largerthanbad (n m)
		 (cond
		  ((zerop n) nil)
		  ((zerop m) t)
		  (t (largerthanbad (1- n) (1- m)))))

(defun lessthan (n m)
		 (cond
		  ((zerop m) nil)
		  ((zerop n) t)
		  (t (lessthan (1- n) (1- m)))))

(defun equals (n m)
		 (cond
		  ((lessthan n m) nil)
		  ((largerthanbad n m) nil)
		  (t t )))

(defun power (n m)
		 (cond
		  ((zerop m) 1)
		  ((x n (power n (1- m))))))

(defun stiglength (lat)
		 (cond
		  ((null lat) 0)
		  ((1+ (stiglength (cdr lat))))))

(defun pick (n lat)
		 (cond 
		  ((null lat) nil)
		  ((equals 1 n) (car lat))
		  (t (pick (1- n) (cdr lat)))))


(defun rempick (n lat)
               (cond
                ((one? n) (cdr lat))
                (t (cons (car lat) (rempick (1- n) (cdr lat))))))

(defun no-nums (lat)
                 (cond
                 ((null lat) ())
                 ((numberp (car lat)) (no-nums (cdr lat)))
                 ((cons (car lat) (no-nums (cdr lat))))))

(defun all-nums (lat)
                 (cond 
                  ((null lat) ())
                  ((numberp (car lat)) (cons (car lat) (all-nums (cdr lat))))
                  (t (all-nums (cdr lat)))))

(defun eqan (a1 a2)
                 (cond
                  ((and (numberp  a1) (numberp a2)) (= a1 a2))
                  ((or (numberp a1) (numberp a2)) nil)
                  (t (eq a1 a2))))

(defun occur (a lat)
                 (cond
                  ((null lat) 0)
                  ((eq a (car lat)) (1+ (occur a (cdr lat))))
                  (t (occur a (cdr lat)))))

(defun rember* (a l)
               (cond
                ((null l) ())
                ((atom? (car l)) (cond
                                  ((eq (car l) a) (rember* a (cdr l)))
                                  (t (cons (car l) (rember* a (cdr l))))))
                (t (cons (rember* a (car l)) (rember* a (cdr l))))))

(defun insertR* (new old l)
               (cond
                ((null l) ())
                ((atom? (car l))
                 (cond
                  ((eq old (car l)) (cons old (cons new (insertR* new old (cdr l)))))
                  (t (cons (car l) (insertR* new old (cdr l))))))
                (t (cons (insertR* new old (car l)) (insertR* new old (cdr l))))))

(defun occur* (a l)
               (cond
                ((null l) 0)
                ((atom? (car l)) 
                 (cond
                  ((eq a (car l)) (1+ (occur* a (cdr l))))
                  (t (occur* a (cdr l)))))
                (t (+ (occur* a (car l)) (occur* a (cdr l))))))

(defun subst* (new old l)
               (cond
                ((null l) ())
                ((atom? (car l))
                 (cond
                  ((eq old (car l)) (cons new (subst* new old (cdr l))))
                  (t (cons (car l) (subst* new old (cdr l))))))
                (t (cons (subst* new old (car l)) (subst* new old (cdr l))))))

(defun insertL* (new old l)
               (cond 
                ((null l) ())
                ((atom? (car l))
                 (cond 
                  ((eq old (car l)) (cons new (cons old (insertL* new old (cdr l)))))
                  (t (cons (car l) (subst* new old (cdr l))))))
                (t (cons (insertL* new old (car l)) (insertL* new old (cdr l))))))

(defun member* (a l)
               (cond
                ((null l) nil)
                ((atom? (car l))
                 (or 
                  (eq a (car l))
                  (member* a (cdr l))))
                (t (or (member* a (car l)) (member* a (cdr l))))))

(defun leftmost (l)
               (cond
                ((atom? (car l)) (car l))
                (t (leftmost (car l)))))

(defun eqlist (l1 l2)
  (cond
   ((and (null l1) (null l2)) t)
   ((or (null l1) (null l2)) nil)
   ((and (equal (car l1) (car l2)) (eqlist (cdr l1) (cdr l2))))))

(defun equalsexpr (s1 s2)
  (cond
   ((and (atom? s1) (atom? s2)) (eq s1 s2))
   ((or (atom? s1) (atom? s2)) nil)
   ((eqlist s1 s2))))

(defun rember2 (s l)
               (cond
                ((null l) ())
                ((equal s (car l)) (cdr l))
                ((cons (car l) (rember2 s (cdr l))))))
 
(defun numbered? (aexp)
               (cond
                ((atom? aexp) (number? aexp))
                ((and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))))

(defun value (nexp)
                 (cond
                  ((atom? nexp) nexp )
                  ((eq (car (cdr nexp)) '+) (+ (value (car nexp)) (value (car (cdr (cdr nexp))))))
                  ((eq (car (cdr nexp)) '*) (* (value (car nexp)) (value (car (cdr (cdr nexp))))))
                  ((eq (car (cdr nexp)) '!) (power (value (car nexp)) (value (car (cdr (cdr nexp))))))))


(defun 1st-sub-exp (aexp)
                 (cond
                 ( t (car (cdr aexp)))))

(defun 2nd-sub-exp (aexp)
                 (cond
                  (t (car (cdr (cdr aexp))))))

(defun operator (aexp)
                 (cond
                  (t (car aexp))))

(defun value (nexp)
                 (cond
                  ((atom? nexp) nexp)
                  ((eq (operator nexp) '+) (+ (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
                  ((eq (operator nexp) '*) (* (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
                  ((eq (operator nexp) '!) (power (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))))

(defun sero? (n)
                 (cond
                  (t (null n))))

(defun edd1 (n)
                 (cond
                  (t (cons '() n))))

(defun zub1 (n)
                 (cond
                  (t (cdr n))))

(defun stigplus (n m)
                 (cond
                  ((sero? m) n)
                  ((cons (edd1 n) (stigplus n (cdr m))))))

(defun set? (lat)
  (cond
   ((null lat) t)
   ((member* (car lat) (cdr lat)) nil)
   (t (set? (cdr lat)))))

(defun makeset? (lat)
  (cond
   ((null lat) ())
   ((member* (car lat) (cdr lat)) (makeset? (cdr lat)))
   ((cons (car lat) (makeset? (cdr lat))))))

(defun makeset? (lat)
  (cond
   ((null lat) ())
   (t (cons (car lat) (makeset? (multirember (car lat) (cdr lat)))))))



(defun subset? (set1 set2)
               (cond
                ((null set1) t)
                ((and (member (car set1) set2) (subset? (cdr set1) set2)))))

(defun eqset? (set1 set2)
               (cond
                ((and (subset? set1 set2) (subset? set2 set1)))))

(defun intersect? (set1 set2)
               (cond
                ((null set1) nil)
                ((member (car set1) set2) t)
                ((intersect? (cdr set1) set2))))

(defun intersect (set1 set2)
               (cond
                ((null set1) ())
                ((member (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
                ((intersect (cdr set1) set2))))

(defun unionstig (set1 set2)
               (cond
                ((null set1) ())
                ((member (car set1) set2) (union (cdr set1) set2))
                ((cons (car set1) (union (cdr set1) set2)))))

(defun intersectall (l-set)
                 (cond
                  ((null (cdr l-set)) (car l-set))
                  ((intersect (car l-set) (intersectall (cdr l-set))))))

(defun a-pair? (x)
                (cond
                 ((atom? x) nil)
                 ((null x) nil)
                 ((null (cdr x)) nil)
                 ((null (cdr (cdr x))) t)))

(defun build (s1 s2) 
                (cond
                 ((cons s1 s2))))

(defun fun? (rel)
                (cond
                 ((set? (firsts rel)))))

(defun revrel (rel)
                (cond
                 ((null rel) ())
                 ((cons (build (second (car rel)) (first (car rel))) (revrel (cdr rel))))))

(defun seconds (s)
        (cond
         ((null s) ())
         ((cons (cdr (car s)) (seconds (cdr s))))))

(defun fullfun? (fun)
        (cond
         ((set? (seconds fun)))))

(defun one-to-one? (fun)
        (cond
         (fun? (revrel fun))))



(defun eq?-c (a)
                (function
                 (lambda (x)
                   (eq x a))))

 
(defun rember-f (test?)
        (function 
         (lambda (a l)
           (cond
           ((null l) ())
           ((funcall test? a (car l)) (cdr l))
           (t (cons (car l) (funcall (rember-f test?) a (cdr l))))))))


(defun insertL-f (test?)
        (function
         (lambda (new old l)
           (cond
            ((null l) ())
            ((funcall test? (car l) old) (cons new (cons old (cdr l))))
            (t (cons (car l) (funcall (insertL-f test?) new old (cdr l))))))))

(defun insertR-f (test?)
        (function
         (lambda (new old l)
           (cond
            ((null l) ())
            ((funcall test? old (car l)) (cons old (cons new (cdr l))))
            (t (cons (car l) (funcall (insertR-f test?) new old (cdr l))))))))

(defun seql (new old l)
       (cond
        ((cons new (cons old l)))))

(defun seqr (new old l)
       (cond
        ((cons old (cons new l)))))

(defun seqs (new old l)
        (cons new l))

(defun insert-g (seq)
        (function
         (lambda (new old l)
           (cond
            ((null l) ())
            ((eq old (car l)) (funcall seq new old (cdr l)))
            (t (cons (car l) (funcall (insert-g seq) new old (cdr l))))))))

(setq stigsubst (insert-g 'seqs))


(defun atom-to-function (x)
          (cond
           ((eq x '+) '+)
           ((eq x '*) '*)
           (t 'expt)))

(defun value2 (nexp)
       (cond
        ((atom? nexp) nexp)
        (t (funcall (atom-to-function (operator nexp)) (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))))

(defun multirember-f (test?)
       (lambda (a lat)
         (cond
          ((null lat) ())
          ((funcall test? a (car lat)) (funcall (multirember-f test?) a (cdr lat)))
          (t (cons (car lat) (funcall (multirember-f test?) a (cdr lat)))))))

(setq multirember-eq? (multirember-f 'eq))

(defun multirember-f-2 (test? lat)
        (cond
         ((null lat) ())
         ((funcall test? (car lat)) (multirember-f test? (cdr lat)))
         (t (cons (car lat) (multirember-f test? (cdr lat))))))

(defun multiinsertLR (new oldL oldR lat)
        (cond
         ((null lat) ())
         ((eq (car lat) oldL) (cons new (cons oldL (multiinsertLR new oldL oldR (cdr lat)))))
         ((eq (car lat) oldR) (cons oldR (cons new (multiinsertLR new oldL oldR (cdr lat)))))
         (t (cons (car lat) (multiinsertLR (new oldL oldR (cdr lat)))))))

(defun multiinsertLRandco (new oldL oldR lat col)
  (cond
   ((null lat) (col () 0 0) ())
   ((eq (car lat) oldL) (multiinsertLR new oldL oldR (cdr lat) 
                                                            (lambda (newlat leftins rightins)
                                                              (col (cons new (cons oldL newlat)) (+leftins 1) rightins))))
   ((eq (car lat) oldR) (cons oldR (cons new (multiinsertLR new oldL oldR (cdr lat)
                                                            (lambda (newlat leftins rightins)
                                                              (col (cons oldR (cons new newlat)) leftins (+rightins 1))))))
   (t (cons (car lat) (multiinsertLR (new oldL oldR (cdr lat))
                                     (lambda (newlat leftins rightins)
                                       (col (cons (car lat) newlat) leftins rightins))))))))

(defun even? (n)
       (= (* (/ n 2) 2) n))

(defun evens-only* (l)
  (cond
   ((null l) ())
   ((atom? (car l)) 
    (cond
     ((even? (car l)) (cons (car l) (evens-only? (cdr l))))
     (t (evens-only? (cdr l)))))
   (t (cons (evens-only* (car l)) (evens-only* (cdr l))))))

(defun evens-only*&co (l col)
  (cond
   ((null l) (col () 1 0))
   ((atom? (car l)) 
    (cond
     ((even? (car l)) (evens-only*&co (cdr l)
                                   (lambda (newlat prodeven sumodd)
                                     (col (cons (car l) newlat) (* (car lat) prodeven sumodd))))
     (t (evens-only*&co (cdr l) 
                     (lambda (newlat prodeven sumodd)
                       (col newlat prodeven (+ (car lat) sumodd))))))))
   (t (evens-only*&co (car l) (lambda (newlat prodeven sumodd)
                                (evens-only*&co (cdr l) (lambda (carnewlat carprodeven carsumodd)
                                                          (col (cons newlat carnewlat) (* prodeven carprodeven) (+sumodd carsumodd)))))))))

                      

(defun looking (a lat)
        (keep-looking a (pick 1 lat) lat))

(defun keep-looking (a sorn lat)
       (cond
        ((numberp sorn) (keep-looking a (pick sorn lat) lat))
        (t (eq sorn a))))


(defun shift (pair)
        (build (first (first pair))
               (build (second (first pair))
                      (second pair))))


(defun length* (pora)
        (cond
         ((atom? pora) 1)
         (t (+ (length* (first pora)) (length* (second pora))))))
