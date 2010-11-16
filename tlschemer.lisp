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
