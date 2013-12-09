(load "~/quicklisp/setup.lisp")
(ql:quickload "gsll")

(defparameter *dat* ;; read in the images of the mnist digits
 (with-open-file (s "train-images-idx3-ubyte"
		    :element-type '(unsigned-byte 8))
   (let ((a (make-array (file-length s) :element-type '(unsigned-byte 8))))
     (read-sequence a s)
     (make-array (list 60000 (* 28 28)) :element-type '(unsigned-byte 8)
		 :displaced-to a
		 :displaced-index-offset 16))))

(defparameter *dat-small*
  ;; reduce size by a factor of f=2
  (destructuring-bind (n hw) (array-dimensions *dat*)
    (let* ((f 2)
	   (d2 (make-array (list n 28 28) :element-type '(unsigned-byte 8)
			   :displaced-to *dat*))
	   (a (make-array (list n (/ hw (* f f))) :element-type '(unsigned-byte 8)))
	   (hf (floor 28 f))
	   (wf (floor 28 f))
	   (a2 (make-array (list n hf wf)
			   :element-type '(unsigned-byte 8)
			   :displaced-to a)))
      (dotimes (i n)
	(dotimes (j hf)
	  (dotimes (k wf)
	   (setf (aref a2 i j k) (aref d2 i (* f j) (* f k))))))
      a))) 

(defun write-double (v s)
  (rletz ((a :double-float))
    (setf (pref a :double-float) v)
    (write-sequence (loop for i below 8 collect (ccl:paref a :unsigned-char i))
		    s)))

(defun write-int32 (v s)
  (rletz ((a :int))
    (setf (pref a :int) v)
    (write-sequence (loop for i below 4 collect (ccl:paref a :unsigned-char i))
		    s)))

(defun bla ()
 (let ((b (grid:cl-array (first *svd*)))) 
   (let* ((n-small 60000)
	  (hw-small 50)
	  (theta .5d0)
	  (perplexity 30d0)
	  (a (make-array (* n-small hw-small) :element-type 'double-float))
	  (a2 (make-array (list n-small hw-small) :element-type 'double-float
			  :displaced-to a)))
     (declare (type fixnum n-small hw-small)
	      (type double-float theta perplexity)
	      (type (simple-array double-float 1) a)
	      (type (array double-float 2) a2 b))
     (dotimes (j n-small)
       (dotimes (i hw-small)
	 (setf (aref a2 j i) (aref b j i))))
     (with-open-file (s "/dev/shm/data.dat" :direction :output
			     :if-exists :supersede :if-does-not-exist :create
			     :element-type '(unsigned-byte 8))
       (write-int32 n-small s)
       (write-int32 hw-small s)
       (write-double theta s)
       (write-double perplexity s)
       (defparameter *filepos* (file-position s)))
     (with-open-file (s "/dev/shm/data.dat" :direction :output
			:if-exists :append :if-does-not-exist :create
			:element-type 'double-float)
       (file-position s *filepos*)
       (write-sequence a s)
       nil))))

#+nil
(bla)

(defparameter *mapped-x*
 (progn 
   ;; output file contains 
   ;; n[1]:int32  
   ;; d[1]:int32 (should be 2)
   ;; data[n*d]:double,
   ;; landmark[n]:int32
   ;; costs[n]:double
   ;; the range is -1.7 .. 1.7
   (with-open-file (s "/dev/shm/result.dat" :element-type 'double-float)
     (file-position s (* 2 4)) ;; jump over the first two integers1
     (let* ((a2 (make-array (list 60000 2) :element-type 'double-float))
	   (a (make-array (* 60000 2) :element-type 'double-float
			  :displaced-to a2)))
       (read-sequence a s)
       (format t "max min ~a~%" (list (reduce #'max a)
				      (reduce #'min a)))
       a2))))

(defparameter *labels*
 (progn ;; read the labels
   ;; 72104149590690159734
   ;; 00000000  00 00 08 01 00 00 27 10  07 02 01 00 04 01 04 09  |......'.........|
   (with-open-file (s "train-labels-idx1-ubyte"
		      :element-type '(unsigned-byte 8))
     (file-position s 8)
     (let ((a (make-array 60000 :element-type '(unsigned-byte 8))))
      (read-sequence a s)
      a))))


(progn ;; plot the mapped 2d data
 (let* ((w 400) 
	(mi -30)
	(ma 30)
	(s (* (1- w) (/ (- ma mi))))
	(a (make-array (list w w 3) :element-type '(unsigned-byte 8)))
	(colors '((128 0 0) (255 0 0) (255 128 0) (255 255 0)
		  (255 255 128) (255 255 255)
		  (0 128 0) (0 255 0) (0 255 128) (0 255 255))))
   (destructuring-bind (n d) (array-dimensions *mapped-x*)
     (dotimes (i n)
       (let ((y (min (1- w) (max 0 (floor (* s (- (aref *mapped-x* i 0) mi))))))
	     (x (min (1- w) (max 0 (floor (* s (- (aref *mapped-x* i 1) mi))))))
	     (col (elt colors (aref *labels* i))))
	 (dotimes (c 3)
	   (setf (aref a y x c) (elt col c))))))
   (write-ppm "/dev/shm/mapped.ppm" a)))

;;pnmflip -r90 -tb o04.pgm


(defun write-pgm (fn a)
  (destructuring-bind (h w) (array-dimensions a) 
    (with-open-file (s fn :direction :output
		       :if-exists  :supersede
		       :if-does-not-exist :create)
      (format s "P5~%~a ~a~%255~%" w h))
    (with-open-file (s fn :direction :output
		       :if-exists  :append
		       :if-does-not-exist :create
		       :element-type '(unsigned-byte 8))
      (dotimes (i w)
	(dotimes (j h)
	 (write-byte (aref a j i) s))))))

(defun write-ppm (fn a)
  (destructuring-bind (h w c) (array-dimensions a) 
    (with-open-file (s fn :direction :output
		       :if-exists  :supersede
		       :if-does-not-exist :create)
      (format s "P6~%~a ~a~%255~%" w h))
    (with-open-file (s fn :direction :output
		       :if-exists  :append
		       :if-does-not-exist :create
		       :element-type '(unsigned-byte 8))
      (dotimes (i w)
	(dotimes (j h)
	  (dotimes (k c)
	   (write-byte (aref a j i k) s)))))))

(defun get-mnist-slice (j)
  (let* ((a (make-array '(28 28) :element-type '(unsigned-byte 8)))
	 (a1 (make-array (* 28 28) :element-type '(unsigned-byte 8)
			 :displaced-to a)))
    (dotimes (i (* 28 28))
      (setf (aref a1 i) (aref *dat* j i)))
    a))
(defun get-mnist-slice-v (v j)
  (let* ((f 1)
	 (wf (floor 28 f))
	 (a (make-array (list wf wf) :element-type 'double-float))
	 (a1 (make-array (* wf wf) :element-type 'double-float
			 :displaced-to a)))
    (dotimes (i (* wf wf))
      (setf (aref a1 i) (aref v  i j)))
    a))

#+nil
(destructuring-bind (n hw) (array-dimensions *v*)
  (dotimes (i 100) ;   loop for i from (- n 1) downto (- n 30) do
   (write-pgm (format nil "/dev/shm/uo~3,'0d.pgm" i)
	      (scale-ub8 (get-mnist-slice-v *v* i)))))


(defun get-mnist-slice-small (j)
  (let* ((f 2)
	 (wf (floor 28 f))
	 (a (make-array (list wf wf) :element-type '(unsigned-byte 8)))
	 (a1 (make-array (* wf wf) :element-type '(unsigned-byte 8)
			 :displaced-to a)))
    (dotimes (i (* wf wf))
      (setf (aref a1 i) (aref *dat-small* j i)))
    a))

#+nil
(dotimes (i 20)
 (write-pgm (format nil "/dev/shm/o~2,'0d.pgm" i) (get-mnist-slice i)))
#+nil
(dotimes (i 20)
 (write-pgm (format nil "/dev/shm/s~2,'0d.pgm" i) (get-mnist-slice-small i)))

(time
 (defparameter *dat0*
   (let ((a *dat*))
    (destructuring-bind (n hw) (array-dimensions a)
      (let* ((nsmall 60000)
	     (b (make-array (list nsmall hw) :element-type 'double-float)))
	(declare (type (simple-array double-float 2) b)
		 (type (array (unsigned-byte 8) 2) a))
	(dotimes (i nsmall)
	  (let ((avg (* 1d0 (loop for j below hw sum (aref a i j)))))
	    (declare (type fixnum i)
		     (type double-float avg))
	    (dotimes (j hw)
	      (setf (aref b i j) (- (aref a i j) avg)))))
	b)))))

(defun get-mnist-slice0 (j)
  (let* ((a (make-array '(28 28) :element-type 'double-float))
	 (a1 (make-array (* 28 28) :element-type 'double-float
			 :displaced-to a)))
    (dotimes (i (* 28 28))
      (setf (aref a1 i) (aref *dat0* j i)))
    a))

#+nil
(dotimes (i 20)
 (write-pgm (format nil "/dev/shm/z~2,'0d.pgm" i) (scale-ub8 (get-mnist-slice0 i))))


(defun scale-ub8 (cov &key (scale 1d0))
  (declare (type (array double-float 2) cov))
 (let* ((a (make-array (array-dimensions cov)
		       :element-type '(unsigned-byte 8)))
	(cov1 (make-array (reduce #'* (array-dimensions cov))
			  :element-type 'double-float
			  :displaced-to cov))
	(mi (reduce #'min cov1))
	(ma (reduce #'max cov1))
	(s (/ (* scale 255d0) (- ma mi))))
   (declare (type double-float mi ma)
	    (type (array double-float 1) cov1)
	    (type (simple-array (unsigned-byte 8) 2) a))
   (destructuring-bind (h w) (array-dimensions a)
     (declare (type fixnum h w))
     (dotimes (j h)
       (declare (type fixnum j))
       (dotimes (i w)
	 (declare (type fixnum i))
	 (setf (aref a j i) (min 255 (max 0 (floor (* s (- (aref cov j i) mi))))))))
     a)))


(defparameter *svd*
  (let ((a *dat0*))
   (destructuring-bind (n n2) (array-dimensions a)
     (let ((b (grid:make-foreign-array 'double-float
				       :dimensions (array-dimensions a))))
       (dotimes (i n2)
	 (dotimes (j n)
	   (setf (grid:aref b j i) (aref a j i))))
       (multiple-value-list 
	(gsll:sv-decomposition b))))))

(progn
 (defparameter *u* (grid:cl-array (first *svd*)))
 (write-pgm "/dev/shm/u.pgm" (scale-ub8 *u*)))

(progn
 (defparameter *v* (grid:cl-array (third *svd*)))
 (write-pgm "/dev/shm/v.pgm" (scale-ub8 *v*)))

(defparameter *s* (grid:cl-array (second *svd*)))

(let ((a *dat0*)) ;; find coefficients for the modes with highest singular value
 (destructuring-bind (n hw) (array-dimensions a)
   (declare (type fixnum n hw)
	    (type (array double-float 2) a))
   (defparameter *small-x*
     (let* ((d 400)
	    (c (make-array (list n d) :element-type 'double-float
			   :initial-element 0d0)))
       (declare (type fixnum d)
		(type (simple-array double-float 2) c))
       (dotimes (j n)
	 (declare (type fixnum j))
	 (dotimes (i d)
	   (declare (type fixnum i))
	   (dotimes (k d)
	     (declare (type fixnum k))
	     (incf (aref c j i) (* (aref a j k) (aref *v* k i))))))
       c))))


(defparameter *recon*
 (let ((a *small-x*)) ;; reconstruct using only most prominent modes
   (destructuring-bind (n hw-small) (array-dimensions a)
     (declare (type fixnum n hw)
	      (type (array double-float 2) a))
     (destructuring-bind (n2 hw) (array-dimensions *v*)
       (let* ((c (make-array (list n hw) :element-type 'double-float
			     :initial-element 0d0)))
	 (declare (type fixnum d)
		  (type (simple-array double-float 2) c))
	 (dotimes (j 10)
	   (declare (type fixnum j))
	   (dotimes (i hw)
	     (declare (type fixnum i))
	     (dotimes (k hw-small)
	       (declare (type fixnum k))
	       (incf (aref c j i) (* (aref a j k) (aref *v* i k))
		     ))))
	 c)))))

(defparameter *recon2*
 (let ((u (first *svd*))
       (v (third *svd*))
       (s (second *svd*))) 
   (destructuring-bind (n hw) (array-dimensions u)
     (declare (type fixnum n hw)
	      (type (array double-float 2) u s v))
     (let* ((c (make-array (list n hw) :element-type 'double-float
			   :initial-element 0d0))
	    (hw-small 200))
       (declare (type fixnum d)
		(type (simple-array double-float 2) c))
       (dotimes (j 100)
	 (declare (type fixnum j))
	 (dotimes (i hw)
	   (declare (type fixnum i))
	   (dotimes (k hw-small)
	     (declare (type fixnum k))
	     (incf (aref c j i) (* (aref u j k) (aref v i k))))))
       c))))
#+nil
(dotimes (i 100)
   (write-pgm (format nil "/dev/shm/recon~3,'0d.pgm" i)
	      (scale-ub8 (get-mnist-slice-recon i))))

(defun get-mnist-slice-recon (j)
  (let* ((f 1)
	 (wf (floor 28 f))
	 (a (make-array (list wf wf) :element-type 'double-float))
	 (a1 (make-array (* wf wf) :element-type 'double-float
			 :displaced-to a)))
    (dotimes (i wf)
      (dotimes (k wf)
       (setf (aref a i k) (aref *recon2*  j (+ (* wf k) i)))))
    a))

