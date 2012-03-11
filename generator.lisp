(defvar *outer-width* 3)
(defvar *type* "scalar_t")

(defun bit-reverse (x &optional (width *outer-width*))
  (let ((acc 0)
        (bit (ash 1 width)))
    (dotimes (i width acc)
      (setf bit (ash bit -1))
      (when (logbitp i x)
        (setf acc (logior acc bit))))))

(defun half-bit-reverse (x &optional (width *outer-width*))
  (let ((acc 0)
        (bit (ash 1 (* 2 width))))
    (dotimes (i width acc)
      (setf bit (ash bit -1))
      (when (logbitp i x)
        (setf acc (logior acc bit))))))

(defun swizzle (x y &optional (width *outer-width*))
  (let ((acc 0)
        (bit 1))
    (dotimes (i width acc)
      (when (logbitp i x)
        (setf acc (logior acc bit)))
      (setf bit (* 2 bit))
      (when (logbitp i y)
        (setf acc (logior acc bit)))
      (setf bit (* 2 bit)))))

(defun z-order-pairs (pairs &optional (width *outer-width*))
  (sort pairs #'<
        :key (lambda (pair)
               (swizzle (car pair)
                        (cdr pair)
                        width))))

(defun reversed-variables (&optional (width *outer-width*))
  (let* ((n (ash 1 width))
         (vars (make-array n)))
    (dotimes (i n vars)
      (setf (aref vars i) (format nil "top~A" i)))))

(defun %emit-inner-swap (&optional (prefix (string #\Tab))
                           (width *outer-width*))
  (let ((pairs (z-order-pairs
                (let ((acc '())
                      (n    (ash 1 width)))
                  (dotimes (i n acc)
                    (dotimes (j n)
                      (push (cons i j) acc))))
                width))
        (rev-vars (reversed-variables width)))
    (with-output-to-string (s)
      (loop for (i . j) in pairs do
        (let ((ri (aref rev-vars (bit-reverse i)))
              (rj (aref rev-vars (bit-reverse j))))
          (format s "~ASWAP2(~A+~A*sizeof(~A), ~A+~A*sizeof(~A));~%"
                  prefix
                  rj i *type*
                  ri j *type*))))))

(defun %emit-inner-swap-same (&optional (prefix (string #\Tab))
                                (width *outer-width*))
  (let ((pairs (z-order-pairs
                (let ((acc '())
                      (n    (ash 1 width)))
                  (dotimes (i n acc)
                    (dotimes (j n)
                      (push (cons i j) acc))))
                width))
        (rev-vars (reversed-variables width)))
    (with-output-to-string (s)
      (loop
        for (i . j) in pairs
        when (< i j) do
          (let ((ri (aref rev-vars (bit-reverse i)))
                (rj (aref rev-vars (bit-reverse j))))
            (format s "~ASWAP1(~A+~A*sizeof(~A), ~A+~A*sizeof(~A));~%"
                    prefix
                    rj i *type*
                    ri j *type*))))))

(defun emit-generic-swap-function (name &optional (width *outer-width*))
  (let ((vars (reversed-variables width)))
    (with-output-to-string (s)
      (format s "~
void ~A (struct plan * plan, ~A * base)
\{
"
              name *type*)
      (loop for r across vars
            for i upfrom 0
            do (format s "~Cconst size_t ~A = plan->top[~A];~%"
                       #\Tab r i))
      (format s "
	const struct pair * pairs = plan->pairs;
	for (size_t i = plan->npairs; i --> 0; ) {
		size_t dx = pairs[i].x;
		size_t dy = pairs[i].y;
		if (dx != dy) {
			~A * x = (~A*)((char*)base+dx);
			~A * y = (~A*)((char*)base+dy);
~A		} else {
			~A * x = (~A*)((char*)x_base+dx);
~A		}
	}~%"
              *type* *type*
              *type* *type*
              (%emit-inner-swap (make-string 3 :initial-element #\Tab)
                                width)
              *type* *type*
              (%emit-inner-swap-same (make-string 3 :initial-element #\Tab)
                                width))
      (format s "~
\}~%"))))
