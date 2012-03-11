(defvar *outer-width* 3)
(defvar *small-width-max* (* 2 (1+ *outer-width*)))
(defvar *medium-width-max* 24)

(defun bit-reverse (x &optional (width *outer-width*))
  (let ((acc 0)
        (bit (ash 1 width)))
    (dotimes (i width acc)
      (setf bit (ash bit -1))
      (when (logbitp i x)
        (setf acc (logior acc bit))))))

(defun unswizzle (x &optional (width *outer-width*))
  (let ((acc1 0)
        (acc2 0)
        (bit  1))
    (dotimes (i width (values acc1 acc2))
      (when (logtest x 1)
        (incf acc1 bit))
      (when (logtest x 2)
        (incf acc2 bit))
      (setf x   (ash x -2)
            bit (ash bit 1)))))

(defun ordered-pairs (&key (width *outer-width*)
                        filter)
  (let ((pairs '()))
    (dotimes (i (ash 1 (* 2 width)) (nreverse pairs))
      (multiple-value-bind (x y)
          (unswizzle i width)
        (when (or (not filter)
                  (funcall filter x y))
          (push (cons x y) pairs))))))

(defun sorted-reverses (width &optional filter)
  (let ((lower (ceiling width 2))
        (rest  (floor   width 2)))
    (mapcan (lambda (pair)
              (let* ((i (car pair))
                     (j (cdr pair))
                     (x (logior i (bit-reverse j width)))
                     (y (logior j (bit-reverse i width))))
                (when (or (= lower rest)
                          (not (logbitp rest (logxor i j))))
                  (list (cons x y)))))
            (ordered-pairs :width lower
                           :filter filter))))

(defun emit-small-bit-reverse (width &optional output)
  (let ((indices (sorted-reverses width)))
    (format output "~
LINKAGE void PREFIX(~D) ARGLIST
\{
~{	SWAP(~{~D, ~D~});~^~%~}
\}
"
            width
            (mapcar (lambda (x)
                      (list (car x) (cdr x)))
                    indices))))

(defun backslashify (stream pattern &rest args)
  (flet ((doit (s)
           (map nil (lambda (c)
                      (if (eql c #\Newline)
                          (format s "\\~%")
                          (write-char c s)))
                (apply 'format nil pattern args))
           (fresh-line s)))
    (if stream
        (doit stream)
        (with-output-to-string (s)
          (doit s)))))

(defun emit-preload (&key (name "PRELOAD_REV")
                       (width *outer-width*)
                       (output *standard-output*))
  (backslashify output
                "~
#define ~A(SHIFT, SCALE) 
~{	const ul rev~D = (~DUL<<(SHIFT))*(SCALE);~^~%~}"
                name
                (loop for i below (ash 1 width)
                      collect i
                      collect (bit-reverse i width))))

(defun emit-block-swap (&key
                          (name "BLOCK_SWAP")
                          (width *outer-width*)
                          (output *standard-output*)
                          filter)
  (backslashify output
                "~
#define ~A(SWAPPER) do {
~{	SWAPPER(~{~D, ~D~});~^~%~}
} while (0)"
                name
                (mapcar (lambda (pair)
                          (list (car pair) (cdr pair)))
                        (ordered-pairs :width  width
                                       :filter filter)) ))

(defun emit-foreach-macros (&optional (s *standard-output*))
  (backslashify s "~
#define FOREACH_SMALL_REV(MACRO) 
~{	MACRO(~D)~^~%~}"
                (loop for i upto *small-width-max* collect i))
  (terpri s)
  (backslashify s "~
#define FOREACH_MEDIUM_REV(MACRO) 
~{	MACRO(~D)~^~%~}"
                (loop for i from (1+ *small-width-max*)
                        upto *medium-width-max*
                      collect i)))

(defun emit-include-file (&key (name "revbin_bits.h")
                            ((:width *outer-width*) *outer-width*))
  (with-open-file (s name :direction :output :if-exists :supersede)
    (format s "~
#ifndef ~A
#define ~:*~A

#define LEAF_WIDTH       ~D
#define SMALL_WIDTH_MAX  ~D
#define MEDIUM_WIDTH_MAX ~D

"
            (substitute #\_ #\.
                        (string-upcase name))
            *outer-width*
            *small-width-max*
            *medium-width-max*)
    (emit-preload :output s)
    (terpri s)
    (emit-block-swap :output s)
    (terpri s)
    (emit-foreach-macros s)
    (format s "
#endif
")))

(defun emit-small-bit-reverse-file (&key (name "small_revbin.inc")
                                      (width *small-width-max*))
  (with-open-file (s name :direction :output :if-exists :supersede)
    (dotimes (i (1+ width))
      (emit-small-bit-reverse i s)
      (unless (= i width)
        (terpri s)))))
