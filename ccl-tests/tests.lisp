;;;-*-Mode: LISP; Package: CL-TEST -*-
;;;
;;; CCL-specific tests.  New tests go here, or in compiler.lisp for
;;; compiler tests; the older ones are still in ansi-tests/ccl.lsp,
;;; which is loaded first, so its helpers (such as TEST-COMPILE and
;;; TEST-SOURCE-FILE) are available here.
;;;
;;; The tests run with the current directory set to ansi-tests/, so a
;;; relative file name in a test names a file there.

(in-package :cl-test)

(deftest ccl.issue#544.stassats
    (values (array-element-type (make-array 0 :element-type 'base-char))
            (upgraded-array-element-type 'base-char))
  character
  character)

;;; UPGRADED-ARRAY-ELEMENT-TYPE and ARRAY-ELEMENT-TYPE got their answers
;;; from separate tables, which disagreed about strings (BASE-CHAR vs
;;; CHARACTER).  They should name the same type specifier.
(deftest ccl.issue#544
    (loop for type in '(base-char character standard-char bit (unsigned-byte 8)
                        (signed-byte 8) (unsigned-byte 16) (signed-byte 16)
                        (unsigned-byte 32) (signed-byte 32) fixnum
                        #+64-bit-target (unsigned-byte 64)
                        #+64-bit-target (signed-byte 64)
                        single-float double-float
                        (complex single-float) (complex double-float) t)
          unless (equal (array-element-type (make-array 0 :element-type type))
                        (upgraded-array-element-type type))
            collect type)
  nil)

;;; TRUNCATE by a ratio whose numerator is 1 returned a temporary bignum
;;; from the stack, so FLOOR, CEILING, ROUND, MOD and REM gave a bogus
;;; object or a wrong value.
(deftest ccl.issue#733
    (let* ((tiny (/ (expt 2 70)))
           (q (* 3 (expt 2 68))))
      (loop for (name got want)
              in (list (list 'truncate
                             (multiple-value-list (truncate 3/4 tiny))
                             (list q 0))
                       (list 'floor
                             (multiple-value-list (floor 3/4 tiny))
                             (list q 0))
                       (list 'ceiling
                             (multiple-value-list (ceiling 3/4 tiny))
                             (list q 0))
                       (list 'round
                             (multiple-value-list (round 3/4 tiny))
                             (list q 0))
                       (list 'mod (mod 3/4 tiny) 0)
                       (list 'rem (rem 3/4 tiny) 0))
            unless (equal got want)
              collect name))
  nil)

;;; The same bug, for a bignum dividend (or MOST-NEGATIVE-FIXNUM, which is
;;; handled as one).  A dangling quotient still has the right value until
;;; something reuses its stack, so do every division before checking any
;;; of them: each one lands where the previous one's temporary was.
(deftest ccl.issue#733.bignum
    (let* ((cases (list (list (expt 2 100) 10)
                        (list (- (expt 3 70)) 7)
                        (list (1+ (expt 2 64)) 3)
                        (list most-negative-fixnum (expt 2 70))
                        (list (- (expt 5 60)) 1000)
                        (list (expt 11 40) 13)))
           (quotients (loop for (x d) in cases
                            collect (truncate x (/ d)))))
      (loop for (x d) in cases
            for got in quotients
            unless (eql got (* x d))
              collect x))
  nil)

;;; (ccl::%alias-special ALIAS TARGET) makes ALIAS another name for the
;;; special variable TARGET.  The tests use fresh uninterned symbols.
(defun alias-test-pair (&optional (value nil value-p))
  (let ((alias (make-symbol "ALIAS"))
        (target (make-symbol "TARGET")))
    (when value-p
      (setf (symbol-value target) value))
    (ccl::%alias-special alias target)
    (values alias target)))

(defun alias-test-in-thread (fn)
  (ccl:join-process (ccl:process-run-function "alias-test" fn)))

;;; The global value is shared, in both directions, including a SETQ done
;;; by a thread that has no binding.
(deftest ccl.alias-special.global-value
    (multiple-value-bind (alias target) (alias-test-pair 1)
      (list (symbol-value alias)
            (progn (setf (symbol-value alias) 2) (symbol-value target))
            (progn (setf (symbol-value target) 3) (symbol-value alias))
            (progn (alias-test-in-thread
                    (lambda () (eval `(setq ,alias 4))))
                   (symbol-value target))
            (progn (alias-test-in-thread
                    (lambda () (eval `(setq ,target 5))))
                   (symbol-value alias))
            (alias-test-in-thread (lambda () (symbol-value alias)))))
  (1 2 3 4 5 5))

;;; If only the alias has a value when it is aliased, the target gets it.
(deftest ccl.alias-special.alias-value-migrates
    (let ((alias (make-symbol "ALIAS"))
          (target (make-symbol "TARGET")))
      (setf (symbol-value alias) 7)
      (ccl::%alias-special alias target)
      (list (symbol-value target) (symbol-value alias)))
  (7 7))

(deftest ccl.alias-special.boundp-makunbound
    (multiple-value-bind (alias target) (alias-test-pair)
      (let ((r '()))
        (push (list (boundp alias) (boundp target)) r)
        (set alias 1)
        (push (list (boundp alias) (boundp target)) r)
        (makunbound alias)
        (push (list (boundp alias) (boundp target)) r)
        (set target 2)
        (makunbound target)
        (push (list (boundp alias) (boundp target)) r)
        (set target 3)
        (push (list (symbol-value alias) (eval alias) (eval target)) r)
        (nreverse r)))
  ((nil nil) (t t) (nil nil) (nil nil) (3 3 3)))

;;; A dynamic binding made through one name is seen through the other,
;;; in the binding thread only.
(deftest ccl.alias-special.binding
    (multiple-value-bind (alias target) (alias-test-pair 0)
      (list
       (eval `(let ((,alias 1))
                (declare (special ,alias))
                (list (symbol-value ',alias) (symbol-value ',target)
                      ,target ,alias
                      (alias-test-in-thread
                       (lambda () (symbol-value ',target))))))
       (eval `(let ((,target 2))
                (declare (special ,target))
                (setq ,alias 3)
                (list (symbol-value ',alias) ,target)))
       (symbol-value alias)
       (symbol-value target)))
  ((1 1 1 1 0) (3 3) 0 0))

(deftest ccl.alias-special.progv
    (multiple-value-bind (alias target) (alias-test-pair 0)
      (list (progv (list alias) '(1)
              (list (symbol-value target) (symbol-value alias)))
            (progv (list target) '(2)
              (list (symbol-value target) (symbol-value alias)))
            (symbol-value alias)))
  ((1 1) (2 2) 0))

;;; A function compiled after the aliasing refers to the shared variable.
(deftest ccl.alias-special.compiled-code
    (multiple-value-bind (alias target) (alias-test-pair 10)
      (let ((read (compile nil `(lambda () ,alias)))
            (write (compile nil `(lambda (v) (setq ,alias v))))
            (bind (compile nil `(lambda (f)
                                  (let ((,alias 20))
                                    (declare (special ,alias))
                                    (funcall f))))))
        (list (funcall read)
              (progn (funcall write 11) (symbol-value target))
              (progn (setf (symbol-value target) 12) (funcall read))
              (funcall bind (compile nil `(lambda () ,target)))
              (funcall read))))
  (10 11 12 20 12))

;;; Aliasing an alias aliases its target.
(deftest ccl.alias-special.chain
    (let ((a (make-symbol "A"))
          (b (make-symbol "B"))
          (c (make-symbol "C")))
      (ccl::%alias-special b c)
      (ccl::%alias-special a b)
      (setf (symbol-value a) 1)
      (list (eq (ccl::%special-alias-target a) c)
            (symbol-value b)
            (symbol-value c)
            (eq (ccl::%alias-special a c) a)))
  (t 1 1 t))

(deftest ccl.alias-special.errors
    (let ((a (make-symbol "A"))
          (b (make-symbol "B"))
          (c (make-symbol "C")))
      (ccl::%alias-special a b)
      (flet ((errorp (fn)
               (handler-case (progn (funcall fn) nil)
                 (error () t))))
        (list (errorp (lambda () (ccl::%alias-special a c)))
              (errorp (lambda () (ccl::%alias-special b a)))
              (errorp (lambda () (ccl::%alias-special c c)))
              (errorp (lambda () (ccl::%alias-special c 'pi)))
              (errorp (lambda () (ccl::%alias-special c :foo)))
              (errorp (lambda () (ccl::%alias-special c nil)))
              (errorp (lambda () (ccl::%alias-special 'pi c)))
              (errorp (lambda () (ccl::%alias-special :foo c)))
              (errorp (lambda () (ccl::%alias-special t c)))
              (errorp (lambda () (ccl::%alias-special 1 c)))
              ;; A symbol with a binding in effect cannot become an alias.
              (let ((d (make-symbol "D")))
                (funcall (eval `(lambda (fn)
                                  (let ((,d 1))
                                    (declare (special ,d))
                                    (funcall fn))))
                         (lambda ()
                           (errorp (lambda () (ccl::%alias-special d c))))))
              (eq (ccl::%special-alias-target a) b))))
  (t t t t t t t t t t t t))
