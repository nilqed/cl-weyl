; some silence ...
(defun debug-ignore (c h) (declare (ignore h)) (print c) (abort))
(setf *debugger-hook* #'debug-ignore)
(declaim (sb-ext:muffle-conditions style-warning))

(defparameter weyl-folder "../weyl/")
(load (format nil "~A~A" weyl-folder "new/user-manual"))

(defparameter src-files (list
"packages"
"lisp-support"
"domain-support"
"classes/algebraic-domains"
"classes/space-classes"
"classes/general-classes"
"avl"
"lisp-numbers"
"sets"
"morphisms"
"quotient-fields"
"general" 
"fourier"
"functions"
"direct-sums"
"numbers/bigfloat"
"numbers/numbers"
"numbers/gfp"
"polynomials/poly-tools"
"polynomials/mpolynomial"
"polynomials/upolynomial"
"polynomials/epolynomial"
"polynomials/sparsegcd"
"polynomials/grobner"
"tpower"
"taylor"
"rational-functions"
"differential-domains"
"algebraic-extension"
"vector-spaces/vector"
"vector-spaces/projective-space"
"vector-spaces/quaternions"
 "matrix"
"topology"
"funct-spaces"
"mesh"
"new/weyl-infix"
"new/ge-support"
"new/ge-latex"))

 

(defun doc-output-file (f)
  (if (find #\/ f)
    (setf f (subseq f (+ (position  #\/ f)1))))
      (format nil "~Adocs/~A.txt" weyl-folder f))

(defun document-files (g)
  (loop for f in g do 
    (progn (format t "~A~%" f)
      (with-open-file (*standard-output* (doc-output-file f)
        :direction :output
        :if-exists :supersede)
        (ignore-errors 
          (create-user-manual 
             (format nil "~A~A.lisp" weyl-folder f)))))))
