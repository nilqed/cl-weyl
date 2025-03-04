;;; assumes new/ge-support.lisp already in weyl.
(load "load-weyl")
(in-package :weyl)
;;;

#|
(defun wtype (obj) (cl::type-of obj))

(defun ltx-ratint (x)
  (format nil "{~A}" (slot-value x 'weyli::value)))  

(defun ltx-var (x) ;;TODO: subscripts
  (format nil "{~A}" (slot-value x 'weyli::string)))
  
(defun ltx-expt (x)
  (format nil "{{~A}^{~A}}" 
    (ltx (slot-value x 'weyli::base))
    (ltx (slot-value x 'weyli::exp))))
    
(defun ltx-plus (x)
  (format nil "~{{~a}~^ + ~}" (mapcar 'ltx (slot-value x 'weyli::terms))))

(defun ltx-times (x)
  (format nil "~{{~a}~^ \\, ~}" (mapcar 'ltx (slot-value x 'weyli::terms))))
  
(defun ltx-app (x)
  (format nil "\\operatorname{~a}( ~{{~a}~^ , ~})" 
    (slot-value x 'weyli::funct)
    (mapcar 'ltx (slot-value x 'weyli::args))))

(defun ltx (x)
  (case (wtype x) 
    ('rational-integer       (ltx-ratint x))
    ('weyli::ge-variable     (ltx-var x))
    ('weyli::ge-expt         (ltx-expt x))
    ('weyli::ge-plus         (ltx-plus x))
    ('weyli::ge-times        (ltx-times x))
    ('weyli::ge-application  (ltx-app x))
    (otherwise "END"))) 



(defun latex (x) 
  (format nil "$$~A$$" (ltx x)))
  
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ge-vars '(p q r x_0 x_1 x_2 x_3)) 

(defun test-ltx ()
  (list (ltx (+ p q))
        (ltx (* p q))
        (ltx (* p (expt q 2) ))
        (ltx (* p (expt q 2) (sin x_1))) 
        (ltx (/ p q))
        (ltx (+ (* x_3 (expt x_1 x_2)) (* x_0 x_1 x_2 (expt x_3 p))))
        (ltx (* p (expt q 2) (sin x_1))) 
        (ltx (/ x_1 x_0))
        (ltx (+ (* x_3 (expt x_1 x_2)) (* x_0 x_1 x_2 (expt x_3 p))))        
        ))

;;; Test with MathJax in browser ...
(defvar template
"<!DOCTYPE html>
<html>
<head>
<title>MathJax TeX Test Page</title>
<script type=\"text/javascript\" id=\"MathJax-script\" async
  src=\"https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js\">
</script>
</head>
<body>
  ~{$$ ~a $$~%  ~^ ~} ~%
</body>
</html>
")


(defun test-latex (lst)  ;; lst a list of latex-strings w/o $
  (let* ((htmlfile "/home/kfp/tmp/weyl-ltx.html")
         (browser "/usr/bin/firefox"))
         (with-open-file (s htmlfile :direction :output :if-exists :supersede) 
            (format s template lst))
             (sb-ext::run-program browser (list htmlfile))))
            
;(test-ltx) --> list of lattex-strings           
;(test-latex (test-ltx))

(defvar ltxm '("\\hbar" "\\int_\\Omega d \\omega =" 
               "\\int_{\\partial\\Omega} \\omega" "\\LaTeX"))

;(test-latex ltxm)


(defun test-latex->sixel ()
  (progn (display6 (+ p q))
         (display6 (* p q))
         (display6 (* p (expt q 2) ))
         (display6 (* p (expt q 2) (sin x_1))) 
         (display6 (/ p q))
         (display6 (+ (* x_3 (expt x_1 x_2)) (* x_0 x_1 x_2 (expt x_3 p))))
         (display6 (* p (expt q 2) (sin x_1))) 
         (display6 (/ x_1 x_0))
         (display6 (+ (* x_3 (expt x_1 x_2)) (* x_0 x_1 x_2 (expt x_3 p))))        
        ))


;;; mlterm fg_color: #f7f7f7 corresponds to "White" in dvipng
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-latex->sixel)



;(weyli::set-variable-property *general* p  'latex-repr "\\pi" )
;; "\\pi"
(get-variable-property *general* p  'latex-repr )
;; "\\pi"

(display6 p)

;;; note: adding subs creates a NEW variable ....

(defvar pp (add-subscripts p q r))
(print (getf pp :subscripts))

#|
(display6 (* p (expt q 2) ))
(display6 (* p (expt q 2) (sin x_1))) 
(display6 (deriv (+-> "p/q^r") p q r))
(display6 (+ (* x_3 (expt x_1 x_2)) (* x_0 x_1 x_2 (expt x_3 p))))
(display6 (* p (expt q 2) (sin x_1))) 
(weyli::set-variable-property *general* p  'latex-repr "\\pi" )
(display6 (+ p q))
|#
