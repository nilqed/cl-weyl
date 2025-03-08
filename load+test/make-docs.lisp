; some silence ...
(defun debug-ignore (c h) (declare (ignore h)) (print c) (abort))
(setf *debugger-hook* #'debug-ignore)
(declaim (sb-ext:muffle-conditions style-warning))

(defparameter weyl-folder "../weyl/")
(load (format nil "~A~A" weyl-folder "new/weyl-user-manual"))

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

 

(defun doc-output-file (f suffix)
  (if (find #\/ f)
    (setf f (subseq f (+ (position  #\/ f)1))))
      (format nil "~Adocs/~A.~A" weyl-folder f suffix))

(defun document-files (g &key (fmt 'text) (suffix "txt"))
  (loop for f in g do 
    (progn (format t "~A~%" f)
      (with-open-file (*standard-output* (doc-output-file f suffix)
        :direction :output
        :if-exists :supersede)
        (ignore-errors 
          (create-user-manual 
             (format nil "~A~A.lisp" weyl-folder f)   
                :output-format fmt))))))

;; (document-files src-files)
;; (document-files src-files :fmt 'latex :suffix "tex")
;; (document-files src-files :fmt 'text :suffix "md")

;; txt2html -p 0 


(defparameter html-index
"<html>
<head>
<title>Example for Frame</title>
</head>
    <frameset cols=\"30%,*\">
    <frame src=\"nav.html\" name=nav> 1st FRAME
    <frame src=\"start.html\" name=main> 2nd FRAME
    </frameset>
</html>")


(defparameter html-nav
"
<pre>
SOURCE FILES
</pre>
<details>
<summary>Packages and Lisp support</summary>
<a href=\"packages.txt\"  target=\"main\">packages</a><br>
<a href=\"lisp-support.txt\"  target=\"main\">lisp-support</a><br>
<a href=\"domain-support.txt\"  target=\"main\">domain-support</a><br>
</details>

<details>
<summary>Classes</summary>
<a href=\"algebraic-domains.txt\"  target=\"main\">algebraic-domains</a><br>
<a href=\"space-classes.txt\"  target=\"main\">space-classes</a><br>
<a href=\"general-classes.txt\"  target=\"main\">general-classes</a><br>
</details>

<details>
<summary>Generic Tools</summary>
<a href=\"avl.txt\"  target=\"main\">avl</a><br>
<a href=\"lisp-numbers.txt\"  target=\"main\">lisp-numbers</a><br>
</details>

<details>
<summary>Basic Tools</summary>
<a href=\"sets.txt\"  target=\"main\">sets</a><br>
<a href=\"morphisms.txt\"  target=\"main\">morphisms</a><br>
<a href=\"quotient-fields.txt\"  target=\"main\">quotient-fields</a><br>
</details>

<details>
<summary>General Expressions</summary>
<a href=\"general.txt\"  target=\"main\"> general</a><br>
</details>

<details>
<summary>Sums, Product, Quotients</summary>
<a href=\"fourier.txt\"  target=\"main\">fourier</a><br>
<a href=\"functions.txt\"  target=\"main\">functions</a><br>
<a href=\"direct-sums.txt\"  target=\"main\">direct-sums</a><br>
</details>

<details>
<summary>Numbers</summary>
<a href=\"bigfloat.txt\"  target=\"main\">bigfloat</a><br>
<a href=\"numbers.txt\"  target=\"main\">numbers</a><br>
<a href=\"gfp.txt\"  target=\"main\">gfp</a><br>
</details>

<details>
<summary>Polynomials</summary>
<a href=\"poly-tools.txt\"  target=\"main\">poly-tools</a><br>
<a href=\"mpolynomial.txt\"  target=\"main\">mpolynomial</a><br>
<a href=\"upolynomial.txt\"  target=\"main\">upolynomial</a><br>
<a href=\"epolynomial.txt\"  target=\"main\">epolynomial</a><br>
<a href=\"sparsegcd.txt\"  target=\"main\">sparsegcd</a><br>
<a href=\"grobner.txt\"  target=\"main\">grobner</a><br>
</details>

<details>
<summary>Truncated Power Series</summary>
<a href=\"tpower.txt\"  target=\"main\">tpower</a><br>
<a href=\"taylor.txt\"  target=\"main\">taylor</a><br>
</details>

<details>
<summary>Algebraic structures</summary>
<a href=\"rational-functions.txt\"  target=\"main\">rational-functions</a><br>
<a href=\"differential-domains.txt\"  target=\"main\">differential-domains</a><br>
<a href=\"algebraic-extension.txt\"  target=\"main\">algebraic-extension</a><br>
</details>

<details>
<summary>Linear Spaces</summary>
<a href=\"vector.txt\"  target=\"main\">vector</a><br>
<a href=\"projective-space.txt\"  target=\"main\">projective-space</a><br>
<a href=\"quaternions.txt\"  target=\"main\">quaternions</a><br>
<a href=\"matrix.txt\"  target=\"main\">matrix</a><br>
</details>

<details>
<summary>Topology and Function Spaces</summary>
<a href=\"topology.txt\"  target=\"main\">topology</a><br>
<a href=\"funct-spaces.txt\"  target=\"main\">funct-spaces</a><br>
</details>

<details>
<summary>Meshing</summary>
<a href=\"mesh.txt\"  target=\"main\">mesh</a><br>
</details>

<details>
<summary>New files</summary>
<a href=\"weyl-infix.txt\"  target=\"main\">weyl-infix</a><br>
<a href=\"ge-support.txt\"  target=\"main\">ge-support</a><br>
<a href=\"ge-latex.txt\"  target=\"main\">ge-latex</a><br>
</details>
")


(defparameter html-start
"<!doctype html>
<html lang=\"en\">
<head>
  <meta charset=\"utf-8\"/>
  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
  <title>The Weyl Computer Algebra Substrate</title>
</head>
<body>
  <header>
    <img src=\"data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAAUA
       AAAFCAYAAACNbyblAAAAHElEQVQI12P4//8/w38GIAXDIBKE0DHxgljNBAAO
        9TXL0Y4OHwAAAABJRU5ErkJggg==\" alt=\"Red dot\"/> Weyl
    
    <h5>HEADER</h5>
  </header>
  <nav>
    <ul>
      <li><a href=\"#link_1.html\">Link1</a></li>
      <li><a href=\"#link_2.html\">Link2</a></li>
      <li><a href=\"#link_3.html\">Link3</a></li>
    </ul>
  </nav>
  <main>
   <h4>This is h4</h4>
   <pre> This is -- preformatted </pre>
  </main>
  <footer>
     <h5>FOOTER</h5>
     <a href=\"contact.html\">Contact</a>
     <p>(c) 2025 by</p>
  </footer>
</body>
</html>")


(defun create-html-page (fn tpl)
  (with-open-file (*standard-output* (doc-output-file fn "html")
    :direction :output
    :if-exists :supersede)
    (ignore-errors (format t "~A" tpl))))
    

; (create-html-page "index" html-index)
; (create-html-page "nav" html-nav)
; #!/bin/sh
; for a in *.txt; do txt2html -p 1 $a > $(basename $a .txt).html ; done

(defun create-refman ()
  (progn
    (document-files src-files)
    (create-html-page "index" html-index)
    (create-html-page "nav" html-nav)
    (create-html-page "start" html-start)))

