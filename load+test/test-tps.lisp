;(load "load-weyl")
;; necessary to create number system (get-rational-integers) ...
;(compile-file "../weyl/numbers/numbers.lisp")
;(compile-file "../weyl/tpower.lisp")
(ql:quickload :weyl)

(in-package :weyl)

(defvar tsd1 (weyli::get-tpower-series-domain (get-rational-integers) 'x))
(defvar tsd2 (get-tpower-series-domain (get-rational-numbers) 'x))
(taylor (sin 'x) tsd2  8)  ;; not tsd1 of course =Z

(ge-var p) ;; not standard
(defvar tsdp (get-tpower-series-domain (get-rational-numbers) p))
(taylor (sin p) tsdp 4) 
(taylor (+-> "cos(p)")  tsdp 6)
(taylor (+-> "1/(1-p^2)")  tsdp 6)
(taylor (+-> "1/(1-p^2)^4")  tsdp 6)

 

