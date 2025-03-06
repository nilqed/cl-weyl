(load "load-weyl")
;; necessary to create number system (get-rational-integers) ...
(compile-file "../weyl/numbers/numbers.lisp")
(compile-file "../weyl/tpower.lisp")


;(ql:quickload :weyl)

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

(taylor (+-> "cos(p)/(1-p^2)^4")  tsdp 12)
;1 + (7/2)p^2 + (193/24)p^4 + (10919/720)p^6 + (1024577/40320)p^8 + 
; (142682759/3628800)p^10 + (27509446273/479001600)p^12 + o(p^12)


