;;; -*- Mode: LISP; Syntax: Common-lisp; Package: XLIB; Base: 10; Lowercase: Yes -*-

;;; This file contains patches to CLX that allow it to open a display
;;; using and internet style xxx.xxx.xxx.xxx address.

(in-package :xlib)

#+lucid
(lucid::DEF-FOREIGN-FUNCTION
 (INET-ADDR (:NAME "_inet_addr")
	    (:RETURN-TYPE :FIXNUM)
	    (:LANGUAGE :C))
 (HOST :STRING))

#+lucid
(lucid::DEF-FOREIGN-FUNCTION
 (connect-to-server (:NAME "_connect_to_server")
		    (:RETURN-TYPE :FIXNUM)
		    (:LANGUAGE :C))
 (HOST :STRING)
 (display :fixnum))

#+lucid
(user::load-foreign-files
 (list 
  (clu::logical-path "gfx"
		     (make::standard-binary-directory)
		     "socket.o"))
 (list "-lc" "-lm"))

#+Lucid
(defun host-address (host &optional (family :internet))
  ;; Return a list whose car is the family keyword (:internet :DECnet :Chaos)
  ;; and cdr is a list of network address bytes.
  (declare (type stringable host)
	   (type (or null (member :internet :decnet :chaos) card8) family))
  (declare (clx-values list))
  (if (plusp (setf addr (inet-addr host)))
      (return-from host-address
		   (list :internet
			 (ldb (byte 8 24) addr)
			 (ldb (byte 8 16) addr)
			 (ldb (byte 8 8) addr)
			 (ldb (byte 8 0) addr))))
  (labels ((no-host-error ()
			  (error "Unknown host ~S" host))
	   (no-address-error ()
			     (error "Host ~S has no ~S address" host family)))
	  (let ((hostent 0))
	    (setf hostent (gethostbyname (string host)))
	    (when (zerop (lcl:foreign-pointer-address hostent))
		  (no-host-error))
	    (ecase family
		   ((:internet nil 0)
		    (unless (= (hostent-h_addrtype hostent) 2)
			    (no-address-error))
		    (assert (= (hostent-h_length hostent) 4))
		    (let ((addr (hostent-h_addrlist hostent)))
		      (setq addr (lcl:foreign-value (lcl:foreign-value addr)))
		      (list :internet
			    (ldb (byte 8 24) addr)
			    (ldb (byte 8 16) addr)
			    (ldb (byte 8 8) addr)
			    (ldb (byte 8 0) addr))))))))

#+lucid
(defun open-x-stream (host display protocol)
  protocol ;; unused
  (let ((fd -1))
    (loop 
     for i below 5
     while (minusp fd) do
     (setf fd (connect-to-server host display))
     (sleep i))
    (if (minusp fd)
	(error "Failed to connect to server: ~A ~D" host display))
    (user::make-lisp-stream :input-handle fd
			    :output-handle fd
			    :element-type 'unsigned-byte
			    #-lcl3.0 :stream-type #-lcl3.0 :ephemeral)))
   


