;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-

#+quicklisp (ql:quickload :asdf-package-system)
#-asdf3 (progn 
	  #.`(
	      #+quicklisp ,@'(cerror 
			      "It is in : ~%
 (merge-pathnames \"quicklisp/local-projects/asdf/\"
		  (user-homedir-pathname)), ~%
So (asdf:upgrade-asdf) and (ql:quickload :asdf).~%
")			  #-quicklisp error			  			  		   			     
			  "PWAP requires ASDF 3 or later. Please upgrade your ASDF.")
	  (let ((asdf:*central-registry* 
		 (list (merge-pathnames "quicklisp/local-projects/asdf/"
					(user-homedir-pathname)))))
	    (asdf:upgrade-asdf) (ql:quickload :asdf)))
	  
(asdf:defsystem :pwap
  :description "PWAP: Paradigms of Web Application Programming"
  :long-description ""
  :class :package-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on ())

(asdf:register-system-packages 
 :cxml 
 '(:sax :runes))

(asdf:register-system-packages 
 :cxml-stp
 '(:stp :cxml-stp))
