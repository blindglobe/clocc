(in-package :cl-user)

(my-assert
 (#+cmu unix:unix-access
	#+sbcl sb-unix:unix-access "test-dir"
	#+cmu unix:r_ok
	#+sbcl sb-unix:r_ok)
 T)

(my-assert
 (#+cmu unix:unix-access #+sbcl sb-unix:unix-access "test-dir" #+cmu unix:w_ok #+sbcl sb-unix:w_ok)
 T)

(my-assert
 (#+cmu unix:unix-access #+sbcl sb-unix:unix-access "test-dir" #+cmu unix:x_ok #+sbcl sb-unix:x_ok)
 T)

(my-assert
 (#+cmu unix:unix-access #+sbcl sb-unix:unix-access "test-dir" #+cmu unix:f_ok #+sbcl sb-unix:f_ok)
 T)

(my-assert
 (#+cmu unix:unix-access #+sbcl sb-unix:unix-access "test-dir/a" #+cmu unix:r_ok #+sbcl sb-unix:r_ok)
 T)

(my-assert
 (#+cmu unix:unix-access #+sbcl sb-unix:unix-access "test-dir/a" #+cmu unix:w_ok #+sbcl sb-unix:w_ok)
 T)

(my-assert
 (#+cmu unix:unix-access #+sbcl sb-unix:unix-access "test-dir/a" #+cmu unix:x_ok #+sbcl sb-unix:x_ok)
 NIL)

(my-assert
 (#+cmu unix:unix-access #+sbcl sb-unix:unix-access "test-dir/a" #+cmu unix:f_ok #+sbcl sb-unix:f_ok)
 T)

(my-assert
 (progn
   (#+cmu unix:unix-gettimeofday #+sbcl sb-unix:unix-gettimeofday)
   t)
 t)

