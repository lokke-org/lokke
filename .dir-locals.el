;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil .
      ((eval .
             (progn
               (let ((file-name (expand-file-name default-directory)))
                 (setenv "GUILE_LOAD_PATH" (concat file-name "mod"))
                 (setenv "GUILE_EXTENSIONS_PATH" (concat file-name "lib/.libs/"))
                 (setenv "LD_LIBRARY_PATH" (concat file-name "lib/.libs/"))))))))
