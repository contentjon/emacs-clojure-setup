(use 'robert.hooke
     '[leiningen.util.paths :only [leiningen-home]])
(require 'leiningen.compile)

(add-hook #'leiningen.compile/get-readable-form
          (fn [orig j project form init]
            (orig j project form
                  `(do ~init (load-file (str (leiningen-home)
                                             "/user.clj"))))))
