RT-LAZYGEN

This is just me playing around with LISP.  I'm providing lazy
generator functions which let you build pipelines of
map/filter/take/drop etc.

There's nothing very complicated going on here.

The package shadows a number of lisp builtins, and as such it would
be generally inconvenient to "use."  It is intended to be
used with a local-nickname (which hopefully your lisp provides):

    (add-package-local-nickname "LG" "RT-LAZYGEN")
    (lg:--> (lg:range 1)
            (lg:take 5)
	    (lg:gen->list))
    => (1 2 3 4 5)

