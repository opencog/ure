; Follow the recommendation of Guile manual for managing extensions and library paths
; https://www.gnu.org/software/guile/manual/html_node/Modules-and-Extensions.html

; We have a conditional to flick between build environment and install environment.

; We call it as-config as the opencog root module is split across git repos.
; Ideally each repo would be a separate namespace because it makes 
(define-module (opencog as-config))

; CMAKE uses this tag generate different files for in-build 
(define-public atomspace-in-build-dir @SCM_IN_BUILD_DIR@)
(define install-location "${CMAKE_INSTALL_PREFIX}/lib/opencog/")

; If you create a new binary library for scheme, define it here
; along with it's destination path in the build directory.
; You also need to add the symbol to the install dir section below.

(define-public opencog-ext-path-smob "@CMAKE_BINARY_DIR@/opencog/guile/")
(define-public opencog-ext-path-exec "@CMAKE_BINARY_DIR@/opencog/atoms/execution/")
(define-public opencog-ext-path-dist-gearman "@CMAKE_BINARY_DIR@/opencog/persist/gearman/libdist-gearman.so")
(define-public opencog-ext-path-logger "@CMAKE_BINARY_DIR@/opencog/guile/modules/")
(define-public opencog-ext-path-persist "@CMAKE_BINARY_DIR@/opencog/persist/guile/")
(define-public opencog-ext-path-persist-sql "@CMAKE_BINARY_DIR@/opencog/persist/sql/multi-driver/")
(define-public opencog-ext-path-persist-zmq "@CMAKE_BINARY_DIR@/opencog/persist/zmq/")
(define-public opencog-ext-path-python-scm "@CMAKE_BINARY_DIR@/opencog/cython/")
(define-public opencog-ext-path-randgen "@CMAKE_BINARY_DIR@/opencog/guile/modules/")
(define-public opencog-ext-path-ure "@CMAKE_BINARY_DIR@/opencog/ure/")
(define-public opencog-ext-path-type-utils "@CMAKE_BINARY_DIR@/opencog/guile/modules/")

(if (not atomspace-in-build-dir)
    (begin
        (let ( (is-testing (getenv "ATOMSPACE_TEST")) )
            (if (eq? is-testing "1")
                (exit)
            ))
    
        ; When installed, all the modules have the same path
        ; TODO: Is there a way to automatically set all these symbols to
        ; the same location? It would preferable for us to only have to
        ; change one line in this file to add a new module.
        ; This would be trivial in Python but I'm still getting used to
        ; Scheme/Guile. Suggestions welcome!
        (set! opencog-ext-path-smob install-location)
        (set! opencog-ext-path-exec install-location)
        (set! opencog-ext-path-dist-gearman install-location)
        (set! opencog-ext-path-logger install-location)
        (set! opencog-ext-path-persist install-location)
        (set! opencog-ext-path-persist-sql install-location)
        (set! opencog-ext-path-persist-zmq install-location)
        (set! opencog-ext-path-python-scm install-location)
        (set! opencog-ext-path-randgen install-location)
        (set! opencog-ext-path-ure install-location)
        (set! opencog-ext-path-type-utils install-location)
    )
)

; For reference:
; The below was how the path was previously set up using LTDL_LIBRARY_PATH
; This required less build time configuration, but made it hard to know where
; your libraries were being loaded from, and could silent mix up libraries in your
; build dir and system dirs.
; ---
;
; In theory, we should have opencog installed into one of these locations:
;    /usr/lib/guile/2.2/extensions
;    /usr/local/lib/guile/2.2/extensions
;    /usr/lib64/guile/2.2/extensions
;    /usr/local/lib64/guile/2.2/extensions
;
; But which one? Its a pain, so we wing it, below, and use
; LTDL_LIBRARY_PATH

; lib64 is used by various versions of CentOS
;(define path "/usr/lib/opencog:/usr/lib64/opencog:/usr/local/lib/opencog:/usr/local/lib64/opencog")
;(setenv "LTDL_LIBRARY_PATH"
;	(if (getenv "LTDL_LIBRARY_PATH")
;		(string-append (getenv "LTDL_LIBRARY_PATH") ":" path)
;		path))