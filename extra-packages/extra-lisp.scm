(define-module (extra-lisp))
(use-modules (guix packages)
             (guix git-download)
             (guix build-system asdf)
             (guix licenses)
             (gnu packages lisp-xyz)
             (gnu packages lisp-check))

(define-public sbcl-quasiquote-2.0
  (package
   (name "sbcl-quasiquote-2.0")
   (version "0.3")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/mabragor/quasiquote-2.0")
           (commit "cac90875d1f66e9385e559bfebafe6b7808b0930")))
     (sha256
      (base32
       "1g0s3aplrgmdjj8k1wrx3dkqdsl4lka2nmgdng0rcd93xp11q6hn"))
     (file-name (git-file-name name version))))
   (inputs
    (list sbcl-iterate))
   (native-inputs (list sbcl-fiveam))
   (build-system asdf-build-system/sbcl)
   (arguments '(#:asd-systems '("quasiquote-2.0")
                #:tests? #f))
   (synopsis "Writing macros that write macros. Effortless.")
   (description
    "Quasiquote more suitable for macros that define other macros")
   (home-page "Quasiquote more suitable for macros that define other macros")
   (license gpl3)))

(define-public sbcl-lisp-binary
  (package
    (name "sbcl-lisp-binary")
    (version "1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/j3pic/lisp-binary")
             (commit "711c38c2d9862bec7b83c8bf42823dd3985dc517")))
       (sha256
        (base32
         "0vn1kjvcch9ky50rq1axg5hixf3zkbb46as99g0aks1b7y250a17"))
       (file-name (git-file-name name version))))
    (inputs
     (list sbcl-closer-mop sbcl-moptilities sbcl-flexi-streams sbcl-alexandria sbcl-cffi sbcl-quasiquote-2.0))
    (build-system asdf-build-system/sbcl)
    (synopsis "A library to easily read and write complex binary formats.")
    (description
     "Declare binary formats as structs and then read and write them.")
    (home-page "https://github.com/j3pic/lisp-binary")
    (license gpl3)))

(define-public sbcl-usocket
  (package
    (name "sbcl-usocket")
    (version "0.8.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/usocket/usocket/")
             (commit (string-append "v" version))))
       (file-name (git-file-name "cl-usocket" version))
       (sha256
        (base32 "0by8hhg6gijdbq5vjykd374rmvqyikp4synpyk0wjcl7rk3r0vgn"))))
    (build-system asdf-build-system/sbcl)
    (native-inputs
     (list sbcl-rt))
    (inputs
     (list sbcl-bordeaux-threads sbcl-split-sequence))
    (arguments
     `(#:tests? #f ; FIXME: Tests need network access?
       #:asd-systems '("usocket"
                       "usocket-server")))
    (home-page "https://common-lisp.net/project/usocket/")
    (synopsis "Universal socket library for Common Lisp")
    (description
     "This library strives to provide a portable TCP/IP and UDP/IP socket
interface for as many Common Lisp implementations as possible, while keeping
the abstraction and portability layer as thin as possible.")
    (license expat)))

(define-public sbcl-cl-str
  (package
    (name "sbcl-cl-str")
    (version "0.19.1")
    (home-page "https://github.com/vindarel/cl-str")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit "b1c83802a323ebd843b82b30c8473c84cb09cea3")))
              (sha256
               (base32 "0l2b01mr0jxln1igxg0gdwyaxvm2fm9v5a862d2q61mdi1yadddk"))
              (file-name (git-file-name name version))))
    (build-system asdf-build-system/sbcl)
    (inputs
     (list sbcl-cl-ppcre sbcl-cl-ppcre-unicode sbcl-cl-change-case))
    (native-inputs
     (list sbcl-fiveam))
    (arguments
     '(#:asd-systems '("str")))
    (synopsis "Modern, consistent and terse Common Lisp string manipulation library")
    (description "A modern and consistent Common Lisp string manipulation
library that focuses on modernity, simplicity and discoverability:
@code{(str:trim s)} instead of @code{(string-trim '(#\\Space ...) s)}), or
@code{str:concat strings} instead of an unusual format construct; one
discoverable library instead of many; consistency and composability, where
@code{s} is always the last argument, which makes it easier to feed pipes and
arrows.")
    (license expat)))
