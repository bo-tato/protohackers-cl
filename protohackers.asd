(require 'asdf)
(asdf:defsystem "protohackers"
  :depends-on (alexandria
               iterate
               usocket
               usocket-server
               lisp-binary
               trivia
               trivia.ppcre
               serapeum
               flexi-streams
               lparallel
               shasht
               metabang-bind
               arrows
               str)
  :components ((:file protohackers)))
