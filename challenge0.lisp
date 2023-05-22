(in-package :protohackers)

(serve #'identity
       :reader #'read-byte
       :writer #'write-byte
       :binary t)
