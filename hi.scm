(import chicken.file.posix)
(import chicken.bytevector)
(import chicken.string)

; NOTE: 7 bits; 1 stop bit; even parity
; stty -F /dev/ttyUSB0 raw -cstopb evenp
; or
; stty -F /dev/ttyUSB0 0:4:dad:8a38:3:1c:7f:15:4:0:1:0:11:13:1a:0:12:f:17:16:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0

(define plc (file-open "/dev/ttyUSB0" open/rdwr))


(print "writing to plc.")

(file-write plc (bytevector #x05))

(print "reading from plc.")

(define-values (bytes size) (apply values (file-read plc 1)))

(print "plc said: " (->string bytes))

(file-close plc)
