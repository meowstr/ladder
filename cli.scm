(import fx1n)

(import chicken.process-context)
(import chicken.io)
(import chicken.string)
(import chicken.condition)
(import chicken.base)
(import srfi-1)


(define (read-file-to-string file-path)
  (with-input-from-file 
    file-path
    (lambda () (read-string #f))))

(define (with-plc path proc)
  (plc-open path)
  (handle-exceptions exn
    (begin
      (plc-close)
      (raise exn))
    (let ([ret (proc)])
      (plc-close)
      ret)))

(define (assemble file-path)
  (define str (read-file-to-string file-path))
  (define clean-str (string-translate str #\newline #\space))
  (assembly->string clean-str))

(define (assemble-and-show file-path)
  (define str (assemble file-path))
  (print str))

(define (assemble-and-upload serial-path file-path)
  (define str (assemble file-path))
  (with-plc 
    serial-path
    (lambda ()
      (unless (plc-hi) (error "failed to connect"))
      (plc-write-program 64 str)
      )))

(define (download-and-disassemble serial-path)
  (define str 
    (with-plc
      serial-path
      (lambda ()
        (unless (plc-hi) (error "failed to connect"))
        (plc-read-program 64)
        )))
  (for-each print (string->assembly str)))

(define (disassemble-and-show str)
  (for-each print (string->assembly str)))

(define (show-help)
  (print (program-name) " upload <serial-path> <assembly-file>")
  (print (program-name) " download <serial-path>")
  (print (program-name) " assemble <assembly-file>")
  (print (program-name) " disassemble <hex-string>")
  (print (program-name) " <assembly-file>"))

(define args (command-line-arguments))

(define (process-command command . args) 
  (cond 
    ((string=? command "upload") (assemble-and-upload (first args) (second args)))
    ((string=? command "download") (download-and-disassemble (first args)))
    ((string=? command "assemble") (assemble-and-show (first args)))
    ((string=? command "disassemble") (disassemble-and-show (first args)))
    ))

(case (length args)
  ([0] (show-help))
  ([1] (assemble-and-show (first args)))
  (else (apply process-command args)))
