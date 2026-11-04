(module fx1n
  (plc-location->string
   string->plc-words
   string->instructions
   string->assembly
   plc-words->instructions
   plc-words->string
   assembly->instructions
   assembly->string
   instructions->plc-words
   instructions->assembly
   instructions->string
   compute-program-checksum
   plc-open
   plc-close
   plc-hi
   plc-send-command-str
   plc-send-enquiry
   plc-read-version
   plc-read-step-count
   plc-read-block
   plc-write-block
   plc-read-program-checksum
   plc-write-program-checksum
   plc-read-params
   plc-read-program
   plc-write-program)


(import chicken.bitwise)
(import chicken.file.posix)
(import srfi-1)
(import srfi-13) ;; extra string stuff
(import scheme.base)
(import scheme.char)
(import chicken.base)
(import miscmacros)
(import (prefix comparse comparse:)) ;; contains stuff like "is" and "char-seq"

(define MEMORY_S #x000)
(define MEMORY_X #x400)
(define MEMORY_Y #x500)
(define MEMORY_T #x600)
(define MEMORY_M #x800)
(define MEMORY_C #xe00)
(define MEMORY_M8xxx #xf00)

(define (plc-location->string loc)
  (cond 
    [(>= loc MEMORY_M8xxx) (string-append "M" (number->string (+ 8000 (- loc MEMORY_M8xxx))))] ;; is this right?
    [(>= loc MEMORY_C) (string-append "C" (number->string (- loc MEMORY_C)))]
    [(>= loc MEMORY_M) (string-append "M" (number->string (- loc MEMORY_M)))]
    [(>= loc MEMORY_T) (string-append "T" (number->string (- loc MEMORY_T)))]
    [(>= loc MEMORY_Y) (string-append "Y" (number->string (- loc MEMORY_Y)))]
    [(>= loc MEMORY_X) (string-append "X" (number->string (- loc MEMORY_X)))]
    [(>= loc MEMORY_S) (string-append "S" (number->string (- loc MEMORY_S)))]))


(define test-program1
  "00340164FAFF0144026403C500C5FCFF034401C5")
(define test-program2 
  "022400C50025000602800080002601C50125000E03800080002E02C5002E025400D8002802440C00008E00E80F00")
(define test-bad-program1
  "00340164FAFF0144026403C500C5FCFF034401C5F")
(define test-assembly1 
  (list "LD   X0" 
        "AND  X1" 
        "OR   X2" 
        "ANI  X3" 
        "OUT  Y0" 
        "OUT  T0   K100"
        "LDI  X4"
        "OUT  Y1"
        "END"))

(define (string->plc-words str)
  (define (word-chars->number b1 b2)
    (string->number (list->string (append b2 b1)) 16))
  (define hex-char (comparse:in (string->list "0123456789ABCDEF")))
  (define byte-chars (comparse:sequence hex-char hex-char))
  (define word-chars (comparse:sequence byte-chars byte-chars))
  (define word (comparse:lift (lambda (x) (apply word-chars->number x)) word-chars))
  (define-values (x unparsed) (comparse:parse (comparse:zero-or-more word) str))
  (ensure comparse:parser-input-end? unparsed "not all of bytecode string was parsed")
  x)

(define (plc-words->instructions words)
  (define (high-byte w) (arithmetic-shift w -8))
  (define (low-byte w) (bitwise-and w #xff))
  (define (nibble w) (arithmetic-shift w -12))
  (define (slab w) (bitwise-and w #xfff))
  (define constant
    (comparse:lift 
      (lambda (words)
        (+ (low-byte (first words))
           (arithmetic-shift (low-byte (second words)) 8))) 
      (comparse:sequence comparse:item comparse:item)))
  (define (fn-instr w)
    (cond
      ([= (slab w) #x0f] (comparse:result (list 'END)))
      ([= (slab w) #x08] (comparse:lift (cut list 'PLS <>) (comparse:lift slab comparse:item)))
      ([= (slab w) #x09] (comparse:lift (cut list 'PLF <>) (comparse:lift slab comparse:item)))
      ([= (slab w) #x0c] (comparse:lift (cut list 'RST <>) (comparse:lift slab comparse:item)))
      ([<= #x600 (slab w) #x7ff] (comparse:lift (cut list 'OUT (slab w) <>) constant))
      ([<= #xe00 (slab w) #xeff] (comparse:lift (cut list 'OUT (slab w) <>) constant))
      (else comparse:fail)))
  (define (ch-instr w)
    (case (bitwise-and w #x0f)
      ([1]  (comparse:result (list 'STL)))
      ([8]  (comparse:result (list 'ANB)))
      ([9]  (comparse:result (list 'ORB)))
      ([10] (comparse:result (list 'MPS)))
      ([11] (comparse:result (list 'MRD)))
      ([12] (comparse:result (list 'MPP)))
      ([13] (comparse:result (list 'INV)))
      ([15] (comparse:result (list 'NOP)))
      (else comparse:fail)))
  (define (instr1 w) 
    (case (nibble w)
      ([0]  (fn-instr w))
      ;([1]  (comparse:result (list 'CMDP  (slab w))))
      ([2]  (comparse:result (list 'LD    (slab w))))
      ([3]  (comparse:result (list 'LDI   (slab w))))
      ([4]  (comparse:result (list 'AND   (slab w))))
      ([5]  (comparse:result (list 'ANI   (slab w))))
      ([6]  (comparse:result (list 'OR    (slab w))))
      ([7]  (comparse:result (list 'ORI   (slab w))))
      ;([8]  (comparse:result (list 'EXT   (slab w))))
      ([12] (comparse:result (list 'OUT   (slab w))))
      ([13] (comparse:result (list 'SET   (slab w))))
      ([14] (comparse:result (list 'RST   (slab w))))
      ([15] (ch-instr w))
      (else comparse:fail)))
  (define instr (comparse:bind comparse:item instr1))
  (define-values (x unparsed) (comparse:parse (comparse:zero-or-more instr) words))
  (ensure comparse:parser-input-end? unparsed "not all words were parsed")
  x)

(define string->instructions (o plc-words->instructions string->plc-words))

(define (instructions->plc-words instrs)
  (define (high-byte w) (arithmetic-shift w -8))
  (define (low-byte w) (bitwise-and w #xff))
  (define (byte-byte b1 b2)
    (define bb1 (bitwise-and b1 #xff))
    (define bb2 (bitwise-and b2 #xff))
    (+ (arithmetic-shift bb1 8) bb2))
  (define (nibble-slab n s)
    (define nn (bitwise-and n #xf))
    (define ss (bitwise-and s #xfff))
    (+ (arithmetic-shift nn 12) ss))
  (define (nibble-slab-lst n s)
    (list (nibble-slab n s)))
  (define (out->word instr)
    (cond
      ([= (length instr) 2] (nibble-slab-lst 12 (second instr)))
      ([= (length instr) 3] 
       (list (nibble-slab 0 (second instr))
             (byte-byte #x80 (low-byte (third instr)))
             (byte-byte #x80 (high-byte (third instr)))))
      (else (error 'out->word "invalid OUT instruction"))))
  (define (rst->word instr)
    (case (string-ref (plc-location->string (second instr)) 0)
      ([#\C #\T]
       (list (nibble-slab 0 #x0c)
             (nibble-slab 8 (second instr))))
      (else (nibble-slab-lst 14 (second instr)))))
  (define (cmd->word i)
    (define ii (bitwise-and i #xf))
    (nibble-slab-lst 15 (+ #xff0 ii)))
  (define (instr->word instr)
    (case (first instr)
      ([LD]  (nibble-slab-lst 2 (second instr)))
      ([LDI] (nibble-slab-lst 3 (second instr)))
      ([AND] (nibble-slab-lst 4 (second instr)))
      ([ANI] (nibble-slab-lst 5 (second instr)))
      ([OR]  (nibble-slab-lst 6 (second instr)))
      ([ORI] (nibble-slab-lst 7 (second instr)))
      ([SET] (nibble-slab-lst 13 (second instr)))
      ([STL] (cmd->word 1))
      ([ANB] (cmd->word 8))
      ([ORB] (cmd->word 9))
      ([MPS] (cmd->word 10))
      ([MRD] (cmd->word 11))
      ([MPP] (cmd->word 12))
      ([INV] (cmd->word 13))
      ([NOP] (cmd->word 15))
      ([PLS] (list (nibble-slab 0 #x08) (nibble-slab 8 (second instr))))
      ([PLF] (list (nibble-slab 0 #x09) (nibble-slab 8 (second instr))))
      ([RST] (rst->word instr))
      ([OUT] (out->word instr))
      ([END] (nibble-slab-lst 0 #x0f))
      (else (error 'instr->word "invalid instruction"))))
  (append-map instr->word instrs))


(define (assembly->instructions str)
  (define (->plc-location r i) 
    (case r
      ([#\S] (+ MEMORY_S i))
      ([#\X] (+ MEMORY_X i))
      ([#\Y] (+ MEMORY_Y i))
      ([#\T] (+ MEMORY_T i))
      ([#\M] (+ MEMORY_M i))
      ([#\C] (+ MEMORY_C i))
      (else #x00)))
  (define tokens1 (list "LDI" "LD" "AND" "ANI" "ORI" "OR" "OUT" "SET" "RST" "PLS" "PLF"))
  (define tokens2 (list "STL" "ANB" "ORB" "MPS" "MRD" "MPP" "INV" "NOP" "END"))
  (define whitespace (comparse:zero-or-more (comparse:is #\space)))
  (define number 
    (comparse:lift 
      (o string->number list->string)
      (comparse:one-or-more (comparse:satisfies char-numeric?))))
  (define (token-parser str) 
    (comparse:lift string->symbol (comparse:char-seq str)))
  (define plc-location0 
    (comparse:sequence (comparse:any-of (map comparse:is (list #\S #\X #\Y #\T #\M #\C))) number))
  (define plc-constant (comparse:preceded-by (comparse:is #\K) number))
  (define plc-location 
    (comparse:lift
      (lambda (x) (->plc-location (first x) (second x)))
      plc-location0))
  (define instruction1 
    (comparse:sequence 
      (comparse:any-of (map token-parser tokens1)) 
      (comparse:preceded-by whitespace plc-location)))
  (define instruction2 (comparse:sequence (comparse:any-of (map token-parser tokens2))))
  (define instruction3 
    (comparse:sequence 
      (token-parser "OUT") 
      (comparse:preceded-by whitespace plc-location) 
      (comparse:preceded-by whitespace plc-constant)))
  (define instruction 
    (comparse:preceded-by whitespace (comparse:any-of instruction3 instruction1 instruction2)))
  (define-values (x unparsed) (comparse:parse (comparse:zero-or-more instruction) str))
  (define-values (x2 unparsed2) (comparse:parse whitespace unparsed))
  (ensure comparse:parser-input-end? unparsed2 "not all assembly was parsed")
  x)

(define (remove-trailing-nops instrs)
  (if (null? instrs)
    instrs
    (if (eqv? (first (last instrs)) 'NOP)
      (remove-trailing-nops (butlast instrs))
      instrs)))

(define (instructions->assembly instrs)
  (define (instruction->line instr)
    (case (length instr)
      ([1] (list (symbol->string (first instr))))
      ([2] (list (symbol->string (first instr))
                 (plc-location->string (second instr))))
      ([3] (list (symbol->string (first instr))
                 (plc-location->string (second instr))
                 (string-append "K" (number->string (third instr)))))
      ))
  (define (line->string lst)
    (apply string-append (map (cut string-pad-right <> 5) lst)))
  (map (o line->string instruction->line) (remove-trailing-nops instrs)))

(define (plc-word->hex word)
  (define hex (string->vector "0123456789ABCDEF"))
  (map (lambda (i) (vector-ref hex (bitwise-and #xf (arithmetic-shift word i)))) 
       (list -4 0 -12 -8)))

(define (plc-words->string words)
  (list->string (append-map plc-word->hex words)))

(define instructions->string (o plc-words->string instructions->plc-words))
(define assembly->string (o instructions->string assembly->instructions))
(define string->assembly (o instructions->assembly string->instructions))

(define (compute-program-checksum words max-steps)
  (define sum (apply + (* (- max-steps (length words)) #xffff) words))
  (list->string (plc-word->hex sum)))

(define (bytecode-roundtrip? str)
  (string=? str (instructions->string (string->instructions str))))

(define plc-fd #f)

(define (plc-open path)
  (set! plc-fd (file-open path open/rdwr)))

(define (plc-close)
  (when plc-fd
    (file-close plc-fd)
    (set! plc-fd #f)))

(define (plc-wait)
  (define-values (ready1? ready2?) (file-select plc-fd #f 1))
  (unless ready1? (error "timeout")))

(define (plc-read-byte) 
  (plc-wait)
  (define bv (first (file-read plc-fd 1)))
  (bytevector-u8-ref bv 0))

(define (plc-write bv) 
  (file-write plc-fd bv))

(define (compute-command-checksum payload) 
  (define sum (apply + #x03 payload))
  (define hex (number->string (bitwise-and sum #xff) 16))
  (map (o char->integer char-upcase) (string->list hex)))

(define (plc-receive-response)
  (define (receive-payload)
    (let loop ([lst (list)])
      (let ([byte (plc-read-byte)])
        (if (= byte #x03)
          (reverse lst)
          (loop (cons byte lst)))
        )))
  (define (receive-packet)
    (let* ([payload (receive-payload)]
           [expected-checksum (compute-command-checksum payload)]
           [checksum1 (plc-read-byte)]
           [checksum2 (plc-read-byte)])
      (unless (every = expected-checksum (list checksum1 checksum2))
        (error "invalid command checksum"))
      payload))
  (let loop ()
    (case (plc-read-byte)
      ([#x02] (receive-packet))
      ([#x06] 'ACK)
      ([#x15] 'NACK)
      (else => identity))))

(define (plc-send-enquiry)
  (plc-write (bytevector #x05))
  (case (plc-read-byte)
    ([#x06] 'ACK)
    ([#x15] 'NACK)
    (else => identity)))

(define (plc-send-command payload)
  (define bytes 
    (apply bytevector 
           (append 
             (list #x02) 
             payload 
             (list #x03) 
             (compute-command-checksum payload))))
  (plc-write bytes)
  (plc-receive-response))

(define (plc-send-command-str payload-str)
  (define response (plc-send-command (map char->integer (string->list payload-str))))
  (if (list? response)
    (list->string (map integer->char response))
    response))

(define (plc-hi)
  (eqv? (plc-send-enquiry) 'ACK))

(define (plc-read-version)
  (define str1 (plc-send-command-str "00E0202"))
  (define str2 (number->string (first (string->plc-words str1))))
  (string-append "PLC: " (substring str2 3) " version: " (substring str2 0 3)))

(define (plc-read-step-count)
  (define str (plc-send-command-str "00E0402"))
  (first (string->plc-words str)))

(define (plc-read-program-checksum)
  (plc-send-command-str "0800202"))

(define (plc-write-program-checksum str)
  (plc-send-command-str (string-append "1800202" str)))

(define (plc-read-block addr size)
  (define addr-str (string-upcase (number->string addr 16)))
  (define size-str (string-upcase (number->string size 16)))
  (plc-send-command-str 
    (string-append "0" 
                   (string-pad addr-str 4 #\0)
                   (string-pad size-str 2 #\0))))

(define (plc-write-block addr str)
  (define size (quotient (string-length str) 2))
  (define addr-str (string-upcase (number->string addr 16)))
  (define size-str (string-upcase (number->string size 16)))
  (plc-send-command-str 
    (string-append "1" 
                   (string-pad addr-str 4 #\0)
                   (string-pad size-str 2 #\0)
                   str)))

(define (plc-read-params)
  (plc-read-block #x8000 #x5c))

(define (plc-read-program size)
  (plc-read-block #x805c size))

(define (plc-write-program size program-str)
  (define str (string-pad-right program-str (* size 2) #\F))
  (plc-write-block #x805c str))

) ;; end module
