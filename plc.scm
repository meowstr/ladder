(import srfi-1)
(import srfi-18)
(import scheme.char)
(import scheme.base)
(import chicken.string)
(import chicken.bitwise)
(import chicken.sort)
(import miscmacros)
(import raylib)
(import deterministic-threads)

(import (prefix comparse comparse:)) ;; we can't have stuff like "is" and "char-seq" binded to this

(deterministic-threads #t) ;; <3

(load "com.so")
(import com)

(define MEMORY_S #x000)
(define MEMORY_X #x400)
(define MEMORY_Y #x500)
(define MEMORY_T #x600)
(define MEMORY_M #x800)
(define MEMORY_C #xe00)

(define MEMORY_M8xxx #xf00)

(define-enum instruction-list1->int 
             instruction-list1->sym
  CMDFNC CMDP  LD    LDI   
  AND    ANI   OR    ORI 
  EXTEND ERR   ERR   ERR 
  OUT    SET   RST CMDCH)

(define-enum instruction-list-cmdch->int 
             instruction-list-cmdch->sym
  ERR STL ERR ERR
  ERR ERR ERR ERR
  ANB ORB MPS MRD 
  MPP INV ERR RET)

;; NOTE: 0142 is the sum of the 8000 program words (actual sum is 4201) (remember to byte swap the words)

(define *program* (list))

(define (plc-location->string loc)
  (cond 
    [(>= loc MEMORY_M8xxx) (string-append "M" (number->string (+ 8000 (- loc MEMORY_M8xxx))))] ;; is this right?
    [(>= loc MEMORY_C) (string-append "C" (number->string (- loc MEMORY_C)))]
    [(>= loc MEMORY_M) (string-append "M" (number->string (- loc MEMORY_M)))]
    [(>= loc MEMORY_T) (string-append "T" (number->string (- loc MEMORY_T)))]
    [(>= loc MEMORY_Y) (string-append "Y" (number->string (- loc MEMORY_Y)))]
    [(>= loc MEMORY_X) (string-append "X" (number->string (- loc MEMORY_X)))]
    [(>= loc MEMORY_S) (string-append "S" (number->string (- loc MEMORY_S)))]))


(define (string->program-words str)
  (define (swap x) (list (second x) (first x)))
  (define (lst-pairs lst)
    (if (null? lst)
      (list)
      (cons (take lst 2) (lst-pairs (drop lst 2)))))
  (let* ([lst (string->list str)]
         [bytes (lst-pairs lst)]
         [word-pairs (map swap (lst-pairs bytes))]
         [word-strings (map list->string (map concatenate word-pairs))]
         [words (map (cut string->number <> 16) word-strings)])
    words))

(define hex (string->vector "0123456789ABCDEF"))

(define (compute-program-checksum program-str max-steps)
  (define words (string->program-words program-str))
  (define sum (apply + (* (- max-steps (length words)) #xffff) words))
  (define x1 (bitwise-and #xf (arithmetic-shift sum -12)))
  (define x2 (bitwise-and #xf (arithmetic-shift sum -8)))
  (define x3 (bitwise-and #xf (arithmetic-shift sum -4)))
  (define x4 (bitwise-and #xf (arithmetic-shift sum 0)))
  (list->string (map (cut vector-ref hex <>) (list x3 x4 x1 x2))))

(define (compute-program-header program-str max-steps)
  (string-append "0800" (compute-program-checksum program-str max-steps) 
                 "0000000020202020202020202020202020202020202020202020202020202020202020202020202020202020F409FF0BF401E703640EC70EDC0EFF0E9001FE03000000000000000000000000000000000000000000000000" ))

(define (write-byte b)
  (push! (vector-ref hex (arithmetic-shift b -4)) *program*)
  (push! (vector-ref hex (bitwise-and b #x0f)) *program*))

(define (write-word b1 b2)
  (write-byte b2)
  (write-byte b1))

(define (write-instruction1 op n)
  (define opcode (instruction-list1->int op))
  (let* ([op (list (arithmetic-shift opcode 4) #x00)]
         [nn (list (arithmetic-shift n -8) (bitwise-and n #xff))])
    (apply write-word (map bitwise-ior op nn))))

(define (write-instruction-cmch cmd)
  (define cmdcode (instruction-list-cmdch->int cmd))
  (write-instruction1 'CMDCH (bitwise-ior #xff0 cmdcode)))

(define (write-extended-out loc k)
  (write-instruction1 'CMDFNC loc)
  (write-word #x80 (bitwise-and k #x0f))
  (write-word #x80 (arithmetic-shift k -8)))

(define (write-rst-tc loc)
  (write-instruction1 'CMDFNC #x0c)
  (write-instruction1 'EXTEND loc))

(define (write-pls loc)
  (write-instruction1 'CMDFNC #x08)
  (write-instruction1 'EXTEND loc))

(define (write-plf loc)
  (write-instruction1 'CMDFNC #x09)
  (write-instruction1 'EXTEND loc))

(define (write-end)
  (write-instruction1 'CMDFNC #x0f))

(define (write-instruction instr)
  (case (car instr)
    ([LD LDI AND ANI OR ORI SET] 
     (write-instruction1 (first instr) (second instr)))
    ([STL ANB ORB MPS MRD MPP INV RET]
     (write-instruction-cmch (first instr)))
    ([OUT]
     (if (>= (length instr) 3)
       (write-extended-out (second instr) (second (third instr)))
       (write-instruction1 'OUT (second instr))))
    ([RST]
     (let ([loc-type (car (string->list (plc-location->string (second instr))))])
       (if (or (eqv? loc-type #\C) (eqv? loc-type #\T))
         (write-rst-tc (second instr))
         (write-instruction1 'RST (second instr)))))
    ([END] (write-end))
    ([PLS] (write-pls (second instr)))
    ([PLF] (write-plf (second instr)))))

(define (write-program-string program)
  (set! *program* (list))
  (for-each write-instruction program)
  (list->string (reverse *program*)))

(define current-high #x00)
(define current-low #x00)

(define (pop-byte!)
  (let ([high (string->number (string (pop! *program*)) 16)]
        [low (string->number (string (pop! *program*)) 16)])
    (+ low (* high 16))))

(define (pop-instruction!)
  (set! current-low (pop-byte!))
  (set! current-high (pop-byte!)))

(define (read-instruction1 op p) 
  (list (instruction-list1->sym op) p))

(define (read-instruction-cmdch op p) 
  (list (instruction-list-cmdch->sym (bitwise-and p #x0f))))

(define (read-constant!)
  (define low 0)
  (define high 0)
  (pop-instruction!)
  (set! low current-low)
  (pop-instruction!)
  (set! high current-low)
  (+ low (arithmetic-shift high 8)))

(define (read-instruction-out-tk t)
  (list 'OUT t (list #\K (read-constant!))))

(define (read-instruction-out-ck t)
  (list 'OUT t (list #\K (read-constant!))))

(define (instruction-p)
  (+ (arithmetic-shift (bitwise-and current-high #x0f) 8) current-low))

(define (read-instruction-cmdfnc op p) 
  (cond 
    ([= p #x0f] (list 'END))
    ([= p #x08] 
     (pop-instruction!) 
     (list 'PLS (instruction-p)))
    ([= p #x09] 
     (pop-instruction!) 
     (list 'PLF (instruction-p)))
    ([= p #x0c] 
     (pop-instruction!) 
     (list 'RST (instruction-p)))
    ([<= #x600 p #x7ff] (read-instruction-out-tk p))
    ([<= #xe00 p #xeff] (read-instruction-out-ck p))
    (else (list (instruction-list1->sym op) p))))

(define (read-instruction)
  (let ([op (arithmetic-shift current-high -4)]
        [p (instruction-p)])
    (case (instruction-list1->sym op)
      ([CMDFNC] (read-instruction-cmdfnc op p ))
      ([CMDCH] (read-instruction-cmdch op p ))
      (else (read-instruction1 op p)))))

(define (read-program-string program-str)
  (set! *program* (string->list program-str))
  (let loop ([lst (list)])
    (if (null? *program*)
      (reverse lst)
      (begin
        (pop-instruction!)
        (loop (cons (read-instruction) lst))))))



(define (graph-start? edges i)
  (not (any (lambda (edge)
              (= (cdr edge) i)) edges)))

(define (graph-starts edges)
  (delete-duplicates 
    (filter (cut graph-start? edges <>) 
            (map car edges))))

(define (graph-end? edges i)
  (not (any (lambda (edge)
              (= (car edge) i)) edges)))

(define (graph-ends edges)
  (delete-duplicates 
    (filter (cut graph-end? edges <>) 
            (map cdr edges))))

(define (rename x from to)
  (if (= x from) to x))

(define (map-pair f pair)
  (cons (f (car pair))
        (f (cdr pair))))

(define (rename-pair pair from to)
  (map-pair (cut rename <> from to) pair))

(define (rename-node edges from to)
  (map (cut rename-pair <> from to) edges))

(define (connect-nodes edges nodes)
  (let ([to (car nodes)])
    (fold 
      (lambda (from edges) (rename-node edges from to))
      edges nodes)))

(define (connect-starts edges)
  (connect-nodes edges (graph-starts edges)))

(define (connect-ends edges)
  (connect-nodes edges (graph-ends edges)))

(define (program->graph program)
  (define (cons-top x stack)
    (if (null? stack)
      (error 'cons-top "empty stack")
      (cons (cons x (car stack)) (cdr stack))))
  (define (change-top x stack)
    (if (null? stack)
      (error 'change-top "empty stack")
      (cons x (cdr stack))))
  (define (merge-top-2 f stack)
    (if (or (null? stack) (null? (cdr stack)))
      (error 'merge-top-2 "less than 2 stack")
      (let ([a (first stack)]
            [b (second stack)])
        (cons (f a b) (cddr stack)))))
  (define (top s) 
    (if (null? s)
      (error 'top "empty stack")
      (car s)))
  (define-record state i names stack1 stack2 stack3)
  (define x
    (fold
      (lambda (instr s)
        (let ([i (state-i s)]            ;; current id
              [names (state-names s)]    ;; list of edge names
              [stack1 (state-stack1 s)]  ;; stack of edge lists
              [stack2 (state-stack2 s)]  ;; stack of working nodes (corresponds to stack1)
              [stack3 (state-stack3 s)]) ;; MPS/MRD/MPP stack
          (case (car instr)
            ([LD LDI]
             (state-i-set! s (add1 i))
             (state-names-set! s (cons instr names))
             (state-stack1-set! s (cons (list (cons 0 i)) stack1))
             (state-stack2-set! s (cons i stack2))
             s)
            ([AND ANI]
             (state-i-set! s (add1 i))
             (state-names-set! s (cons instr names))
             (state-stack1-set! s (cons-top (cons (top stack2) i) stack1))
             (state-stack2-set! s (change-top i stack2))
             s)
            ([OR ORI]
             (state-names-set! s (cons instr names))
             (state-stack1-set! s (cons-top (cons 0 (top stack2)) stack1))
             s)
            ([ORB]
             (state-stack1-set! 
               s 
               (merge-top-2 
                 (lambda (a b) 
                   ;; connect their ends by renaming a's end to b's end id
                   (append (rename-node a (first stack2) (second stack2)) b)) 
                 stack1))
             (state-stack2-set! s (cdr stack2)) ;; graph "a" merged, no longer need its end
             s)
            ([ANB]
             (state-stack1-set! 
               s 
               (merge-top-2 
                 (lambda (a b) 
                   ;; connect a's starts to b's end
                   (append (rename-node a 0 (second stack2)) b))
                 stack1))
             (state-stack2-set! s (cons (car stack2) (cddr stack2))) ;; remove second element (b's end)
             s)
            ([MPS]
             (state-stack3-set! s (cons (top stack2) stack3))
             s)
            ([MRD]
             (state-stack2-set! s (change-top (top stack3) stack2))
             s)
            ([MPP]
             (state-stack2-set! s (change-top (top stack3) stack2))
             (state-stack3-set! s (cdr stack3))
             s)
            (else
              (state-i-set! s (add1 i))
              (state-names-set! s (cons instr names))
              (state-stack1-set! s (cons-top (cons (top stack2) i) stack1))
              s))))
      (make-state 1 (list) (list) (list) (list))
      program))
  (list (reverse (state-names x)) 
        (connect-ends (reverse (concatenate (state-stack1 x))))))



(define (graph-from edges from)
  (delete-duplicates 
    (filter-map 
      (lambda (edge) 
        (and (= (car edge) from) (cdr edge)))
      edges)))

(define (graph-nodes edges)
  (delete-duplicates 
    (concatenate (map (lambda (x) (list (car x) (cdr x))) edges))))

;; NOTE: does not detect loops
(define (max-alt-path edges from to len)
  (if (= from to) 
    len
    (apply 
      max -1 
      (map (cut max-alt-path edges <> to (add1 len)) 
           (graph-from edges from)))))

(define (compute-x-values edges)
  (map (lambda (node) 
         (cons node (max-alt-path edges 0 node 0))) 
       (graph-nodes edges)))

(define (compute-y-values edges)
  (define edge-ref (cut list-ref edges <>))
  (define x-list (compute-x-values edges))
  (define (good-node? closed last-node new-node)
    ;; find first edge in open s.t. the start node:
    ;; is the last end node (it connects the last edge) or:
    ;;   1. it connects to another node in the closed list
    ;;   2. and has an x position > to last node
    (let ([closed-nodes (graph-nodes (map edge-ref (map car closed)))]
          [last-x (cdr (assoc last-node x-list))]
          [new-x (cdr (assoc new-node x-list))])
      (or (= last-node new-node)
          (and (member new-node closed-nodes)
               (> new-x last-x)))))
  (define (choose-edge open closed last-node)
    (find 
      (lambda (edge)
        (good-node? closed last-node (car (edge-ref edge)))) 
      open))
  (let loop ([open (iota (length edges))] 
             [closed (list)] 
             [last-node 0]
             [y 0])
    (if (null? open)
      (map cdr (sort closed (lambda (a b) (< (car a) (car b)))))
      (let ([new-edge (choose-edge open closed last-node)])
        (if new-edge
          (loop 
            (delete new-edge open) ;; remove from open
            (alist-cons new-edge y closed) ;; add to closed with current y
            (cdr (edge-ref new-edge)) ;; change node cursor to the next one
            y) ;; same y
          (loop open closed 0 (add1 y)) ;; next line, reset node to 0
          ))
      )))

;(define parsed-program (read-program-string "00340164FAFF0144026403C500C5FCFF034401C5"))
(define parsed-program (read-program-string "022400C50025000602800080002601C50125000E03800080002E02C5002E025400D8002802440C00008E00E80F00"))

;; out: 022400C50025000602800080002601C50125000E03800080002E02C5002E025400D8002802440C00008E00E80F00
;;      022400C50025000602800080002601C50125000E03800080002E02C5002E025400D8002802440C00008E00E80F00

(define graph (program->graph parsed-program))
(define graph-names (first graph))
(define graph-edges (second graph))

(define graph-end (car (graph-ends graph-edges)))

(define x-values (alist-cons graph-end 10 (compute-x-values graph-edges)))
(define y-values (compute-y-values graph-edges))

;; (define bg-color (color 0 0 170 255))
;; (define terminal-color (color 0 0 100 255))

(define bg-color (color 0 0 0 255))
(define terminal-color (color 0 0 0 255))
(define terminal-select-color (color 100 0 0 255))

(define (edge-points edge y-value)
  (list (make-vec2 (cdr (assoc (car edge) x-values)) y-value)
        (make-vec2 (cdr (assoc (cdr edge) x-values)) y-value)))

(define (world->screen p)
  (make-vec2 (+ 50 (* 50 (vec2-x p)))
             (+ 50 (* 50 (vec2-y p)))))

(define font #f)

(define (margin-rec r amount)
  (make-rect (+ (rect-x r) amount)
             (+ (rect-y r) amount)
             (- (rect-w r) (* amount 2))
             (- (rect-h r) (* amount 2))))

(define terminal-rec (margin-rec (make-rect 550 0 250 500) 5))
(define terminal-lines (list "LD X0" "AND X1" "OR X2" "OUT Y0"))
(define current-line 0)

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
(define tokens2 (list "STL" "ANB" "ORB" "MPS" "MRD" "MPP" "INV" "RET" "END"))

(define (string->program str)
  (define whitespace (comparse:zero-or-more (comparse:is #\space)))
  (define number 
    (comparse:bind 
      (comparse:as-string (comparse:one-or-more (comparse:satisfies char-numeric?))) 
      (lambda (s) (comparse:result (string->number s)))))
  (define (token-parser str) 
    (comparse:bind 
      (comparse:char-seq str) 
      (lambda (s) (comparse:result (string->symbol s)))))
  (define plc-location0 
    (comparse:sequence (comparse:any-of (map comparse:is (list #\S #\X #\Y #\T #\M #\C))) number))
  (define plc-constant (comparse:sequence (comparse:is #\K) number))
  (define plc-location 
    (comparse:bind 
      plc-location0 
      (lambda (x) (comparse:result (->plc-location (first x) (second x))))))
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
  (define program (comparse:zero-or-more instruction))
  (comparse:parse program str))


(define (draw-contact name x y)
  (case (car name)
    ([LD AND OR] 
     (begin
       (draw-rectangle (- x 7) (- y 7) 14 14 WHITE)
       (draw-rectangle (- x 5) (- y 7) 10 14 bg-color)))
    ([LDI ANI ORI]
     (begin
       (draw-rectangle (- x 7) (- y 7) 14 14 WHITE)
       (draw-rectangle (- x 5) (- y 7) 10 14 bg-color)
       (draw-line-ex (make-vec2 (- x 3) (+ y 3)) (make-vec2 (+ x 3) (- y 3)) 2 WHITE))
     )
    ([OUT]
     (begin
       (draw-rectangle (- x 20) (- y 5) 40 10 bg-color)
       (draw-text-ex font "(   )" (make-vec2 (- x 20) (- y 8)) 16 0 WHITE)))
    ([SET]
     (begin
       (draw-rectangle (- x 20) (- y 5) 40 10 bg-color)
       (draw-text-ex font "(SET)" (make-vec2 (- x 20) (- y 8)) 16 0 WHITE)))
    ([END]
     (begin
       (draw-rectangle (- x 20) (- y 5) 40 10 bg-color)
       (draw-text-ex font "(END)" (make-vec2 (- x 20) (- y 8)) 16 0 WHITE)))
    ([RST]
     (begin
       (draw-rectangle (- x 20) (- y 5) 40 10 bg-color)
       (draw-text-ex font "(RST)" (make-vec2 (- x 20) (- y 8)) 16 0 WHITE))))
  (unless (null? (cdr name))
    (draw-text-ex font (plc-location->string (second name)) (make-vec2 (- x 20) (- y 25)) 16 0 WHITE))
  (when (>= (length name) 3)
    (draw-text-ex font (string-append "K" (number->string (second (third name)))) (make-vec2 (- x 20) (+ y 8)) 16 0 WHITE))
  )

(define (draw-edge name edge y-value)
  (let* ([points (edge-points edge y-value)]
         [a (world->screen (first points))]
         [b (world->screen (second points))])
    (draw-line-v a b WHITE)
    (if (= graph-end (cdr edge))
      (draw-contact name
                    (inexact->exact (- (vec2-x b) 25)) 
                    (inexact->exact (+ 0 (vec2-y b))))
      (draw-contact name
                    (inexact->exact (+ 25 (vec2-x a))) 
                    (inexact->exact (+ 0 (vec2-y a)))))))

(define (pair-member? x pair)
  (or (= x (car pair))
      (= x (cdr pair))))

(define (draw-node-verticals node)
  (let ([x (cdr (assoc node x-values))]
        [y-list (delete-duplicates 
                  (filter-map 
                    (lambda (edge y)
                      (if (pair-member? node edge) y #f)) 
                    graph-edges y-values))])
    (pair-for-each 
      (lambda (pair)
        (unless (null? (cdr pair))
          ;(draw-circle-v (world->screen (make-vec2 x (car pair))) 2 WHITE)
          ;(draw-circle-v (world->screen (make-vec2 x (cadr pair))) 2 WHITE)
          (draw-line-ex (world->screen (make-vec2 x (car pair))) 
                        (world->screen (make-vec2 x (cadr pair))) 2 WHITE))) 
      y-list)))


(define (format-string-spacing str len)
  (if (>= (string-length str) len)
    (substring str 0 len)
    (apply string-append str (make-list (- len (string-length str)) " "))))

(define (draw-terminal)
  (draw-rectangle-rec terminal-rec terminal-color)
  (let ([x (+ (rect-x terminal-rec) 20)]
        [y (+ (rect-y terminal-rec) (* 20 current-line) 20)]
        [w (- (rect-w terminal-rec) 40)]
        [h 14])
    (draw-rectangle-rec (margin-rec (make-rect x y w h) -2) terminal-select-color))
  (for-each
    (lambda (line line-number)
      (let ([x (+ (rect-x terminal-rec) 20)]
            [y (+ (rect-y terminal-rec) (* 20 line-number) 20)]
            [w (- (rect-w terminal-rec) 40)]
            [h 14]
            [str (apply string-append (intersperse (map (cut format-string-spacing <> 5) (string-split line)) " "))])
        (draw-text-ex font str (make-vec2 x y) 16 0 WHITE)))
    terminal-lines (iota (length terminal-lines)))
  )


(define input-handler #f)
(define draw-handler #f)
(define byte-counter 0)

(define (parse-terminal-lines!)
  (handle-exceptions exn
    (print "failed to parse terminal instructions") 
    (set! parsed-program 
      (ensure 
        (o not null?) 
        (string->program (apply string-append (intersperse terminal-lines " ")))))
    (set! graph (program->graph parsed-program))
    (set! graph-names (first graph))
    (set! graph-edges (second graph))
    (set! graph-end (car (graph-ends graph-edges)))
    (set! x-values (alist-cons graph-end 10 (compute-x-values graph-edges)))
    (set! y-values (compute-y-values graph-edges))))

(define (clamp x a b)
  (max a (min x b)))

(define (change-current-line! proc)
  (define new-line (proc current-line))
  (set! current-line (clamp new-line 0 (length terminal-lines))))

(define edited-line #f)

(define (edit-line)
  (unless (>= current-line (length terminal-lines))
    (set! edited-line (list-ref terminal-lines current-line))
    (list-set! terminal-lines current-line (string-append edited-line "_"))
    (set! input-handler input-edit-mode)))

(define (insert-line)
  (define-values (left right) (split-at terminal-lines current-line))
  (set! terminal-lines (append left (list "") right))
  (edit-line))

(define (delete-line)
  (define-values (left right) (split-at terminal-lines current-line))
  (unless (>= current-line (length terminal-lines))
    (set! terminal-lines (append left (cdr right)))
    (change-current-line! identity)
    (parse-terminal-lines!)))

(define (input-normal-mode)
  (when (key-pressed? KEY_J) (change-current-line! add1))
  (when (key-pressed? KEY_K) (change-current-line! sub1))
  (when (key-pressed? KEY_O) (insert-line))
  (when (key-pressed? KEY_D) (delete-line))
  (when (key-pressed? KEY_I) (edit-line))
  (when (key-pressed? KEY_A) (edit-line))
  (when (key-pressed? KEY_F1) (start-upload-program))
  )

(define (input-edit-mode)
  (when (key-pressed? KEY_ESCAPE) 
    (if (= 0 (string-length edited-line))
      (delete-line)
      (list-set! terminal-lines current-line edited-line)) 
    (parse-terminal-lines!)
    (set! input-handler input-normal-mode))
  (when (key-pressed? KEY_ENTER)
    (list-set! terminal-lines current-line edited-line) 
    (change-current-line! add1)
    (parse-terminal-lines!)
    (insert-line))
  (when (and (> (string-length edited-line) 0) 
             (key-pressed? KEY_BACKSPACE))
    (set! edited-line (substring edited-line 0 (- (string-length edited-line) 1)))
    (list-set! terminal-lines current-line (string-append edited-line "_")))
  (let loop ([k (integer->char (get-char-pressed))])
    (unless (eqv? k #\null)
      (set! edited-line (string-append edited-line (string (char-upcase k))))
      (list-set! terminal-lines current-line (string-append edited-line "_"))
      (loop (integer->char (get-char-pressed))))))

(define tick 0)

(define (draw-ladder)
  (clear-background bg-color)
  (draw-text (number->string tick) 0 0 20 WHITE)
  (draw-line-ex (make-vec2 50 0) (make-vec2 50 500) 2 WHITE)
  (draw-line-ex (make-vec2 (* 50 11) 0) (make-vec2 (* 50 11) 500) 2 WHITE)
  (for-each draw-edge graph-names graph-edges y-values)
  (for-each draw-node-verticals (graph-nodes graph-edges))
  (draw-terminal))

(define (compute-checksum payload) 
  (define sum (apply + #x03 payload))
  (define hex (number->string (bitwise-and sum #xff) 16))
  (map (o char->integer char-upcase) (string->list hex)))


(define (await-com-byte)
  (let ([byte (com-read-byte)])
    (if (= byte -1)
      (begin
        (thread-yield!)
        (await-com-byte))
      byte)))

(define (receive-response)
  (define (receive-payload)
    (let loop ([lst (list)])
      (let ([byte (await-com-byte)])
        (if (= byte #x03)
          (reverse lst)
          (loop (cons byte lst)))
        )))
  (define (receive-packet)
    (let* ([payload (receive-payload)]
           [expected-checksum (compute-checksum payload)]
           [checksum1 (await-com-byte)]
           [checksum2 (await-com-byte)])
      (unless (every = expected-checksum (list checksum1 checksum2))
        (print "invalid checksum: " checksum1 " " checksum2 " is not expected " expected-checksum))
      payload))
  (let loop ()
    (case (await-com-byte)
      ([#x02] (receive-packet))
      ([#x06] 'ACK)
      ([#x15] 'NACK)
      (else => identity))))

(define (send-enquiry)
  (com-write (bytevector #x05) 1)
  (case (await-com-byte)
    ([#x06] 'ACK)
    ([#x15] 'NACK)
    (else => identity)))

(define (send-command payload)
  (define bytes (apply bytevector (append (list #x02) payload (list #x03) (compute-checksum payload))))
  (com-write bytes (bytevector-length bytes))
  (inc! byte-counter (bytevector-length bytes))
  (receive-response))

(define (send-command-str payload-str)
  (define response (send-command (map char->integer (string->list payload-str))))
  (list->string (map integer->char response)))

(define (async-upload-program)
  (repeat 200
    (thread-sleep! (/ 320 9600))
    (inc! byte-counter 40))
  (enable-event-waiting)
  (set! input-handler input-normal-mode)
  (set! draw-handler draw-ladder))

(define (start-upload-program)
  (set! byte-counter 0)
  (set! input-handler void)
  (set! draw-handler draw-menu1)
  (disable-event-waiting)
  (thread-start! async-upload-program))

(define (draw-menu1)
  (define (line y str)
    (draw-text-ex font str
                  (make-vec2 106 (+ 106 (* 20 y))) 16 0 WHITE))
  (clear-background bg-color)
  (draw-rectangle 100 100 600 300 WHITE)
  (draw-rectangle 102 102 596 296 bg-color)
  (line 0 (string-append "UPLOADING. " (number->string byte-counter) " BYTES"))
  )

(define (gui)
  (init-window 800 500 "fx1n")

  (set! font (load-font "vga.ttf"))
  (set! input-handler input-normal-mode)
  (set! draw-handler draw-ladder)
  (enable-event-waiting)
  (set-exit-key KEY_NULL)

  (let loop ()
    (inc! tick)
    (input-handler)
    (with-drawing draw-handler)
    (unless (and (window-should-close?))
      (thread-yield!)
      (loop)))

  (close-window))
