(import srfi-1)
(import scheme.char)
(import scheme.base)
(import chicken.string)
(import chicken.bitwise)
(import chicken.sort)
(import miscmacros)
(import comparse)
(import raylib)

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
  ERR    ERR   ERR   ERR 
  OUTYM  SETYM RSTYM CMDCH)

(define-enum instruction-list-cmdch->int 
             instruction-list-cmdch->sym
  ERR STL ERR ERR
  ERR ERR ERR ERR
  ANB ORB MPS MRD 
  MPP INV ERR RET)


(define (plc-location->string loc)
  (cond 
    [(>= loc MEMORY_M8xxx) (string-append "M80" (number->string (- loc MEMORY_M8xxx)))] ;; is this right?
    [(>= loc MEMORY_C) (string-append "C" (number->string (- loc MEMORY_C)))]
    [(>= loc MEMORY_M) (string-append "M" (number->string (- loc MEMORY_M)))]
    [(>= loc MEMORY_T) (string-append "T" (number->string (- loc MEMORY_T)))]
    [(>= loc MEMORY_Y) (string-append "Y" (number->string (- loc MEMORY_Y)))]
    [(>= loc MEMORY_X) (string-append "X" (number->string (- loc MEMORY_X)))]
    [(>= loc MEMORY_S) (string-append "S" (number->string (- loc MEMORY_S)))]))

(define hex (string->vector "0123456789ABCDEF"))

(define (write-byte b)
  (display (vector-ref hex (arithmetic-shift b -4)))
  (display (vector-ref hex (bitwise-and b #x0f))))

(define (write-word b1 b2)
  (write-byte b2)
  (write-byte b1))

(define (write-instruction1 op n)
  (let* ([op (list (arithmetic-shift op 4) #x00)]
         [nn (list (arithmetic-shift n -8) (bitwise-and n #xff))])
    (apply write-word (map bitwise-ior op nn))))

(define (write-instruction-cmch cmd)
  (write-instruction1 CMDCH (bitwise-ior #xff0 cmd)))

(define (write-out-tk t k)
  (write-instruction1 CMDFNC (+ #x600 t))
  (write-word #x80 (bitwise-and k #x0f))
  (write-word #x80 (arithmetic-shift k -8)))

(define (write-out-ck c k)
  (write-instruction1 CMDFNC (+ #xe00 c))
  (write-word #x80 (bitwise-and k #x0f))
  (write-word #x80 (arithmetic-shift k -8)))

(define current-high #x00)
(define current-low #x00)

(define *program* #f)

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

(define (read-instruction-out-tk t)
  (pop-instruction!)
  (pop-instruction!)
  (list 'OUTTK t))

(define (read-instruction-out-ck t)
  (pop-instruction!)
  (pop-instruction!)
  (list 'OUTCK t))

(define (read-instruction-cmdfnc op p) 
  (cond 
    ([<= #x600 p #x7ff] (read-instruction-out-tk p))
    ([<= #xe00 p #xeff] (read-instruction-out-ck p))
    (else (list (instruction-list1->sym op) p))))

(define (read-instruction)
  (let ([op (arithmetic-shift current-high -4)]
        [p (+ (arithmetic-shift (bitwise-and current-high #x0f) 8) current-low)])
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

(define parsed-program (read-program-string "00340164FAFF0144026403C500C5FCFF034401C5"))

(define graph (program->graph parsed-program))
(define graph-names (first graph))
(define graph-edges (second graph))

(define graph-end (car (graph-ends graph-edges)))

(define x-values (alist-cons graph-end 10 (compute-x-values graph-edges)))
(define y-values (compute-y-values graph-edges))

(define bg-color (color 0 0 200 255))
(define terminal-color (color 0 0 100 255))

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

(define tokens1 (list "LDI" "LD" "AND" "ANI" "ORI" "OR" "OUT" "SET" "RST"))
(define tokens2 (list "STL" "ANB" "ORB" "MPS" "MRD" "MPP" "INV" "RET"))

(define (string->program str)
  (define whitespace (zero-or-more (is #\space)))
  (define number (bind (as-string (one-or-more (satisfies char-numeric?))) (lambda (s) (result (string->number s)))))
  (define (token-parser str) (bind (char-seq str) (lambda (s) (result (string->symbol s)))))
  (define plc-location0 (sequence (any-of (map is (list #\S #\X #\Y #\T #\M #\C))) number))
  (define plc-location (bind plc-location0 (lambda (x) (result (->plc-location (first x) (second x))))))
  (define instruction1 (sequence (any-of (map token-parser tokens1)) (preceded-by whitespace plc-location)))
  (define instruction2 (sequence (any-of (map token-parser tokens2))))
  (define instruction (preceded-by whitespace (any-of instruction1 instruction2)))
  (define program (zero-or-more instruction))
  (parse program str))


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
    (else
      (begin
        (draw-circle x y 8 WHITE)
        (draw-circle x y 7 bg-color))))
  (unless (null? (cdr name))
    (draw-text-ex font (plc-location->string (second name)) (make-vec2 (- x 20) (- y 25)) 16 0 WHITE)))

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
  (for-each
    (lambda (line line-number)
      (let ([x (+ (rect-x terminal-rec) 20)]
            [y (+ (rect-y terminal-rec) (* 20 line-number) 20)]
            [w (- (rect-w terminal-rec) 40)]
            [h 14]
            [str (apply string-append (intersperse (map (cut format-string-spacing <> 5) (string-split line)) " "))])
        (when (= line-number current-line)
          (draw-rectangle-rec (margin-rec (make-rect x y w h) -2) bg-color))
        (draw-text-ex font str (make-vec2 x y) 16 0 WHITE)))
    terminal-lines (iota (length terminal-lines)))
  )


(define input-handler #f)

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

(define (draw)
  (with-drawing
    (lambda ()
      (clear-background bg-color)
      (draw-line-ex (make-vec2 50 0) (make-vec2 50 500) 2 WHITE)
      (draw-line-ex (make-vec2 (* 50 11) 0) (make-vec2 (* 50 11) 500) 2 WHITE)
      (for-each draw-edge graph-names graph-edges y-values)
      (for-each draw-node-verticals (graph-nodes graph-edges))
      (draw-terminal)
      )))

(define (gui)
  (init-window 800 500 "plc")

  (set! font (load-font "vga.ttf"))
  (set! input-handler input-normal-mode)
  (draw)

  (let loop ()
    (input-handler)
    (if (= (get-key-pressed) KEY_NULL)
      (with-drawing (lambda () #f))
      (draw))
    (unless (and (window-should-close?) (not (key-pressed? KEY_ESCAPE)))
      (loop)))

  (close-window))
