(import srfi-1)
(import scheme.char)
(import scheme.base)
(import chicken.string)
(import chicken.bitwise)
(import chicken.sort)
(import miscmacros)
(import ncurses)
(import fx1n)

;(import deterministic-threads)
;(deterministic-threads #t) ;; <3

(define (graph-end? edges i)
  (not (any (lambda (edge)
              (= (car edge) i)) edges)))
(define (graph-ends edges)
  (delete-duplicates 
    (filter (cut graph-end? edges <>) 
            (map cdr edges))))

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

(define (instructions->graph instrs)
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
  (define (connect-ends edges)
    (connect-nodes edges (graph-ends edges)))
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
  (define s (make-state 1 (list) (list) (list) (list)))
  (define (process-instr instr)
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
         (state-stack2-set! s (cons i stack2)))
        ([AND ANI]
         (state-i-set! s (add1 i))
         (state-names-set! s (cons instr names))
         (state-stack1-set! s (cons-top (cons (top stack2) i) stack1))
         (state-stack2-set! s (change-top i stack2)))
        ([OR ORI]
         (state-names-set! s (cons instr names))
         (state-stack1-set! s (cons-top (cons 0 (top stack2)) stack1)))
        ([ORB]
         (state-stack1-set! 
           s 
           (merge-top-2 (lambda (a b) 
                          ;; connect their ends by renaming a's end to b's end id
                          (append (rename-node a (first stack2) (second stack2)) b)) 
                        stack1))
         ;; graph "a" merged, no longer need its end
         (state-stack2-set! s (cdr stack2)))
        ([ANB]
         ;; NOTE: check for side effects (OUT SET RST) in graph "a"
         ;; e.g. LD X0 LD X1 OUT Y0 ANB is invalid and should not be rendered
         (state-stack1-set! 
           s 
           (merge-top-2 (lambda (a b) 
                          (unless (graph-end? a (first stack2))
                            (error "ANB cannot have side effects"))
                          ;; connect a's starts to b's end
                          (append (rename-node a 0 (second stack2)) b))
                        stack1))
         ;; remove second element (b's end)
         (state-stack2-set! s (cons (car stack2) (cddr stack2))))
        ([MPS]
         (state-stack3-set! s (cons (top stack2) stack3)))
        ([MRD]
         (state-stack2-set! s (change-top (top stack3) stack2)))
        ([MPP]
         (state-stack2-set! s (change-top (top stack3) stack2))
         (state-stack3-set! s (cdr stack3)))
        ([NOP] (void))
        (else
          (state-i-set! s (add1 i))
          (state-names-set! s (cons instr names))
          (state-stack1-set! s (cons-top (cons (top stack2) i) stack1))))))
  (for-each process-instr instrs)
  (values (reverse (state-names s)) 
          (connect-ends (reverse (concatenate (state-stack1 s))))))


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

(define (bits . xs) 
  (fold (lambda (x a)
          (+ (if x 1 0) 
             (arithmetic-shift a 1))) 
        0 xs))

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
(define terminal-lines test-assembly1)

(initscr)
(noecho)
(define terminal-x (- (COLS) 30))
(define terminal-win (newwin (LINES) (- (COLS) terminal-x) 0 terminal-x))
(define log-win-border (newwin 7 (COLS) (- (LINES) 7) 0))
(define log-win (newwin 5 (- (COLS) 2) (- (LINES) 6) 1))
;(keypad terminal-win #t)
(scrollok log-win #t)

(define (draw-ladder)
  (define contact-width 8)
  (define rung-height 3)
  (define (->screen-x pair) (cons (car pair) (+ (* (cdr pair) contact-width) 1)))
  (define (->screen-y y) (+ (* y rung-height) 3))
  (define assembly-string (string-intersperse terminal-lines " "))
  (define instructions (assembly->instructions assembly-string))
  ;(define instructions (string->instructions test-program1))
  (define-values (names graph) (instructions->graph instructions))
  (define graph-end-node (car (graph-ends graph))) ;; should only be one
  (define x-values-raw (map ->screen-x (compute-x-values graph)))
  (define x-values (alist-cons graph-end-node (- terminal-x 2) x-values-raw))
  (define y-values (map ->screen-y (compute-y-values graph)))
  (define (draw-contact name x y)
    (when (>= (length name) 2)
      (mvaddstr (sub1 y) (add1 x) (plc-location->string (second name))))
    (when (>= (length name) 3)
      (let ([str (string-append "K" (number->string (third name)))])
        (mvaddstr (add1 y) (+ x 8 (- (string-length str))) str)))
    (case (first name) 
      ([LD AND OR]
       (mvaddstr y (+ x 3) "] ["))
      ([LDI ANI ORI]
       (mvaddstr y (+ x 3) "]/["))
      ([OUT]
       (mvaddstr y (+ x 2) "(   )"))
      ([SET]
       (mvaddstr y (+ x 2) "(SET)"))
      ([END]
       (mvaddstr y (+ x 2) "(END)"))
      ([RST]
       (mvaddstr y (+ x 2) "(RST)"))
      ))
  (define (draw-edge name edge y)
    (let ([x1 (cdr (assoc (car edge) x-values))]
          [x2 (cdr (assoc (cdr edge) x-values))])
      (mvhline y x1 0 (- x2 x1 -1))
      (if (= (cdr edge) graph-end-node)
        (draw-contact name (- x2 contact-width) y)
        (draw-contact name x1 y))
      ))
  (define (pair-member? x pair) (or (= x (car pair)) (= x (cdr pair))))
  (define (conn-character y x)
    (let ([left   (= (mvinch y (sub1 x)) (ACS_HLINE))]
          [right  (= (mvinch y (add1 x)) (ACS_HLINE))]
          [bottom (= (mvinch (add1 y) x) (ACS_VLINE))]
          [top    (= (mvinch (sub1 y) x) (ACS_VLINE))])
      (case (bits left right bottom top)
        ([#b1101] (ACS_BTEE))
        ([#b1110] (ACS_TTEE))
        ([#b1111] (ACS_PLUS))
        ([#b1001] (ACS_LRCORNER))
        ([#b1010] (ACS_URCORNER))
        ([#b1011] (ACS_RTEE))
        ([#b0101] (ACS_LLCORNER))
        ([#b0110] (ACS_ULCORNER))
        ([#b0111] (ACS_LTEE))
        (else (ACS_HLINE)))))
  (define conns (list))
  (define (draw-vertical y1 y2 x)
    (for-each
      (lambda (y)
        (when (= (mvinch y x) (ACS_HLINE))
          (push! (list x y) conns))
        (when (= (mvinch y x) (char->integer #\space)) 
          (mvaddch y x (ACS_VLINE))))
      (iota (- y2 y1 -1) y1)))
  (define (draw-verticals node)
    (let* ([x (cdr (assoc node x-values))]
           [y-list-raw (filter-map 
                         (lambda (edge y) (and (pair-member? node edge) y)) 
                         graph y-values)]
           [y-list (sort (delete-duplicates y-list-raw) <)])
      (pair-for-each 
        (lambda (pair)
          (unless (null? (cdr pair))
            (draw-vertical (car pair) (cadr pair) x))) 
        y-list)))
  (define (draw-conn x y)
    (mvaddch y x (conn-character y x)))

  (mvaddstr (- (LINES) 2) 2 "[1]   read   [2] write      [jk] move")
  (mvaddstr (- (LINES) 1) 2 "[rtn] edit   [o/O] insert   [q] quit")
  (for-each draw-edge names graph y-values)
  (for-each draw-verticals (graph-nodes graph))
  (draw-vertical 1 (- (LINES) 2) 1)
  (draw-vertical 1 (- (LINES) 2) (- terminal-x 2))
  (for-each (cut apply draw-conn <>) conns)
  (refresh))

(define (draw-ladder-protected)
  (handle-exceptions exn
    (begin
      (if (error-object? exn)
        (mvaddstr 1 1 (string-append "failed to draw ladder: " (error-object-message exn)))
        (mvaddstr 1 1 "failed to draw ladder: internal error"))
      (refresh))
    (draw-ladder)))

(define (string-trim str)
  (repeat 
    10
    (set! str (string-chomp str " ")))
  str)

(define (draw-terminal-line str y)
  (mvwaddstr terminal-win (add1 y) 3 str))

(define (draw-terminal)
  (box terminal-win 0 0)
  (for-each draw-terminal-line terminal-lines (iota (length terminal-lines)))
  (wrefresh terminal-win)
  ;(prefresh terminal-pad 0 0 1 (add1 terminal-x) (- (LINES) 3) (- (COLS) 3))
  )

(define (trim-spaces lst)
  (if (null? lst)
    lst
    (if (eqv? (last lst) #\space)
      (trim-spaces (butlast lst))
      lst)))


(define (insert-line i)
  (define-values (left right) (split-at terminal-lines i))
  (set! terminal-lines (append left (list "") right)))

(define (delete-line i)
  (define-values (left right) (split-at terminal-lines i))
  (set! terminal-lines (append left (cdr right))))

(define (edit-line line-number)
  (curs_set 1)
  (draw-terminal-line (list-ref terminal-lines line-number) line-number)
  (define line (string->list (list-ref terminal-lines line-number)))
  (define (char-alphnum? c)
    (or (char-alphabetic? c) (char-numeric? c) (eqv? c #\space)))
  (let loop ()
    (let ([k (wgetch terminal-win)])
      (when (and (char? k) (char-alphnum? k)) 
        (let repeat-char ()
          (unless (>= (length line) 20)
            (set! line (append line (list (char-upcase k))))
            (when (and (eqv? k #\space) (> (modulo (length line) 5) 0)) 
              (repeat-char)))))
      (when (or (eqv? k #\backspace)
                (eqv? k #\delete))
        (unless (null? line)
          (if (eqv? (last line) #\space)
            (modify! line trim-spaces)
            (modify! line butlast)
            )))
      (list-set! terminal-lines line-number (list->string line))
      (draw-terminal-line (make-string 20 #\space) line-number)
      (draw-terminal-line (list->string line) line-number)
      (wrefresh terminal-win)
      (when (eqv? k #\newline)
        (inc! line-number)
        (insert-line line-number)
        (set! line (list))
        (werase terminal-win)
        (draw-terminal)
        (draw-terminal-line "" line-number))
      (if (eqv? k #\escape) 
        (when (and (null? line) 
                   (> (length terminal-lines) 1))
          (delete-line line-number)
          (werase terminal-win)
          (draw-terminal))
        (loop))))
  (curs_set 0)
  line-number)

(define (clamp a b x)
  (cond
    ((< x a) a)
    ((> x b) b)
    (else x)))

(define (draw-new-log-win)
  (box log-win-border 0 0)
  (wrefresh log-win-border)
  (werase log-win)
  (wrefresh log-win))

(define (add-log . strings)
  (waddstr log-win (apply string-append (append strings (list "\n"))))
  (wrefresh log-win))

(define (read-program1)
  (define str (plc-read-program 64))
  (set! terminal-lines (string->assembly str)))

(define (write-program1)
  (define str (assembly->string (string-intersperse terminal-lines " ")))
  (plc-write-program 64 str))

(define (read-program)
  (draw-new-log-win)
  (handle-exceptions exn
    (begin
      (plc-close)
      (if (error-object? exn)
        (add-log "failed: " (error-object-message exn))
        (add-log "failed."))
      (wgetch log-win))
    (add-log "opening serial port.")
    (plc-open "/dev/ttyUSB0")
    (add-log "testing connection to plc.")
    (unless (plc-hi)
      (error "bad response."))
    (add-log "reading program from plc.")
    (read-program1)
    (add-log "done.")
    (plc-close)
    (wgetch log-win)))

(define (write-program)
  (draw-new-log-win)
  (handle-exceptions exn
    (begin
      (plc-close)
      (if (error-object? exn)
        (add-log "failed: " (error-object-message exn))
        (add-log "failed."))
      (wgetch log-win))
    (add-log "opening serial port.")
    (plc-open "/dev/ttyUSB0")
    (add-log "testing connection to plc.")
    (unless (plc-hi)
      (error "bad response."))
    (add-log "writing program to plc.")
    (write-program1)
    (add-log "done.")
    (plc-close)
    (wgetch log-win)))


(define (navigate-lines)
  (curs_set 0)
  (define line-number 0)

  (ungetch (char->integer #\k))
  (let loop ()
    (let* ([k (wgetch terminal-win)]
           [kcode (if (char? k) (char->integer k) k)]
           [old-line-number line-number])
      (when (and (eqv? k #\d) 
                 (< line-number (length terminal-lines))
                 (> (length terminal-lines) 1))
        (delete-line line-number)
        (werase terminal-win)
        (erase)
        (draw-ladder-protected)
        (draw-terminal))
      (when (eqv? k #\1)
        (read-program)
        (werase terminal-win)
        (erase)
        (draw-ladder-protected)
        (draw-terminal)
        (touchwin (stdscr))
        (touchwin terminal-win)
        (refresh)
        (wrefresh terminal-win))
      (when (eqv? k #\2)
        (write-program)
        (touchwin (stdscr))
        (touchwin terminal-win)
        (refresh)
        (wrefresh terminal-win))
      (when (eqv? k #\k)
        (dec! line-number))
      (when (eqv? k #\j)
        (inc! line-number))
      (when (eqv? k #\o)
        (inc! line-number))
      (when (or (eqv? k #\o)
                (eqv? k #\O))
        (insert-line line-number))
      (wattroff terminal-win A_REVERSE)
      (draw-terminal-line (make-string 20 #\space) old-line-number)
      (when (< old-line-number (length terminal-lines))
        (draw-terminal-line (list-ref terminal-lines old-line-number) old-line-number))
      (when (or (eqv? k #\newline)
                (eqv? k #\o)
                (eqv? k #\O))
        (werase terminal-win)
        (draw-terminal)
        (modify! line-number edit-line)
        (erase)
        (draw-ladder-protected)
        (draw-terminal)
        )
      (modify! line-number (cut clamp 0 (sub1 (length terminal-lines)) <>))
      (wattron terminal-win A_REVERSE)
      (draw-terminal-line (make-string 20 #\space) line-number)
      (draw-terminal-line (list-ref terminal-lines line-number) line-number)
      (wattroff terminal-win A_REVERSE)
      (wmove terminal-win 1 1)
      (wrefresh terminal-win)
      (unless (eqv? k #\q) (loop)))))

(handle-exceptions exn
  (begin (endwin)
         (raise exn))
  (draw-ladder)
  (draw-terminal)
  (navigate-lines)
  (endwin))
