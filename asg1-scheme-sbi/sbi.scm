#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.3 2016-09-23 18:23:20-07 - - $
;;
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.  Currently it is only printed.
;;

(define *stderr* (current-error-port))

(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)

(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)

(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)

(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))

(define (write-program-by-line filename program)
    (printf "==================================================~n")
    (printf "~a: ~s~n" *run-file* filename)
    (printf "==================================================~n")
    (printf "(~n")
    (map (lambda (line) (printf "~s~n" line)) program)
    (printf ")~n"))

(define *function-table* (make-hash))
(define (function-get key)
		(hash-ref *function-table* key))
(define (function-put! key value)
		(hash-set! *function-table* key value))
		
(define *label-table* (make-hash))
(define (label-get key)
		(hash-ref *label-table* key))
(define (label-put! key value)
		(hash-set! *label-table* key value))
		
(define *variable-table* (make-hash))
(define (variable-get key)
		(hash-ref *variable-table* key))
(define (variable-put! key value)
		(hash-set! *variable-table* key value))

(define length (lambda (l)
	(define length..(lambda (l.. n)
		(if(null? l..) n
			(length.. (cdr l..)(+ n 1)))))
	(length.. l 0)))

(define (value l)
	(if(pair? l)
		(apply (function-get (car l)) (map value (cdr l)))
		(cond((number? l) l)
			(else(variable-get l)))))

(define (sb-print tok) 
   (if (not (null?  tok) )
     (begin
          (if (string? (car tok))  
            (display (car tok))
            (display (value (car tok)))      
          )         
          (sb-print (cdr tok))
      )
         (newline)))
			
(define (sb-input in)
	(variable-put! 'inputcount 0)
	(define (sb-input.. in)
		(when (not (null? (car in)))
			(variable-put! (car in)(void))
			(let ((object(read)))
				(cond[(eof-object? object)(variable-put! 'inputcount -1)]
					[(number? object)(variable-put!(car in) object)
					(variable-put! 'inputcount (+ (variable-get 'inputcount) 1))]
				[else(begin (printf "ERROR: non number (~a~n)" object))]))
			(when (not (null? (cdr in)))
				(sb-input.. (cdr in)))))
	(sb-input.. in))

(define (sb-dim expr)
  (variable-put! (caar expr) (make-vector (value (cadar expr))) )
  (function-put! (caar expr) 
      (lambda(x) (vector-ref (variable-get (caar expr)) (- x 1)))))
	
(define (sb-let expr)
  (if (pair? (car expr))
    (begin
     (vector-set! (variable-get
        (caar expr)) (- (value (cadar expr)) 1) (value (cadr expr)))
    )
    (begin
     (let ((result (value (cadr expr))))
       (variable-put! (car expr) result)
     ))))
			
(define (sb-goto label prog)
	(exec-lines prog (label-get (car label))))
			
(for-each
    (lambda (pair)
            (function-put! (car pair) (car(cdr pair))))
    `(
        (log10_2 0.301029995663981195213738894724493026768189881)
        (sqrt_2  1.414213562373095048801688724209698078569671875)
        (e       2.718281828459045235360287471352662497757247093)
        (pi      3.141592653589793238462643383279502884197169399)
        (/       ,(lambda (x y)  (/ x (if (equal? y 0) 0.0 y))))
        (log10   ,(lambda (x) (/ (log x) (log 10.0))))
		(log     ,(lambda(x)(log (if (equal? x 0) 0.0 x))))
        (%       ,(lambda (x y) (- x (* (div x y) y))))
        (quot    ,(lambda (x y) (truncate (/ x y))))
        (rem     ,(lambda (x y) (- x (* (quot x y) y))))
		(<       ,(lambda (x y) (< x y)))
		(>       ,(lambda (x y) (> x y)))
		(<=      ,(lambda (x y) (<= x y)))
		(=       ,(lambda (x y) (< x y)))
		(>=      ,(lambda (x y) (>= x y)))
		(<>      ,(lambda (x y) (not(equal? x y))))
        (+       ,+)
		(-       ,-)
		(*       ,*)
        (^       ,expt)
        (ceil    ,ceiling)
        (exp     ,exp)
        (floor   ,floor)
		(abs     ,abs)
		(round   ,round)
        (sqrt    ,sqrt)
		(sin     ,sin)
		(cos     ,cos)
		(tan     ,tan)
		(asin    ,asin)
		(acos    ,acos)
		(atan    ,atan)
		(dim     ,sb-dim)
		(goto    ,sb-goto)
		(input   ,sb-input)
		(let     ,sb-let)
		(print   ,sb-print)
		(if      ,(void))))

(for-each
    (lambda (pair)
            (variable-put! (car pair) (cadr pair)))
    `(
        (inputcount 0)
        (pi      3.141592653589793238462643383279502884197169399)
        (e       2.718281828459045235360287471352662497757247093)
     ))
		
(define (func instr program line-nr)
(if (null? instr)
  (exec-lines program (+ line-nr 1))
  (begin
  (when (not (hash-has-key? *function-table* (car instr )))
        (display (car instr))(display " is not valid")(newline)
         (usage-exit))
  (cond
    ((eqv? (car instr) 'goto)
      (exec-lines program (- (label-get (cadr instr)) 1))
    )
    ((eqv? (car instr) 'if)
      (if (equal? #t (value (cadr instr)))
        (exec-lines program (- (label-get (caddr instr)) 1))
        (exec-lines program (+ line-nr 1))
      )
    )  
    (else
      ((function-get (car instr)) (cdr instr))
      (exec-lines program (+ line-nr 1))
    )))))

(define (eval-labels list)
(when (not (null? list))
              (let ((first (caar list)))
                   (when (number? first)
                         (if (not (null? (cdar list)))
                             (if(not (symbol? (cadar list)))
                                (void)
                                (begin
                                (label-put! (cadar list) (caar list))
                                ))
                             (void))))
              (eval-labels (cdr list))))

(define (exec-lines program line-nr)
        (when (< line-nr (length program))
          (let ((line (list-ref program line-nr)))
             (cond
               ((= (length line) 3) 
                 (set! line (cddr line))
                 (func (car line) program line-nr)
               )
               ((and (= (length line) 2) (list? (cadr line)))
               (set! line (cdr line))
               (func (car line) program line-nr)
               )           
               (else
                  (exec-lines program (+ line-nr 1)))
             ))))

			 

(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile)))
               (begin(eval-labels program)
                     (exec-lines program 0)
               ))))


(main (vector->list (current-command-line-arguments)))
