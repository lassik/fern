(use srfi-69)

;; Globals

(define inp (open-input-file (car (command-line-arguments))))

;; Utils

(define (accumulate generate)
  (let rec ((acc '()))
    (let ((val (generate)))
      (if (not val) (reverse acc) (rec (cons val acc))))))

(define (=p k)
  (lambda (c) (eqv? c k)))

;; Keywords

(define pipe 'pipe)
(define getter 'getvar)
(define splice (gensym "splice"))
(define call (gensym "call"))
(define listlit 'list)
(define body (gensym "body"))

;; Syntax char classes

(define (eol? c)
  (or (eof-object? c) (eqv? #\newline c)))

(define (bare-char? c)
  (char-alphabetic? c))

(define (white-char? c)
  (and (char-whitespace? c) (not (eol? c))))

;; Tokenizer

(define (rc? p)
  (if (p (peek-char inp))
      (read-char inp)
      #f))

(define (rc* p)
  (let rec ((ans #f))
    (let ((c (rc? p)))
      (if (not c) ans (rec (string-append (or ans "") (string c)))))))

(define (skip-comment)
  (if (not (rc? (=p #\#)))
      #f
      (let rec ()
	(if (rc? eol?)
	    #t
	    (begin (rc? (lambda (c) #t))
		   (rec))))))

(define (skip-whitespace)
  (let rec ()
    (if (or (rc* white-char?) (skip-comment))
	(rec)))
  #f)

(define (read-bareword)
  (rc* bare-char?))

(define (read-getter)
  (if (not (rc? (=p #\$)))
      #f
      (let ((head (if (rc? (=p #\@)) splice getter)))
	(let ((word (read-bareword)))
	  (if (not word)
	      (error "No word after $")
	      (list head word))))))

(define (read-value)
  (or (read-list #\( #\) #f)
      (read-list #\[ #\] listlit)
      (read-list #\{ #\} body)
      (read-getter)
      (read-bareword)
      (error "No value could be read at point")))

(define (list->lists newlist lists)
  (cond ((null? newlist)
	 lists)
	((null? (cdr newlist))
	 (cons (car newlist) lists))
	(else
	 (cons (reverse newlist) lists))))

(define (pipeify cur-pipes done-lists)
  (cond ((null? cur-pipes)
	 done-lists)
	((null? (cdr cur-pipes))
	 (cons (car cur-pipes) done-lists))
	(else
	 (cons (cons pipe (reverse cur-pipes))
	       done-lists))))

(define (read-list open-char close-char head)
  (if (and open-char (not (rc? (=p open-char))))
      #f
      (let ((resulting-list
	     (let rec ((cur-list '()) (cur-pipes '()) (done-lists '()))
	       (define (finish-pipeline)
		 (let ((cur-pipes (list->lists cur-list cur-pipes)))
		   (pipeify cur-pipes done-lists)))
	       (skip-whitespace)
	       (cond ((rc? (if (eqv? close-char #f) eof-object? (=p close-char)))
		      (reverse (finish-pipeline)))
		     ((or (rc? (=p #\;)) (rc? (=p #\newline)))
		      (rec '() '() (finish-pipeline)))
		     (else
		      (if (rc? (=p #\|))
			  (rec '()
			       (list->lists cur-list cur-pipes)
			       done-lists)
			  (rec (cons (read-value) cur-list)
			       cur-pipes
			       done-lists)))))))
	(if head
	    (cons head resulting-list)
	    (cond ((null? resulting-list)
		   resulting-list)
		  ((null? (cdr resulting-list))
		   (car resulting-list))
		  (else
		   resulting-list))))))

(define (read-file)
  (read-list #f #f "toplevel"))

(define builtins (make-hash-table))

(define (evaluate form)
  ;;(display "in evaluate ") (display form) (newline)
  (if (not (pair? form))
      form
      (let ((handler (hash-table-ref builtins (car form))))
	(apply handler (list (cdr form))))))

(define-syntax defbuiltin
  (syntax-rules ()
    ((_ name (args) body ...)
     (hash-table-set! builtins (symbol->string 'name)
		      (lambda (args) body ...)))))

(defbuiltin pipe (commands)
  a a)

(defbuiltin echo (args)
  (let rec ((args args))
    (if (null? args)
	(newline)
	(begin (display (car args))
	       (if (cdr args)
		   (begin (display " ")
			  (rec (cdr args))))))))

(defbuiltin toplevel (commands)
  (let rec ((commands commands) (ans #f))
    (if (null? commands)
	ans
	(rec (cdr commands) (evaluate (car commands))))))

(evaluate (read-file))
