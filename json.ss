(define l-curly #\{)
(define r-curly #\})
(define l-brace #\[)
(define r-brace #\])
(define colon #\:)
(define quote-mark #\")
(define comma #\,)
(define minus #\-)
(define plus #\+)
(define period #\.)

(define transcoder (make-transcoder (utf-8-codec)
				    (native-eol-style)
				    (error-handling-mode raise)))

(define get-port
  (lambda (path)
    (open-file-input-port path
			  (file-options)
			  (buffer-mode line)
			  transcoder)))

(define whitespace?
  (lambda (c)
    (or (char=? c #\space)
	(char=? c #\linefeed)
	(char=? c #\return)
	(char=? c #\tab))))

(define value-delim?
  (lambda (c)
    (or (whitespace? c)
	(char=? c comma)
	(char=? c r-brace)
	(char=? c r-curly))))

(define make-token
  (lambda (type value)
    (cons type value)))

(define make-string-token
  (lambda (port)
    (make-token 'string
		(list->string (get-char-list port)))))

(define get-char-list
  (lambda (port)
    (let ([c (get-char port)])
      (if (or (char=? c quote-mark) (eof-object? c))
	  '()
	  (cons c (get-char-list port))))))

(define make-number-token
  (lambda (port first-char)
    (if (and (sign-char? first-char)
	     (not (char-numeric? (peek-char port))))
	(error make-number-token
	       "Invalid number prefix"
	       (string first-char (peek-char port)))
	(let* ([digit-part (get-numeric-char-list port)]
	       [fraction (get-fraction-char-list port)]
	       [exponent (get-exponent-char-list port)]
	       [number-string
		(list->string (cons first-char
				    (append digit-part fraction exponent)))]
	       [next (peek-char port)])
	  (if (not (value-delim? next))
	      (error make-number-token "Invalid number format" number-string)
	      (make-token 'number number-string))))))

(define get-numeric-char-list
  (lambda (port)
    (let ([next (peek-char port)])
      (if (char-numeric? next)
	  (cons (get-char port)
		(get-numeric-char-list port))
	  '()))))

(define get-fraction-char-list
  (lambda (port)
    (let ([next (peek-char port)])
      (if (not (char=? next period))
	  '()
	  (cons (get-char port)
		(get-numeric-char-list port))))))

(define get-exponent-char-list
  (lambda (port)
    (let ([next (peek-char port)])
      (if (or (char=? next #\e))
	  (cons (get-char port) (get-numeric-char-list port))
	  '()))))

(define valid-symbolic?
  (lambda (s)
    (or (string=? s "true")
	(string=? s "false")
	(string=? s "null"))))

(define make-symbol-token
  (lambda (port first-char)
    (define loop
      (lambda ()
	(let* ([current (get-char port)]
	       [next (lookahead-char port)])
	  (if (not (char-alphabetic? next))
	      (cons current '())
	      (cons current (loop))))))
    (let ([s (list->string (cons first-char (loop)))])
      (if (not (valid-symbolic? s))
	  (error make-symbol-token "Invalid value" s))
      (make-token 'symbol s))))

(define sign-char?
  (lambda (c) (or (char=? c plus) (char=? c minus))))

(define get-tokens
  (lambda (port)
    (let ([c (get-char port)])
      (cond ((eof-object? c) '())
	    ((char=? c l-curly) (cons (make-token 'l-curly c)
				      (get-tokens port)))
	    ((char=? c r-curly) (cons (make-token 'r-curly c)
				      (get-tokens port)))
	    ((char=? c quote-mark) (cons (make-string-token port)
					 (get-tokens port)))
	    ((char=? c colon) (cons (make-token 'colon c)
				    (get-tokens port)))
	    ((char=? c r-brace) (cons (make-token 'r-brace c)
				      (get-tokens port)))
	    ((char=? c l-brace) (cons (make-token 'l-brace c)
				      (get-tokens port)))
	    ((char=? c comma) (cons (make-token 'comma c)
				    (get-tokens port)))
	    ((char-alphabetic? c) (cons (make-symbol-token port c)
					(get-tokens port)))
	    ((or (sign-char? c) (char-numeric? c))
	     (cons (make-number-token port c)
		   (get-tokens port)))
	    (else (get-tokens port))))))

(define test
  (lambda ()
    (let ((port (get-port "~/projects/scheme/example1.json")))
      (let ((tokens (get-tokens port)))
	(for-each (lambda (x) (display x) (newline)) tokens)
	(close-port port)))))

(test)
