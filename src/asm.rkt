#lang racket

; Temporary design.

(define category->ternary/hash
  #hash([I/O . (TAB LF)]
        [Stack . (SP)]
        [Arithmetic . (TAB SP)]
        [Control . (LF)]
        [Heap . (TAB TAB)]))

(define category->ternary (curry hash-ref category->ternary/hash))

(define category->commands/hash
  #hash([I/O . (getchar getnum putchar putnum)]
        [Stack . (push dup swap pop (copy cpy) slide)]
        [Arithmetic . (add (sub comp!) (mul mult) (div quo quot) (mod rem))]
        [Control . (setlabel call (jump jmp) (j0 jz jzero je jequal) (jneg jl jless) ret end)]
        [Heap . (store retrieve)]))

(define category->commands (curry hash-ref category->commands/hash))

(define (command->category command)
  (define (matches name-or-list)
    ((cond [(symbol? name-or-list) eq?]
           [(list? name-or-list) memq]
           [else (curry error "Unrecognized command specification type")])
     command
     name-or-list))
  (for/first ([(k v) (in-hash category->commands/hash)]
              #:when (ormap matches v))
    k))

(define command->ternary/hash
  #hash([getchar . (TAB SP)]
        [getnum . (TAB TAB)]
        [putchar . (SP SP)]
        [putnum . (SP TAB)]

        [push . (SP)]
        [dup . (LF SP)]
        [swap . (LF TAB)]
        [pop . (LF LF)]
        [copy . (TAB SP)]
        [cpy . (TAB SP)]
        [slide . (TAB LF)]

        [add . (SP SP)]
        [sub . (SP TAB)]
        [comp! . (SP TAB)]
        [mul . (SP LF)]
        [mult . (SP LF)]
        [div . (TAB SP)]
        [quo . (TAB SP)]
        [quot . (TAB SP)]
        [mod . (TAB TAB)]
        [rem . (TAB TAB)]

        [setlabel . (SP SP)]
        [call . (SP TAB)]
        [jump . (SP LF)]
        [jmp . (SP LF)]
        [j0 . (TAB SP)]
        [jz . (TAB SP)]
        [jzero . (TAB SP)]
        [je . (TAB SP)]
        [jequal . (TAB SP)]
        [jneg . (TAB TAB)]
        [jl . (TAB TAB)]
        [jless . (TAB TAB)]
        [ret . (TAB LF)]
        [end . (LF LF)]

        [store . (SP)]
        [retrieve . (TAB)]))

(define command->ternary (curry hash-ref command->ternary/hash))

(define command-arg-type/hash
  #hash([push . number]
        [copy . number]
        [cpy . number]
        [slide . number]

        [setlabel . label]
        [call . label]
        [jump . label]
        [jmp . label]
        [j0 . label]
        [jz . label]
        [jzero . label]
        [je . label]
        [jequal . label]
        [jneg . label]
        [jl . label]
        [jless . label]))

(define (command-arg-type command)
  (hash-ref command-arg-type/hash command #f))

(define (optional-arg->ternary arg-type arg)
  (define (integer->binary-string i)
    (unless (integer? i)
      (error "Argument not an integer"))
    (number->string i 2))
  (define (clean-integer arg)
    (cond [(integer? arg) arg]
          [(char? arg) (char->integer arg)]
          [else (error "Argument should be integer or characters")]))
  (define (binary-mapping ch)
    (match ch
      [#\+ 'SP]
      [#\- 'TAB]
      [#\0 'SP]
      [#\1 'TAB]
      [_ (error)]))
  (define (binary-string->ternary s)
    (for/list ([ch (in-string s)]) (binary-mapping ch)))
  (define (signed->ternary num)
    (let* ([s (integer->binary-string num)]
           [ls (binary-string->ternary s)]  ; TODO: check only 0, 1, -
           [with-end (append ls '(LF))])
      ; add + sign for nonnegative integers
      (if (nonnegative-integer? num)
          (cons 'SP with-end)
          with-end)))
  (define (byte->binary-string b)
    (let ([s (integer->binary-string b)])
      (string-append (make-string (- 8 (string-length s)) #\0) s)))
  (define (text->binary-string s)
    (string-append* (for/list ([b (in-bytes (string->bytes/utf-8 s))])
                      (byte->binary-string b))))
  (define (bin->binary-string s)
    (unless (regexp-match? #rx"^[01]*$" s)
      (error "bin string must contain only 0 and 1:" s))
    s)
  (define (label-element->binary-string elem)
    (match elem
      [(list 'bin s)
       (unless (string? s)
         (error "bin expects a string argument:" elem))
       (bin->binary-string s)]
      [(list 'text s)
       (unless (string? s)
         (error "text expects a string argument:" elem))
       (text->binary-string s)]
      [(? integer? n)
       (unless (positive? n)
         (error "Integer label elements must be positive:" n))
       (integer->binary-string n)]
      [(? char? c)
       (error "Characters are not allowed as label elements; use (text \"...\") instead")]
      [(? string? s)
       (error "Ambiguous string label element; wrap it in (bin \"...\") or (text \"...\"):" s)]
      [_
       (error "Unrecognized label element:" elem)]))
  (define (label-args->ternary args)
    (let* ([elements
            (if (and (= 1 (length args))
                     (pair? (car args))
                     (eq? (caar args) 'label))
                (cdar args)
                args)]
           [bits (string-append* (map label-element->binary-string elements))])
      (append (binary-string->ternary bits) '(LF))))

  (match arg-type
    [#f '()]
    ['number
     (signed->ternary (clean-integer (car arg)))]
    ['label
     (label-args->ternary arg)]
    [_
     (error "Unrecognized argument type")]))

(define (statement->ternary statement)
  (let ([command (first statement)]
        [arg (rest statement)])
    ; handle comment
    (if (memq command '(comment //))
        '()
        (let ([imp-ternary (category->ternary (command->category command))]
              [command-ternary (command->ternary command)]
              [arg-type (command-arg-type command)])
          (append imp-ternary command-ternary (optional-arg->ternary arg-type arg))))))

(define (statements->ternary statements)
  (append* (map statement->ternary statements)))

(define (ternary->whitespace ternary)
  (define (single-mapping sym)
    (match sym
      ['SP #\space]
      ['TAB #\tab]
      ['LF #\newline]
      [_ (error "Unrecognized ternary element")]))
  (apply string (map single-mapping ternary)))

(define statements->whitespace (compose ternary->whitespace statements->ternary))

(module+ main
  (define (usage [code 1])
    (displayln "Usage: racket src/asm.rkt [INPUT] [-o OUTPUT]")
    (displayln "  Assemble Whitespace statements into a Whitespace program.")
    (displayln "  INPUT (default: stdin; \"-\" also means stdin) holds s-expressions,")
    (displayln "  either one list of statements like  ((push #\\H) (putchar))")
    (displayln "  or several top-level statements, e.g. (push #\\H) (putchar)")
    (displayln "  OUTPUT (default: stdout) receives the raw Whitespace text.")
    (exit code))

  (define (parse-args args)
    (let loop ([rest args] [input #f] [output #f])
      (cond
        [(null? rest) (values input output)]
        [(member (car rest) '("-h" "--help")) (raise 'help)]
        [(equal? (car rest) "-o")
         (unless (and (pair? (cdr rest))
                      (not (member (cadr rest) '("-o" "-h" "--help"))))
           (raise 'usage))
         (when output (raise 'usage))
         (loop (cddr rest) input (cadr rest))]
        [input (raise 'usage)]
        [else (loop (cdr rest) (car rest) output)])))

  (define (read-program port)
    (define (read-all)
      (let loop ()
        (define x (read port))
        (if (eof-object? x) '() (cons x (loop)))))
    (define datums (read-all))
    (cond
      [(null? datums) '()]
      [(and (null? (cdr datums)) (null? (car datums))) '()]
      ;; single datum that is itself a list of statements, e.g. ((push #\H) (putchar))
      [(and (null? (cdr datums))
            (pair? (car datums))
            (pair? (caar datums)))
       (car datums)]
      ;; otherwise each top-level datum is one statement
      [else datums]))

  (define (run)
    (define-values (input output)
      (parse-args (vector->list (current-command-line-arguments))))
    (define in-port (if (and input (not (equal? input "-")))
                        (open-input-file input)
                        (current-input-port)))
    (define statements
      (dynamic-wind
        void
        (lambda () (read-program in-port))
        (lambda () (unless (eq? in-port (current-input-port))
                     (close-input-port in-port)))))
    (define ws (statements->whitespace statements))
    (define out-port (if output
                         (open-output-file output #:exists 'truncate/replace)
                         (current-output-port)))
    (dynamic-wind
      void
      (lambda () (write-bytes (string->bytes/utf-8 ws) out-port))
      (lambda () (unless (eq? out-port (current-output-port))
                   (close-output-port out-port))))
    (void))

  (with-handlers ([(lambda (e) (eq? e 'help)) (lambda (e) (usage 0))]
                  [(lambda (e) (eq? e 'usage)) (lambda (e) (usage))]
                  [exn:fail? (lambda (e)
                               (eprintf "asm: ~a~n" (exn-message e))
                               (exit 1))])
    (run)))
