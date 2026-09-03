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
  (let* ([command (first statement)]
         [arg (rest statement)]
         [imp-ternary (category->ternary (command->category command))]
         [command-ternary (command->ternary command)]
         [arg-type (command-arg-type command)])
    (append imp-ternary command-ternary (optional-arg->ternary arg-type arg))))
