; Actividad Integradora 1: Resaltador de sintaxis
; Sofía Margarita Hernández Muñoz A01655084
; Emiliano Saucedo Arriola A01659258
; Alfonso Pineda Castillo A01660394
; Gael Eduardo Pérez Gómez A01753336

#lang racket/gui
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(define-lex-abbrevs
  ; --> BASICS
  [letter         (union (char-range #\a #\z) (char-range #\A #\Z))]
  [digit          (char-range #\0 #\9)]
  [underscore     #\_]

  ; --> DATA TYPES
  [datatype        (union "int" "float" "void" "bool" "char")]

  ; --> NUMBERS
  ; integer
  [integer        (:: (:? #\-) (repetition 1 +inf.0 digit))]
  ; float
  [floatnumber    (:or pointfloat exponentfloat)]
  [pointfloat     (:or (:: (:?  (:: (:? #\-) intpart)) fraction) (:: intpart "."))]
  [exponentfloat  (:: (:or (:: (:? #\-) intpart) pointfloat) exponent)]
  [intpart        (:+ digit)]
  [fraction       (:: "." (:+ digit))]
  [exponent       (:: (:or "E" "e") (:? (:or "+" "-")) (:+ digit))]

  ; --> BOOLEAN
  [bool        (union "true" "false")]

  ; --> VARIALES
  [identifier     (concatenation (union letter underscore)
                                 (repetition 0 +inf.0 (union letter digit underscore)))]

  ; --> STRINGS
  [char-content   (char-complement (char-set "'\n"))]
  [char-literal   (union (concatenation #\' char-content #\')
                         "'\\n'" "'\\\\'")]
  [string-content (union (char-complement (char-set "\"\n")))]
  [string-literal (union (concatenation #\" (repetition 0 +inf.0 string-content) #\")
                         "\"\\n\"" "\"\\\\\"")]

; --> FUNCTIONS
  [function (concatenation identifier (concatenation "("(complement (concatenation any-string "(" any-string)) ")"))]

  ; --> DIRECTIVES
  [directive       (union "#include")]

  ; --> LIBRARIES
  [library          (concatenation "<" (complement (concatenation any-string ">" any-string)) ">")]

  ; --> STREAMS
  [streams (union "cin" "cout" "cerr" "clog")]

  ; --> KEYWORDS
  [keyword        (union "auto" "break" "case" "const" "continue" "default" "do" "enum" "extern" 
                  "goto" "long" "register" "return" "short" "signed" "sizeof" "static" "struct" 
                  "switch" "typedef" "union" "unsigned" "void" "volatile" "mutable" "private" "true" 
                  "using" "delete" "false" "namespace" "proteced" "template" "try" "virtual" "catch"
                  "friend" "new" "public" "this" "typeid" "class" "explicit" "inline" "operator"
                  "throw" "typename" "endl")] 

  ; --> CONDITIONALS
  [conditional    (union "if" "else" "else if")]

  ; --> LOOPS
  [loop           (union "while" "for")]


  ; --> OPERATOR
  [operator       (union "*" "/" "%" "+" "-" "-"
                         "<" "<=" ">" ">=" "==" "!="
                         "!" "=" "&&" "||" "<<" ">>")]

    ; --> MISC OPERATORS
  [misc (union "sizeOf()" "&" "?:" "->")]

  ; --> DELIMITERS
  [delimiter         (union "(" ")" "{" "}")]

  ; --> SEMICOLON
  [semicolon         (union ";")]

  ; --> COMMA
  [comma (union ",")]

  ; --> COMMENTS
  [comment        (concatenation "/*" (complement (concatenation any-string "*/" any-string)) "*/")])

(define operators-ht
  (hash "*"  'Op_multiply "/"  'Op_divide    "%" 'Op_mod      "+"  'Op_add           "-"  'Op_subtract
        "<"  'Op_less     "<=" 'Op_lessequal ">" 'Op_greater  ">=" 'Op_greaterequal "==" 'Op_equal
        "!=" 'Op_notequal "!"  'Op_not       "=" 'Op_assign   "&&" 'Op_and          "||" 'Op_or
        "<<" 'Left_shift ">>" 'Right_shift))

(define delimiter-ht
  (hash "(" 'LeftParen  ")" 'RightParen
        "{" 'LeftBrace  "}" 'RightBrace))

(define symbols-ht
  (hash ";" 'Semicolon  "," 'Comma))

(define (lexeme->datatype l) (string->symbol (~a "" l))) ; to concatenate "Datatype_lexeme"
(define (lexeme->bool l) (string->symbol (~a "Bool")))   
(define (lexeme->library l) (string->symbol (~a "Library")))            ; 
(define (lexeme->directive  l) (string->symbol (~a "Directive")))
(define (lexeme->streams  l) (string->symbol (~a "streams_" l)))  ; to concatenate "Keyword_lexeme"
(define (lexeme->keyword  l) (string->symbol (~a "Keyword")))  ; to concatenate "Keyword_lexeme"
(define (lexeme->delimiter  l) (string->symbol (~a "Delimiter")))  
(define (lexeme->conditional  l) (string->symbol (~a "Conditional")))
(define (lexeme->loop  l) (string->symbol (~a "Loop_")))  
(define (lexeme->operator l) (hash-ref operators-ht l))           ; return the key of the value in the hashtable
(define (lexeme->misc l) (string->symbol (~a "Misc_Op")))  
(define (lexeme->semicolon   l) (string->symbol (~a "Semicolon")))  
(define (lexeme->comma l) (string->symbol (~a "Comma")))  
(define (lexeme->char     l) (match l
                               ["'\\\\'" #\\]
                               ["'\\n'"  #\newline]
                               [_       (string-ref l 1)]))

(define (token name [value #f])
  (cons name (if value (list value) '())))

(define (lex ip)
  (port-count-lines! ip)
  (define my-lexer
    ; lexer categories
    (lexer-src-pos
     [integer        (token 'Integer (string->number lexeme))]
     [floatnumber    (token 'Float (string->number lexeme))]
     [char-literal   (token 'Char lexeme)] 
     [string-literal (token 'String  lexeme)]
     [datatype       (token (lexeme->datatype  lexeme) lexeme)]
     [bool           (token (lexeme->bool  lexeme) lexeme)]
     [library        (token (lexeme->library  lexeme) lexeme)]
     [directive      (token (lexeme->directive  lexeme) lexeme)]
     [streams        (token (lexeme->streams  lexeme) lexeme)]
     [keyword        (token (lexeme->keyword  lexeme) lexeme)]
     [conditional    (token (lexeme->conditional  lexeme) lexeme)]
     [loop           (token (lexeme->loop  lexeme) lexeme)]
     [operator       (token (lexeme->operator lexeme) lexeme)]
     [misc           (token (lexeme->misc lexeme) lexeme)]
     [delimiter      (token (lexeme->delimiter   lexeme) lexeme)]
     [semicolon      (token (lexeme->semicolon  lexeme) lexeme)]
     [comma          (token (lexeme->comma  lexeme) lexeme)]
     [function       (token 'Function lexeme)]
     [comment        (token 'Comment lexeme)]
     [whitespace     #f]
     [identifier     (token 'Identifier lexeme)]
     [(eof)          (token 'End_of_input)]
     [any-char       (token 'Error lexeme)]))
  (define (next-token) (my-lexer ip))
  next-token)

(define (htmllex ip)
  (port-count-lines! ip)
  (define my-lexer
    (lexer-src-pos
     [integer        (token "<p class='integer'>" (string->number lexeme))]
     [floatnumber    (token "<p class='floatnumber'>" (string->number lexeme))]
     [char-literal   (token "<p class='char-literal'>" lexeme)]
     [string-literal (token "<p class='string-literal'>"  lexeme)]
     [datatype       (token (lexeme->datatype  (~a "<p class='datatype'>")) lexeme)]
     [bool           (token "<p class='bool'>" lexeme)]
     [library        (token "<p class='library'>" lexeme)] 
     [directive      (token "<p class='directive'>" lexeme)]
     [streams        (token "<p class='streams'>" lexeme)]
     [keyword        (token "<p class='keyword'>" lexeme)]
     [conditional    (token "<p class='conditional'>" lexeme)]
     [loop           (token "<p class='loop'>" lexeme)]
     [operator       (token "<p class='operator'>" lexeme)]
     [misc           (token "<p class='misc'>" lexeme)]
     [delimiter      (token "<p class='delimiter'>" lexeme)]
    ;  [symbol         (token (lexeme->symbol   lexeme) lexeme)]
     [semicolon      (token "<p class='semicolon'>" lexeme)]
     [comma          (token "<p class='comma'>" lexeme)]
     [function       (token "<p class='function'>" lexeme)]
     [comment        (token "<p class='comment'>" lexeme)]
     [whitespace     #f]
     [identifier     (token "<p class='variable'>" lexeme)]
     [(eof)          (token 'End_of_input)]
     ; [any-char       (token "<p class='error'>" lexeme)]))
     [any-char       (token 'Error lexeme)]))
  (define (next-token) (my-lexer ip))
  next-token)

(define (string->tokens s)
  (port->tokens (open-input-string s)))

(define (string->tokenshtml s)
  (port->tokenshtml (open-input-string s)))

(define (string->tokenstable s)
  (port->tokenstable (open-input-string s)))

(define (port->tokens ip)
  (define next-token (lex ip))
  (let loop ()
    (match (next-token)
      [(position-token t (position offset line col) _)
       (set! col (+ col 1)) ; output is 1-based
       (match t
         [#f                   (loop)] ; skip whitespace
         [(list 'End_of_input) (list (list line col 'End_of_input))]
         [(list name value)    (cons (list line col name value) (loop))]
         [(list name)          (cons (list line col name)       (loop))]
         [_ (error)])])))

(define (port->tokenstable ip)
  (define next-token (lex ip))
  (let loop ()
    (match (next-token)
      [(position-token t (position offset line col) _)
       (set! col (+ col 1)) ; output is 1-based
       (match t
         [#f                   (loop)] ; skip whitespace
         [(list 'End_of_input) (list (list "<br>"))]
         [(list name value)    (cons (list "<tr>" "<td>" line "</td>" "<td>" col "</td>" "<td>" name "</td>" "<td>" value "</td>" "</tr>") (loop))]
         [(list name)          (cons (list line col name)       (loop))]
         [_ (cons (list error line col))])])))

(define (port->tokenshtml ip)
  (define next-token (htmllex ip))
  (let loop ()
    (match (next-token)
      [(position-token t (position offset line col) _)
       (set! col (+ col 1)) ; output is 1-based
       (match t
         [#f                   (loop)] ; skip whitespace
         [(list 'End_of_input) (list (list "</div>"))]
         [(list 'Error name)   (list (list "<p class='error'>" line "\t"col "\t"name"</p>") (loop))]
         [(list name value)    (cons (list name value "</p>") (loop))]
         [(list name)          (cons (list line col name)       (loop))]
         [_ (cons (list error line col))])])))

(define (display-tokens ts)
  (for ([t ts])
    (for ([x t])
      (display x) (display "\t\t"))
    (newline)))
;--------------GUI----------------

;; Make a frame by instantiating the frame% class
(define frame (new frame% [label "Example"]))

;; Make a button in the frame
(new button% [parent frame]
             [label "Select File"]
             ;; Callback procedure for a button click:
             [callback
              (lambda (button event)
                (define inFile (get-file))
                (port->string (open-input-file inFile))
                (display inFile)
(define outFile "out.html")

; Call the lexer
;(display-tokens (string->tokens (port->string (open-input-file inFile))))

; Output file
(define out (open-output-file outFile))


(display "<!DOCTYPE html>\n\n" out)

(display "<html lang='es'>\n\n" out)


(display "<head>\n\n" out)


(display "<meta charset='UTF-8'>\n\n" out)
(display "<meta http-equiv='X-UA-Compatible' content='IE=edge'>\n\n" out)
(display "<meta name='viewport' content='width=device-width, initial-scale=1.0'>\n\n" out)
(display "<title>Lexer result</title>\n\n" out)

(display "<link rel='stylesheet' href='style.css'>\n\n" out)

(display "<script type='text/javascript' href='style.js'> </script>\n\n" out)

(display "</head>\n\n" out)

(display "<body>\n\n" out)

(display "<h1> Syntax highlighter </h1>" out)

(display "<div class='resaltador'>\n\n" out)

(display-lines (string->tokenshtml (port->string (open-input-file inFile))) out)

(display "<h1> Table </h1>" out)

(display "<table>\n\n" out)

(display "<tr> <th>Line</th> <th>Character</th> <th>Type</th> <th>Value</th> </tr>" out)

(display-lines (string->tokenstable (port->string (open-input-file inFile))) out)

(display "</table>\n\n" out)

(display "</div>\n\n" out)
(display "<h1> Console </h1>" out)
(display "<br>\n\n" out)

(display "</body> \n\n" out)


(display "<script type='text/javascript'>  \n\n
    var el = document.getElementsByClassName('error'); \n\n
    console.log(el) \n\n
    console.log(el.length) \n\n
    for (var i = 0; i < el.length; i++) { \n\n
        var currentEl = el[i]; \n\n
        currentEl.style.bottom = -(i * 2) + 'rem'; \n\n
        console.log(currentEl) \n\n
    } \n\n
    var body_element = document.querySelector('body'); \n\n
    body_element.innerHTML = body_element.innerHTML.replaceAll('(', ''); \n\n
    body_element.innerHTML = body_element.innerHTML.replaceAll(')', ''); \n\n
</script> \n\n" out)

(display "</html>\n\n" out)

; Close the file
(close-output-port out)
                
                )])

;; Show the frame by calling its show method
(send frame show #t)

; ----------------FILES----------------

;(define inFile "micodigo_bien.cpp")  ; Correct file
;(define inFile "micodigo.cpp")  ; Incorrect file
