; Actividad Integradora 1: Resaltador de sintaxis
; Sofía Margarita Hernández Muñoz A01655084
; Emiliano Saucedo Arriola A01659258
; Alfonso Pineda Castillo A01660394
; Gael Eduardo Pérez Gómez A01753336

; TODO
; https://www.codewithharry.com/blogpost/cpp-cheatsheet
; - libraries
; - dividir keywords en ciclos y condicionales
; - definir una función
; - isnumber(char) vemos...
; - int main() ?
; - Reubicar el cout cin y endl (no es keyword sino variable)

#lang racket
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
  ; [function (concatenation (:: identifier)(delimiter))]

  ; --> KEYWORDS
  ; [keyword        (union "if" "else" "while" "print" "putc")]
  [keyword        (union "auto" "break" "case" "const" "continue" "default" "do" "enum" "extern"
                         "goto" "long" "register" "return" "short" "signed" "sizeof" "static" "struct"
                         "switch" "typedef" "union" "unsigned" "void" "volatile" "mutable" "private" "true"
                         "using" "delete" "false" "namespace" "proteced" "template" "try" "virtual" "catch"
                         "friend" "new" "public" "this" "typeid" "class" "explicit" "inline" "operator"
                         "throw" "typename" "cout" "cin" "endl")]

  ; --> CONDITIONALS
  [conditional    (union "if" "else" "else if")]

  ; --> LOOPS
  [loop           (union "while" "for")]


  ; --> OPERATOR
  [operator       (union "*" "/" "%" "+" "-" "-"
                         "<" "<=" ">" ">=" "==" "!="
                         "!" "=" "&&" "||" "<<" ">>")]

  ; --> DELIMITERS
  [delimiter         (union "(" ")" "{" "}")]

  ; --> SYMBOLS
  [symbol         (union ";" ",")]

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
(define (lexeme->bool l) (string->symbol (~a "Bool")))            ;
(define (lexeme->keyword  l) (string->symbol (~a "Keyword")))  ; to concatenate "Keyword_lexeme"
(define (lexeme->delimiter  l) (string->symbol (~a "Delimiter")))
(define (lexeme->conditional  l) (string->symbol (~a "Conditional")))
(define (lexeme->loop  l) (string->symbol (~a "Loop_")))
(define (lexeme->operator l) (hash-ref operators-ht l))           ; return the key of the value in the hashtable
(define (lexeme->symbol   l) (hash-ref symbols-ht   l))           ; key NAME lexer
(define (lexeme->char     l) (match l
                               ["'\\\\'" #\\]
                               ["'\\n'"  #\newline]
                               [_       (string-ref l 1)]))

(define (token name [value #f])
  (cons name (if value (list value) '())))

(define (lex ip)
  (port-count-lines! ip)
  (define my-lexer
    ; lexer categories (8 NO whitespace)
    (lexer-src-pos
     [integer        (token 'Integer (string->number lexeme))]
     [floatnumber    (token 'Float (string->number lexeme))]
     [char-literal   (token 'Char lexeme)]
     [string-literal (token 'String  lexeme)]
     [datatype       (token (lexeme->datatype  lexeme) lexeme)]
     [bool           (token (lexeme->bool  lexeme) lexeme)]
     [keyword        (token (lexeme->keyword  lexeme) lexeme)]
     [conditional    (token (lexeme->conditional  lexeme) lexeme)]
     [loop           (token (lexeme->loop  lexeme) lexeme)]
     [operator       (token (lexeme->operator lexeme) lexeme)]
     [delimiter      (token (lexeme->delimiter   lexeme) lexeme)]
     [symbol         (token (lexeme->symbol   lexeme) lexeme)]
     ;  [function       (token (lexeme->bool  lexeme) lexeme)]
     [comment        (token 'Comment lexeme)]
     [whitespace     #f]
     [identifier     (token 'Variable lexeme)]
     [(eof)          (token 'End_of_input)]
     [any-char       (token 'Error lexeme)]))
  (define (next-token) (my-lexer ip))
  next-token)

(define (htmllex ip)
  (port-count-lines! ip)
  (define my-lexer
    ; lexer categories (8 NO whitespace)
    (lexer-src-pos
     [integer        (token "<p class='integer'> Integer" (string->number lexeme))]
     [floatnumber    (token "<p class='floatnumber'> Float" (string->number lexeme))]
     [char-literal   (token "<p class='char-literal'> Char" lexeme)]
     [string-literal (token "<p class='string-literal'> String"  lexeme)]
     [datatype       (token (lexeme->datatype  (~a "<p class='datatype'>Datatype" lexeme )) lexeme)]
     [bool           (token "<p class='bool'> Boolean" lexeme)]
     [keyword        (token "<p class='keyword'> Keyword" lexeme)]
     [conditional    (token "<p class='conditional'> Conditional" lexeme)]
     [loop           (token "<p class='loop'> Loop" lexeme)]
     [operator       (token "<p class='operator'> Op_assign" lexeme)]
     [delimiter      (token "<p class='delimiter'> Delimiter" lexeme)]
     [symbol         (token (lexeme->symbol   lexeme) lexeme)]
     ;  [function       (token (lexeme->bool  lexeme) lexeme)]
     [comment        (token "<p class='comment'> Comment" lexeme)]
     [whitespace     #f]
     [identifier     (token "<p class='variable'> Variable" lexeme)]
     [(eof)          (token 'End_of_input)]
     [any-char       (token "<p class='error'> Error" lexeme)]))
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
         [(list 'End_of_input) (list (list line col 'End_of_input))]
         [(list name value)    (cons (list "<tr>" "<td>" name "</td>" "<td>" value "</td>" "</tr>") (loop))]
         [(list name)          (cons (list line col name)       (loop))]
         [_ (error)])])))

(define (port->tokenshtml ip)
  (define next-token (htmllex ip))
  (let loop ()
    (match (next-token)
      [(position-token t (position offset line col) _)
       (set! col (+ col 1)) ; output is 1-based
       (match t
         [#f                   (loop)] ; skip whitespace
         [(list 'End_of_input) (list (list line col 'End_of_input))]
         [(list name value)    (cons (list name value line col "</p>") (loop))]
         [(list name)          (cons (list line col name)       (loop))]
         [_ (error)])])))

; (define test7 #<<TEST
; /* This is a comment */
; int test1 = 5;
; float test2 = 2.2;

; bool test3 = true;
; bool test4 = false;
; if (test 1 == 5){
;   cout << "True" << endl;
; }else{
;   cout << "False" << endl;
; }
; TEST

;   )

(define (display-tokens ts)
  (for ([t ts])
    (for ([x t])
      (display x) (display "\t\t"))
    (newline)))

; "TEST"
; (display-tokens (string->tokens test7))

; ----------------ARCHIVOS----------------

; Llamamos al lexer
(display-tokens (string->tokens (port->string (open-input-file "micodigo.txt"))))

; Creamos el archivo de salida
(define out (open-output-file "data.html"))


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


(display "<div class='resaltador'>\n\n" out)

; Se añade la salida del lexer (AQUI HACER QUE SE AGREGUE LA CLASE Y LOS DIV)
(display-lines (string->tokenshtml (port->string (open-input-file "micodigo.txt"))) out)

(display "<table>\n\n" out)

(display "<tr> <th>Type</th> <th>Value</th> </tr>" out)

(display-lines (string->tokenstable (port->string (open-input-file "micodigo.txt"))) out)

(display "</table>\n\n" out)

(display "</div>\n\n" out)
(display "<br>\n\n" out)
(display "<br>\n\n" out)
(display "<br>\n\n" out)
(display "<br>\n\n" out)
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


; Cerramos el archivo
(close-output-port out)
