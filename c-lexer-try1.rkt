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

  ; --> KEYWORDS
  [keyword        (union "if" "else" "while" "print" "putc")]

  ; --> OPERATOR
  [operator       (union "*" "/" "%" "+" "-" "-"
                         "<" "<=" ">" ">=" "==" "!="
                         "!" "=" "&&" "||")]

  ; --> SYMBOLS
  [symbol         (union "(" ")" "{" "}" ";" ",")]

  ; --> COMMENTS
  [comment        (concatenation "/*" (complement (concatenation any-string "*/" any-string)) "*/")])

(define operators-ht
  (hash "*"  'Op_multiply "/"  'Op_divide    "%" 'Op_mod      "+"  'Op_add           "-"  'Op_subtract
        "<"  'Op_less     "<=" 'Op_lessequal ">" 'Op_greater  ">=" 'Op_greaterequal "==" 'Op_equal
        "!=" 'Op_notequal "!"  'Op_not       "=" 'Op_assign   "&&" 'Op_and          "||" 'Op_or))

(define symbols-ht
  (hash "(" 'LeftParen  ")" 'RightParen
        "{" 'LeftBrace  "}" 'RightBrace
        ";" 'Semicolon  "," 'Comma))

(define (lexeme->datatype l) (string->symbol (~a "Datatype_" l))) ; to concatenate "Datatype_lexeme"
(define (lexeme->bool l) (string->symbol (~a "Bool")))            ; 
(define (lexeme->keyword  l) (string->symbol (~a "Keyword_" l)))  ; to concatenate "Keyword_lexeme"
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
     [operator       (token (lexeme->operator lexeme) lexeme)]
     [symbol         (token (lexeme->symbol   lexeme) lexeme)]
     [comment        (token 'Comment lexeme)]
     [whitespace     #f]
     [identifier     (token 'Identifier lexeme)]
     [(eof)          (token 'End_of_input)]))
  (define (next-token) (my-lexer ip))
  next-token)

(define (string->tokens s)
  (port->tokens (open-input-string s)))

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

(define test1 #<<TEST
/*
  Hello world
 */
print("Hello, World!\n");

TEST
  )

(define test2 #<<TEST
/*
  Show Ident and Integers
 */
phoenix_number = 142857;
print(phoenix_number, "\n");

TEST
  )

(define test3 #<<TEST
/*
  All lexical tokens - not syntactically correct, but that will
  have to wait until syntax analysis
 */
/* Print   */  print    /* Sub     */  -
/* Putc    */  putc     /* Lss     */  <
/* If      */  if       /* Gtr     */  >
/* Else    */  else     /* Leq     */  <=
/* While   */  while    /* Geq     */  >=
/* Lbrace  */  {        /* Eq      */  ==
/* Rbrace  */  }        /* Neq     */  !=
/* Lparen  */  (        /* And     */  &&
/* Rparen  */  )        /* Or      */  ||
/* Uminus  */  -        /* Semi    */  ;
/* Not     */  !        /* Comma   */  ,
/* Mul     */  *        /* Assign  */  =
/* Div     */  /        /* Integer */  42
/* Mod     */  %        /* String  */  "String literal"
/* Add     */  +        /* Ident   */  variable_name
/* character literal */  '\n'
/* character literal */  '\\'
/* character literal */  ' '
TEST
  )

(define test4 #<<TEST
/*** test printing, embedded \n and comments with lots of '*' ***/
print(42);
print("\nHello World\nGood Bye\nok\n");
print("Print a slash n - \\n.\n");
TEST
  )

(define test5 #<<TEST
count = 1;
while (count < 10) {
    print("count is: ", count, "\n");
    count = count + 1;
}
TEST
  )

(define test6 #<<TEST
/* This is a comment */
int test1 = 5;
float test2 = 2.2;

bool test3 = true;
bool test4 = false;
TEST
  )

(define (display-tokens ts)
  (for ([t ts])
    (for ([x t])
      (display x) (display "\t\t"))
    (newline)))

; "TEST 1"
; (display-tokens (string->tokens test1))
; "TEST 2"
; (display-tokens (string->tokens test2))
; "TEST 3"
; (display-tokens (string->tokens test3))
; "TEST 4"
; (display-tokens (string->tokens test4))
; "TEST 5"
; (display-tokens (string->tokens test5))
"TEST 6"
(display-tokens (string->tokens test6))