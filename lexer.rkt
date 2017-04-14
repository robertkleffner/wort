#lang br
(require brag/support)

(define wort-lexer
  (lexer-srcloc
   [(eof) (return-without-srcloc eof)]
   [(from/to "--" "\n")
    (token 'COMMENT lexeme #:skip? #t)]
   [whitespace
    (token 'WHITESPACE lexeme #:skip? #t)]
   [(:or "add" "eq" "less" "call" "fix" "if"
         "{" "}" "=" "(" ")"
         "bind" "let"
         "true" "false")
    (token lexeme lexeme)]
   [(:seq alphabetic (:* (:or alphabetic numeric)))
    (token 'VAR lexeme)]
   [(:+ (char-set "0123456789"))
    (token 'INT (string->number lexeme))]))

(provide wort-lexer)