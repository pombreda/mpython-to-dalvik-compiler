type token =
  | INT of (int)
  | FLOAT of (float)
  | BOOL of (bool)
  | ID of (string)
  | STRING of (string)
  | LINHA of (int * int * token list)
  | INDENTA
  | DEDENTA
  | NOVALINHA
  | TIPOINT
  | TIPOFLOAT
  | TIPOSTRING
  | TIPOVOID
  | TIPOBOOL
  | PTO
  | PTVIRG
  | DEF
  | IS
  | YIELD
  | FROM
  | RETURN
  | TRUE
  | FALSE
  | APAR
  | FPAR
  | ACOL
  | FCOL
  | ACHA
  | FCHA
  | SETA
  | IF
  | ELSE
  | WHILE
  | DPONTOS
  | FOR
  | IN
  | RANGE
  | VIRG
  | NOT
  | AND
  | OR
  | ATRIB
  | MAIS
  | MENOS
  | VEZES
  | DIVIDIDO
  | MODULO
  | POT
  | MAIOR
  | MENOR
  | IGUAL
  | DIFERENTE
  | MAIORIGUAL
  | MENORIGUAL
  | ATRIBMAIS
  | ATRIBMENOS
  | ATRIBVEZES
  | ATRIBDIV
  | ATRIBMOD
  | EOF
  | PRINT
  | INPUT
  | INT_PARSE

val programa :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Asa.programa
