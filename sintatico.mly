%{
    open Asa;;
(* Cria uma expressao onde v eh o valor da expressÃ£o e o eh a ordem na
regra gramatical *)

    let cria_exp o v =
        { valor = v;
           tipo = (Some TGen);
           pos = Posicao.pos(o) }

(* Cria um comando onde c eh o comando e o eh a ordem na regra gramatical *)
let cria_cmd o c =
    { vcmd = c;
       pcmd = Posicao.pos(o) }
(* Cria um programa onde f sao as funcoes e c os comandos fora das funcoes
*)
let cria_programa f c =
    { funcsP = f;
       cmdsP = c }
(* Cria uma funcao onde o eh a ordem na regra gramatical, i eh o nome da funcao,
p sao os parametros e c sao os comandos dentro da funcao. O tipo de retorno inicia como None *)
let cria_funcao o t i p c =
    { idF = i;
       paramsF = p;
       cmdsF = c;
       returnF = t;
       posF = Posicao.pos(o);
       varLocaisF = Hashtbl.create 20 }
(* Cria um parametro onde o eh a ordem na regra gramatical e i o nome (id) do parametro.
O tipo do parametro inicia como TGen *)
let cria_parametro o c i= (i, cria_ent_var c)


%}


/* Definicao de tokens */

%token <int> INT
%token <float>  FLOAT
%token <bool> BOOL
%token <string> ID
%token <string> STRING
%token <int * int * token list> LINHA
%token INDENTA DEDENTA NOVALINHA
%token TIPOINT TIPOFLOAT TIPOSTRING TIPOVOID TIPOBOOL
%token PTO PTVIRG
%token DEF IS YIELD FROM RETURN
%token TRUE FALSE
%token APAR FPAR ACOL FCOL ACHA FCHA SETA
%token IF ELSE WHILE DPONTOS
%token FOR IN RANGE VIRG
%token NOT AND OR
%token ATRIB
%token MAIS MENOS VEZES DIVIDIDO MODULO POT
%token MAIOR MENOR IGUAL DIFERENTE MAIORIGUAL MENORIGUAL
%token ATRIBMAIS ATRIBMENOS ATRIBVEZES ATRIBDIV ATRIBMOD
%token EOF
%token PRINT INPUT INT_PARSE
/* o símbolo inicial da gramática (ponto de entrada) */
%start programa
%type <Asa.programa> programa
%%

/* um programa eh definido por um conjunto de funcoes seguido por um
conjunto de comandos */
programa: funcoes comandos { cria_programa $1 $2 };

/* */
funcoes:  { [] }
                | funcoes funcao { $1 @ [ $2 ] }
                ;
/* define a estrutura de uma funcao e cria a funcao */
funcao: DEF tipo ID APAR parametros FPAR DPONTOS NOVALINHA
INDENTA comandos DEDENTA { cria_funcao 1 $2 $3 $5 $10 };

/* define a estrutura de uma lista de funcoes e um parametro e cria o
parametro */
tipo:   INT_PARSE {Some TInt}
        ;

parametros: { [] }
                     | parametros parametro { $1 @ [ $2 ] }
                     ;

/* um parametro pode estar seguido de virgula ou nao */
parametro: tipo ID VIRG { cria_parametro 1 $1 $2 }
                |  tipo ID { cria_parametro 1 $1 $2 };

argumentos: { [] }
                    | argumentos argumento { $1 @ [ $2 ] }
                    ;
/* um argumento pode estar seguido de virgula ou nao */
argumento: expressao VIRG { cria_exp 2 $1.valor}
                    | expressao { cria_exp 2 $1.valor}
                    ;
/* a chamada de funcao pode ser ou nao uma atribuicao */
cmd_chamada_func: ID APAR argumentos FPAR NOVALINHA { cria_cmd 1 (ChamaFuncaoVoid ($1, $3))};
                                | expressao ATRIB ID APAR argumentos FPAR NOVALINHA {cria_cmd 1 (ChamaFuncaoAtrib ($1, $3, $5))}

/* tipos de comandos */
comando:  cmd_atrib { $1 }
                | cmd_if_else { $1 }
                | cmd_if { $1 }
                | cmd_while { $1 }
                | cmd_for { $1 }
                | cmd_range1 { $1 }
                | cmd_range2 { $1 }
                | cmd_range3 { $1 }
                | cmd_atribMAIS { $1 }
                | cmd_atribMENOS { $1 }
                | cmd_atribVEZES { $1 }
                | cmd_atribDIV { $1 }
                | cmd_atribMOD { $1 }
                | cmd_chamada_func { $1 }
                | cmd_retorno { $1 }
                | cmd_print { $1 }
                | cmd_input { $1 }
                | cmd_int_parse { $1 }
            ;
/* definicao da estrutura dos comandos */
comandos: { [] }
                | comandos comando { $1 @ [ $2 ] }
                ;
cmd_int_parse: expressao ATRIB INT_PARSE APAR expressao FPAR NOVALINHA
    { cria_cmd 1 ( CmdIntParse ( $1, $5 ))}
cmd_print: PRINT APAR expressao FPAR NOVALINHA
    { cria_cmd 1 ( CmdPrint( $3 ) )}
cmd_input: expressao ATRIB INPUT APAR expressao FPAR NOVALINHA
    { cria_cmd 1 ( CmdInput ( $1, $5 ) )}
cmd_retorno: RETURN expressao NOVALINHA
    { cria_cmd 1 ( CmdReturn ( $2 ) ) }
cmd_atrib: expressao ATRIB expressao NOVALINHA
    { cria_cmd 2 ( CmdAtrib ( $1, $3 ) ) }
cmd_if: IF expressao DPONTOS NOVALINHA INDENTA comandos DEDENTA
    { cria_cmd 1 ( CmdIf ( $2, $6, None ) ) }
cmd_if_else: IF expressao DPONTOS NOVALINHA INDENTA comandos DEDENTA
                    ELSE DPONTOS NOVALINHA INDENTA comandos DEDENTA
    { cria_cmd 1 ( CmdIf ( $2, $6, Some( $12 ) ) ) }
cmd_while: WHILE expressao DPONTOS NOVALINHA INDENTA comandos DEDENTA
    { cria_cmd 1 ( CmdWhile ( $2, $6 ) ) }
cmd_for: FOR expressao IN comando DPONTOS NOVALINHA INDENTA comandos DEDENTA
    { cria_cmd 1 ( CmdFor ( $2, $4, $8 ) ) }
/* se o comando range tiver um parametro a range varia de 0 ao parametro
com incremento 1*/
cmd_range1: RANGE APAR INT FPAR
    { cria_cmd 1 ( CmdRange ( 0, $3, 1 ) ) }
/* se o comando range tiver dois parametros a range varia entre os dois
parametros com incremento 1*/
cmd_range2: RANGE APAR INT VIRG INT FPAR
    { cria_cmd 1 ( CmdRange ( $3, $5, 1) ) }
/* se o comando range tiver tres parametros os tres sao passados para o comando */
cmd_range3: RANGE APAR INT VIRG INT VIRG INT FPAR
    { cria_cmd 1 ( CmdRange ( $3, $5, $7 ) ) }
/* para comandos +=, -= cria uma expressao e coloca na atribuicao */
cmd_atribMAIS: expressao ATRIBMAIS expressao
    { let exp = cria_exp 2 (Some( ExpBin(Mais, $1, $3 ) )) in
        cria_cmd 2 ( CmdAtrib( $1, exp ) ) }
cmd_atribMENOS: expressao ATRIBMENOS expressao
    { let exp = cria_exp 2 (Some (ExpBin ( Menos, $1, $3 ) ) )in
    cria_cmd 2 ( CmdAtrib ( $1, exp ) ) }
cmd_atribVEZES: expressao ATRIBVEZES expressao
    { let exp = cria_exp 2 (Some( ExpBin ( Mult, $1, $3 ) )) in
    cria_cmd 2 ( CmdAtrib ( $1, exp ) ) }
cmd_atribDIV: expressao ATRIBDIV expressao
    { let exp = cria_exp 2 (Some( ExpBin ( Div, $1, $3 ) )) in
    cria_cmd 2 ( CmdAtrib( $1, exp ) ) }
cmd_atribMOD: expressao ATRIBMOD expressao
    { let exp = cria_exp 2 (Some( ExpBin ( Modulo, $1, $3 ) )) in
    cria_cmd 2 ( CmdAtrib( $1, exp ) ) }

/* criando expressoes */
expressao : expressao AND expr1 {cria_exp 5 (Some(ExpBin (And, $1, $3)))}
			|expressao OR expr1 {cria_exp 5 (Some(ExpBin (Or, $1, $3)))}
			| expr1 {$1}
			;

expr1 : expr1 MAIOR expr2 { cria_exp 4 (Some( ExpBin ( Maior, $1, $3 ) )) }
	 | expr1 MENOR expr2 { cria_exp 4 (Some( ExpBin ( Menor, $1, $3 ) )) }
	 | expr1 IGUAL expr2 { cria_exp 4 (Some( ExpBin ( Igual, $1, $3 ) )) }
	 | expr1 DIFERENTE expr2 { cria_exp 4 (Some( ExpBin ( Diferente, $1, $3 ) )) }
	 | expr1 MAIORIGUAL expr2 { cria_exp 4 (Some( ExpBin ( MaiorIgual, $1, $3 ) )) }
	 | expr1 MENORIGUAL expr2 { cria_exp 4 (Some( ExpBin ( MenorIgual, $1, $3 ) )) }
	 | expr1 MODULO expr2 { cria_exp 4 (Some( ExpBin ( Modulo, $1, $3 ) )) }
	 | expr2 { $1 }

expr2 : expr2 MAIS expr3 { cria_exp 3 (Some( ExpBin ( Mais, $1, $3 ) )) }
		  | expr2 MENOS expr3 { cria_exp 3 (Some( ExpBin ( Menos, $1, $3 ) )) }
		  | expr3 { $1 }
          ;

expr3: expr3 VEZES expr4 { cria_exp 2 (Some( ExpBin ( Mult, $1, $3 ) )) }
 	 | expr3 DIVIDIDO expr4 { cria_exp 2 (Some( ExpBin ( Div, $1, $3 ) )) }
	 | expr4 { $1 }
	 ;

expr4: NOT expr4 {cria_exp 1 (Some(ExpUn(Not, $2)))}
	 | expr5 {$1}
	 ;

expr5: operando  { cria_exp 0 $1 }
     | variavel  { cria_exp 0 (Some( ExpVar $1 ) )}
     | APAR expressao FPAR { $2 }
     ;
/* criando operando e variavel */
operando: INT {Some (ExpInt $1) }
		  | FLOAT { Some (ExpFloat $1) }
		  | STRING { Some (ExpString $1) }
		  |  BOOL { Some (ExpBool $1) }
;
variavel: ID { VarSimples $1 }
;