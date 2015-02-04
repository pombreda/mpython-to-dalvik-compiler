# 1 "lexico.mll"
 
    open Sintatico   (* o tipo token eh definido em sintatico.mli *)
    open Lexing
    open Printf


	(* contador de nivel de parentizacao - utilizado para identacao *)
    let nivel_par = ref 0

    (* incrementa a contagem de linhas *)
    let incr_nlinha lexbuf =
        let pos = lexbuf.lex_curr_p in
                lexbuf.lex_curr_p <- { pos with
                                       pos_lnum = pos.pos_lnum + 1;
                                       pos_bol = pos.pos_cnum;
                                     }
    (* imprime mensagem de erro *)
    let msg_erro lexbuf c =
        let pos = lexbuf.lex_curr_p in
            let lin = pos.pos_lnum
            and col = pos.pos_cnum - pos.pos_bol - 1 in
            sprintf "%d-%d: Caracter Desconhecido %c" lin col c

    (* cria tabela hasg  *)
    let cria_tab_hash iniciais =
        let tbl = Hashtbl.create (List.length iniciais) in
            List.iter (fun (chave, valor) -> Hashtbl.add tbl chave valor) iniciais;
            tbl

    (* palavras reservadas *)
    let plv_res =
        cria_tab_hash
        [
        ("def",   DEF);
        ("else",  ELSE );
        ("for",   FOR);
        ("if",    IF);
        ("in",    IN);
        ("not",   NOT);
        ("and",   AND);
        ("or",    OR);
        ("is",    IS);
        ("yield", YIELD);
        ("from",  FROM);
        ("return",RETURN);
        ("while", WHILE);
        ("range", RANGE);
        ("print", PRINT);
        ("raw_input", INPUT);
        ("int",   INT_PARSE)
        ]

(* Valores booleanos sao armazenados como 1 para true e 0 para false. *)
(* Operacoes com booleanos sao transformadas em operacoes com inteiros *)
    let booleano nbool =
    match nbool with
    | "True" -> 1
    | "False" -> 0
    | _ -> failwith "Erro: nao eh valor booleano"

# 63 "lexico.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\249\255\001\000\251\255\002\000\004\000\006\000\254\255\
    \007\000\010\000\008\000\014\000\215\255\216\255\217\255\218\255\
    \219\255\223\255\224\255\225\255\226\255\227\255\228\255\016\000\
    \017\000\045\000\047\000\241\255\048\000\049\000\077\000\079\000\
    \247\255\248\255\093\000\223\000\042\001\168\000\185\000\011\000\
    \255\255\195\000\117\001\222\255\233\255\127\001\137\001\212\001\
    \031\002\106\002\181\002\000\003\236\255\234\255\232\255\231\255\
    \238\255\237\255\235\255\230\255\057\003\249\255\250\255\058\003\
    \255\255\251\255\252\255\253\255\254\255\054\003\249\255\250\255\
    \055\003\255\255\251\255\252\255\253\255\254\255";
  Lexing.lex_backtrk = 
   "\255\255\255\255\255\255\255\255\000\000\003\000\002\000\255\255\
    \000\000\255\255\005\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\026\000\
    \039\000\016\000\015\000\255\255\013\000\012\000\010\000\009\000\
    \255\255\255\255\006\000\006\000\006\000\011\000\001\000\000\000\
    \255\255\255\255\003\000\255\255\255\255\002\000\006\000\006\000\
    \004\000\006\000\006\000\006\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\005\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \005\000\255\255\255\255\255\255\255\255\255\255";
  Lexing.lex_default = 
   "\002\000\000\000\010\000\000\000\004\000\255\255\255\255\000\000\
    \008\000\255\255\010\000\013\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\255\255\
    \255\255\255\255\255\255\000\000\255\255\255\255\255\255\255\255\
    \000\000\000\000\255\255\255\255\255\255\255\255\255\255\039\000\
    \000\000\255\255\255\255\000\000\000\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\062\000\000\000\000\000\255\255\
    \000\000\000\000\000\000\000\000\000\000\071\000\000\000\000\000\
    \255\255\000\000\000\000\000\000\000\000\000\000";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\005\000\003\000\255\255\255\255\009\000\007\000\009\000\
    \007\000\255\255\255\255\009\000\007\000\255\255\000\000\040\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \006\000\000\000\000\000\004\000\009\000\000\000\009\000\008\000\
    \000\000\008\000\009\000\000\000\000\000\008\000\040\000\024\000\
    \033\000\039\000\000\000\023\000\000\000\032\000\022\000\019\000\
    \029\000\030\000\016\000\037\000\014\000\028\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \027\000\015\000\025\000\031\000\026\000\059\000\058\000\034\000\
    \034\000\034\000\034\000\034\000\035\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\036\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\021\000\057\000\018\000\056\000\055\000\054\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\020\000\053\000\017\000\052\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \000\000\000\000\000\000\000\000\034\000\000\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\000\000\000\000\000\000\044\000\043\000\041\000\
    \000\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\042\000\042\000\042\000\042\000\042\000\
    \042\000\042\000\042\000\042\000\042\000\000\000\000\000\000\000\
    \001\000\255\255\255\255\000\000\000\000\000\000\000\000\255\255\
    \255\255\000\000\000\000\255\255\000\000\000\000\012\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\000\000\000\000\000\000\000\000\034\000\000\000\
    \049\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\000\000\000\000\000\000\
    \000\000\034\000\000\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\046\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\042\000\042\000\042\000\
    \042\000\042\000\042\000\042\000\042\000\042\000\042\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\000\000\000\000\000\000\000\000\
    \034\000\000\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\047\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\000\000\
    \000\000\000\000\000\000\034\000\000\000\034\000\034\000\034\000\
    \034\000\048\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\000\000\000\000\000\000\000\000\034\000\000\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\000\000\000\000\000\000\
    \000\000\034\000\000\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\050\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \000\000\000\000\000\000\000\000\034\000\000\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \051\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\064\000\066\000\073\000\075\000\034\000\
    \000\000\034\000\034\000\034\000\034\000\048\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\072\000\074\000\000\000\063\000\065\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\076\000\000\000\000\000\
    \067\000\000\000\000\000\077\000\000\000\000\000\068\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\070\000\000\000\
    \000\000\061\000\000\000";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\002\000\004\000\005\000\005\000\006\000\
    \006\000\008\000\010\000\009\000\009\000\039\000\255\255\011\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\000\000\005\000\255\255\006\000\005\000\
    \255\255\006\000\009\000\255\255\255\255\009\000\011\000\011\000\
    \011\000\011\000\255\255\011\000\255\255\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\023\000\024\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\025\000\011\000\026\000\028\000\029\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\030\000\011\000\031\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \255\255\255\255\255\255\255\255\034\000\255\255\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\255\255\255\255\255\255\037\000\037\000\038\000\
    \255\255\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\041\000\041\000\041\000\041\000\041\000\
    \041\000\041\000\041\000\041\000\041\000\255\255\255\255\255\255\
    \000\000\002\000\004\000\255\255\255\255\255\255\255\255\008\000\
    \010\000\255\255\255\255\039\000\255\255\255\255\011\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\255\255\255\255\255\255\255\255\035\000\255\255\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\255\255\255\255\255\255\
    \255\255\036\000\255\255\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\042\000\042\000\042\000\
    \042\000\042\000\042\000\042\000\042\000\042\000\042\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\046\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\046\000\046\000\255\255\255\255\255\255\255\255\
    \046\000\255\255\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\046\000\046\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\255\255\
    \255\255\255\255\255\255\047\000\255\255\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\255\255\255\255\255\255\255\255\048\000\255\255\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\049\000\049\000\049\000\049\000\049\000\049\000\
    \049\000\049\000\049\000\049\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\049\000\049\000\049\000\049\000\049\000\
    \049\000\049\000\049\000\049\000\049\000\049\000\049\000\049\000\
    \049\000\049\000\049\000\049\000\049\000\049\000\049\000\049\000\
    \049\000\049\000\049\000\049\000\049\000\255\255\255\255\255\255\
    \255\255\049\000\255\255\049\000\049\000\049\000\049\000\049\000\
    \049\000\049\000\049\000\049\000\049\000\049\000\049\000\049\000\
    \049\000\049\000\049\000\049\000\049\000\049\000\049\000\049\000\
    \049\000\049\000\049\000\049\000\049\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \255\255\255\255\255\255\255\255\050\000\255\255\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\060\000\063\000\069\000\072\000\051\000\
    \255\255\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\069\000\072\000\255\255\060\000\063\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\072\000\255\255\255\255\
    \063\000\255\255\255\255\072\000\255\255\255\255\063\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\069\000\255\255\
    \255\255\060\000\255\255";
  Lexing.lex_base_code = 
   "";
  Lexing.lex_backtrk_code = 
   "";
  Lexing.lex_default_code = 
   "";
  Lexing.lex_trans_code = 
   "";
  Lexing.lex_check_code = 
   "";
  Lexing.lex_code = 
   "";
}

let rec preprocessador indentacao lexbuf =
    __ocaml_lex_preprocessador_rec indentacao lexbuf 0
and __ocaml_lex_preprocessador_rec indentacao lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 76 "lexico.mll"
                      ( preprocessador 0 lexbuf )
# 393 "lexico.ml"

  | 1 ->
# 77 "lexico.mll"
                      ( incr_nlinha lexbuf;
                        preprocessador 0 lexbuf )
# 399 "lexico.ml"

  | 2 ->
# 79 "lexico.mll"
                      ( preprocessador (indentacao + 1) lexbuf )
# 404 "lexico.ml"

  | 3 ->
# 80 "lexico.mll"
                      ( let nova_ind = indentacao + 8 - (indentacao mod 8) in
                        preprocessador nova_ind lexbuf )
# 410 "lexico.ml"

  | 4 ->
# 82 "lexico.mll"
                      ( incr_nlinha lexbuf;
                        preprocessador 0 lexbuf )
# 416 "lexico.ml"

  | 5 ->
let
# 84 "lexico.mll"
              linha
# 422 "lexico.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 84 "lexico.mll"
                      (
		  let rec tokenize lexbuf =
		    let tok = token lexbuf in
		    match tok with
		      EOF -> []
		    | _ -> tok :: tokenize lexbuf in
		  let toks = tokenize (Lexing.from_string linha) in
		  LINHA(indentacao,!nivel_par, toks)
  )
# 434 "lexico.ml"

  | 6 ->
# 93 "lexico.mll"
                      ( nivel_par := 0; EOF  )
# 439 "lexico.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_preprocessador_rec indentacao lexbuf __ocaml_lex_state

and token lexbuf =
    __ocaml_lex_token_rec lexbuf 11
and __ocaml_lex_token_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 99 "lexico.mll"
                      ( token lexbuf )
# 450 "lexico.ml"

  | 1 ->
let
# 100 "lexico.mll"
             numint
# 456 "lexico.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 100 "lexico.mll"
                      ( let num = int_of_string numint in INT num )
# 460 "lexico.ml"

  | 2 ->
let
# 101 "lexico.mll"
         numNeg
# 466 "lexico.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 101 "lexico.mll"
                      ( let num = int_of_string numNeg in INT num )
# 470 "lexico.ml"

  | 3 ->
let
# 102 "lexico.mll"
                         numfloat
# 476 "lexico.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 102 "lexico.mll"
                                  (let num = float_of_string numfloat in FLOAT num)
# 480 "lexico.ml"

  | 4 ->
let
# 103 "lexico.mll"
             nbool
# 486 "lexico.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 103 "lexico.mll"
                      ( INT (booleano nbool))
# 490 "lexico.ml"

  | 5 ->
let
# 104 "lexico.mll"
            numfloat
# 496 "lexico.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 104 "lexico.mll"
                      ( FLOAT (float_of_string numfloat) )
# 500 "lexico.ml"

  | 6 ->
let
# 105 "lexico.mll"
        palavra
# 506 "lexico.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 105 "lexico.mll"
                      ( try Hashtbl.find plv_res palavra
                        with Not_found -> ID (palavra))
# 511 "lexico.ml"

  | 7 ->
# 107 "lexico.mll"
                      ( let buffer = Buffer.create 1 in
                        STRING (cadeia buffer lexbuf) )
# 517 "lexico.ml"

  | 8 ->
# 109 "lexico.mll"
                      ( let buffer = Buffer.create 1 in
                        STRING (cadeia2 buffer lexbuf) )
# 523 "lexico.ml"

  | 9 ->
# 111 "lexico.mll"
                 ( ATRIB )
# 528 "lexico.ml"

  | 10 ->
# 112 "lexico.mll"
                      ( MAIS )
# 533 "lexico.ml"

  | 11 ->
# 113 "lexico.mll"
                      ( MENOS )
# 538 "lexico.ml"

  | 12 ->
# 114 "lexico.mll"
                      ( VEZES )
# 543 "lexico.ml"

  | 13 ->
# 115 "lexico.mll"
                      ( DIVIDIDO )
# 548 "lexico.ml"

  | 14 ->
# 116 "lexico.mll"
                  ( DPONTOS )
# 553 "lexico.ml"

  | 15 ->
# 117 "lexico.mll"
                      ( MAIOR )
# 558 "lexico.ml"

  | 16 ->
# 118 "lexico.mll"
                      ( MENOR )
# 563 "lexico.ml"

  | 17 ->
# 119 "lexico.mll"
                      ( MAIORIGUAL )
# 568 "lexico.ml"

  | 18 ->
# 120 "lexico.mll"
                      ( MENORIGUAL )
# 573 "lexico.ml"

  | 19 ->
# 121 "lexico.mll"
                  ( IGUAL )
# 578 "lexico.ml"

  | 20 ->
# 122 "lexico.mll"
                  ( DIFERENTE )
# 583 "lexico.ml"

  | 21 ->
# 123 "lexico.mll"
                   ( ATRIBMAIS)
# 588 "lexico.ml"

  | 22 ->
# 124 "lexico.mll"
                   ( ATRIBMENOS)
# 593 "lexico.ml"

  | 23 ->
# 125 "lexico.mll"
                   ( ATRIBVEZES)
# 598 "lexico.ml"

  | 24 ->
# 126 "lexico.mll"
                   ( ATRIBDIV)
# 603 "lexico.ml"

  | 25 ->
# 127 "lexico.mll"
                   ( ATRIBMOD)
# 608 "lexico.ml"

  | 26 ->
# 128 "lexico.mll"
                 ( MODULO )
# 613 "lexico.ml"

  | 27 ->
# 129 "lexico.mll"
                      ( incr(nivel_par); APAR )
# 618 "lexico.ml"

  | 28 ->
# 130 "lexico.mll"
                      ( incr(nivel_par); ACOL )
# 623 "lexico.ml"

  | 29 ->
# 131 "lexico.mll"
                      ( incr(nivel_par); ACHA )
# 628 "lexico.ml"

  | 30 ->
# 132 "lexico.mll"
                      ( decr(nivel_par); FPAR )
# 633 "lexico.ml"

  | 31 ->
# 133 "lexico.mll"
                      ( decr(nivel_par); FCOL )
# 638 "lexico.ml"

  | 32 ->
# 134 "lexico.mll"
                      ( decr(nivel_par); FCHA )
# 643 "lexico.ml"

  | 33 ->
# 135 "lexico.mll"
                      ( SETA )
# 648 "lexico.ml"

  | 34 ->
# 136 "lexico.mll"
                      ( ATRIB )
# 653 "lexico.ml"

  | 35 ->
# 137 "lexico.mll"
                      ( DPONTOS )
# 658 "lexico.ml"

  | 36 ->
# 138 "lexico.mll"
                 ( VIRG )
# 663 "lexico.ml"

  | 37 ->
# 139 "lexico.mll"
                 ( PTVIRG )
# 668 "lexico.ml"

  | 38 ->
# 140 "lexico.mll"
                 ( PTO )
# 673 "lexico.ml"

  | 39 ->
let
# 141 "lexico.mll"
       c
# 679 "lexico.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 141 "lexico.mll"
                      ( failwith (msg_erro lexbuf c); )
# 683 "lexico.ml"

  | 40 ->
# 142 "lexico.mll"
                      ( EOF )
# 688 "lexico.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_token_rec lexbuf __ocaml_lex_state

and cadeia buffer lexbuf =
    __ocaml_lex_cadeia_rec buffer lexbuf 60
and __ocaml_lex_cadeia_rec buffer lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 146 "lexico.mll"
              ( Buffer.contents buffer )
# 699 "lexico.ml"

  | 1 ->
# 147 "lexico.mll"
              ( Buffer.add_char buffer '\t'; cadeia buffer lexbuf )
# 704 "lexico.ml"

  | 2 ->
# 148 "lexico.mll"
              ( Buffer.add_char buffer '\n'; cadeia buffer lexbuf )
# 709 "lexico.ml"

  | 3 ->
# 149 "lexico.mll"
              ( Buffer.add_char buffer '"';  cadeia buffer lexbuf )
# 714 "lexico.ml"

  | 4 ->
# 150 "lexico.mll"
              ( Buffer.add_char buffer '\\'; cadeia buffer lexbuf )
# 719 "lexico.ml"

  | 5 ->
let
# 151 "lexico.mll"
        c
# 725 "lexico.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 151 "lexico.mll"
              ( Buffer.add_char buffer c;    cadeia buffer lexbuf )
# 729 "lexico.ml"

  | 6 ->
# 152 "lexico.mll"
              ( failwith "string não foi fechada" )
# 734 "lexico.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_cadeia_rec buffer lexbuf __ocaml_lex_state

and cadeia2 buffer lexbuf =
    __ocaml_lex_cadeia2_rec buffer lexbuf 69
and __ocaml_lex_cadeia2_rec buffer lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 155 "lexico.mll"
              ( Buffer.contents buffer )
# 745 "lexico.ml"

  | 1 ->
# 156 "lexico.mll"
              ( Buffer.add_char buffer '\t'; cadeia2 buffer lexbuf )
# 750 "lexico.ml"

  | 2 ->
# 157 "lexico.mll"
              ( Buffer.add_char buffer '\n'; cadeia2 buffer lexbuf )
# 755 "lexico.ml"

  | 3 ->
# 158 "lexico.mll"
              ( Buffer.add_char buffer '"';  cadeia2 buffer lexbuf )
# 760 "lexico.ml"

  | 4 ->
# 159 "lexico.mll"
              ( Buffer.add_char buffer '\\'; cadeia2 buffer lexbuf )
# 765 "lexico.ml"

  | 5 ->
let
# 160 "lexico.mll"
        c
# 771 "lexico.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 160 "lexico.mll"
              ( Buffer.add_char buffer c;    cadeia2 buffer lexbuf )
# 775 "lexico.ml"

  | 6 ->
# 161 "lexico.mll"
              ( failwith "string não foi fechada" )
# 780 "lexico.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_cadeia2_rec buffer lexbuf __ocaml_lex_state

;;

