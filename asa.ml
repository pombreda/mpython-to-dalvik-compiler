
(* Estrutura que define uma posicao*)
module Posicao =
    struct
        type t = { lin_inicial: int ;
                   col_inicial: int ;
                   lin_final: int ;
                   col_final: int
				}
let pos n =
    let pos_inicial = Parsing.rhs_start_pos n in
    let pos_final = Parsing.rhs_end_pos n in
    let linha_inicial = pos_inicial.Lexing.pos_lnum

and coluna_inicial = pos_inicial.Lexing.pos_cnum - pos_inicial.Lexing.pos_bol + 1
and linha_final = pos_final.Lexing.pos_lnum
and coluna_final = pos_final.Lexing.pos_cnum - pos_final.Lexing.pos_bol in
    {  lin_inicial = linha_inicial;
       col_inicial = coluna_inicial;
       lin_final = linha_final;
       col_final = coluna_final
    }
let npos n =
    {  lin_inicial = Parsing.rhs_start(n);
        col_inicial = Parsing.rhs_end(n);
        lin_final = 0;
        col_final = 0
    }
end

(* Tipos base *)
type tipo_base =  TInt
                           |TFloat
                           |TString
                           |TVoid
                           |TGen
				    |TBool

(* Definicao de um comando e uma lista de comandos*)
and comandos = comando list
and comando = { vcmd: cmd;
                          pcmd: Posicao.t
			  }

(* Definicao de um parametro e uma lista de parametros*)
(* and parametro =  { entradaP: string * entradaVariavel} *)

and parametro = string * entradaVariavel

and parametros = parametro list

(* Definicao de um argumento e uma lista de argumentos*)
and argumento = expressao
and argumentos = argumento list

(* Definicao de uma funcao *)
and funcao = {    idF: string;
                          paramsF: parametros;
                          cmdsF: comandos;
                          mutable returnF: tipo_base option;
                          posF: Posicao.t;
                          mutable varLocaisF:(string, entradaVariavel) Hashtbl.t;
                      }

(* Definicao de um programa *)
and programa = {  funcsP: funcao list;
                            cmdsP: comandos
                }

(* Tipos de comandos *)
and cmd = CmdAtrib of expressao * expressao
        | CmdIf of expressao * comandos * comandos option
        | CmdWhile of expressao * comandos
        | CmdRange of int * int * int
        | CmdFor of expressao * comando * comandos
        | CmdFuncao of string * parametros * comandos
        | ChamaFuncaoVoid of string * argumentos
        | ChamaFuncaoAtrib of expressao * string * argumentos
        | CmdReturn of expressao
        | CmdPrint of expressao
        | CmdInput of expressao * expressao
        | CmdIntParse of expressao * expressao

(* Definicao de uma expressao *)
and expressao = {  mutable valor: expr option ;
                                mutable tipo: tipo_base option;
                                mutable pos: Posicao.t;
                }

(* Tipos de valor de uma expressao *)
and expr =   ExpInt of int
                   | ExpFloat of float
                   | ExpString of string
                   | ExpVar of variavel
                   | ExpBin of operadorBin * expressao * expressao
                   | ExpGen
      		  | ExpBool of bool
      		  | ExpUn of operadorUn * expressao

(* Definicao de uma variavel *)
and variavel = VarSimples of string

and operadorUn = Not

(* Tipos de operadores binarios *)
and operadorBin = Mais | Menos | Mult | Div | Maior | Menor | Igual | Diferente | MaiorIgual | MenorIgual | Modulo | And | Or

(*TABELA DE SIMBOLOS*)
(* Definicao da tabela de simbolos geral *)
and ambiente = (string, entradaTabela) Hashtbl.t

(* Entrada de uma funcao na tabela de simbolos *)
and entradaFuncao = {  varLocais: (string, entradaVariavel) Hashtbl.t;
                                    mutable tiporetorno: tipo_base option;
                                    param: parametros
					}

(* Entrada de uma variavel na tabela de simbolos *)
and entradaVariavel = {  mutable tipagem: tipo_base option;
                                      v_inicial: expressao option;
                                      mutable endereco: int option;
                                      mutable valor_variavel: expr option
					  }

(* Tipos de entrada que a tabela de simbolos aceita *)
and entradaTabela = EntVar of entradaVariavel
                                 | EntFn of entradaFuncao

(* Cria uma entrada do tipo variavel para a tabela de simbolos de acordo com o tipo recebido *)
let cria_ent_var tipo = {  tipagem = tipo ;
                                      v_inicial = None;
                                      endereco = None;
                                      valor_variavel = None }

(* Cria uma entrada do tipo funcao para a tabela de simbolos de acordo com o tipo e parametros recebidos *)
let cria_ent_func tipo par locais= { varLocais = locais;
                                                    tiporetorno = Some tipo;
                                                    param = par }

(* let cria_ent_param nome tipo = {  idP = nome;
                                                   tipoP =  tipo;
                                                   posP = {  lin_inicial = 0;
                                                         col_inicial = 0;
                                                         lin_final = 0;
                                                         col_final = 0
                                                      };
                                                   valor_param = None
                                                } *)

(* Procura um parametro numa lista de parametros *)
let rec procuraParam nome params =
    match params with
        [] -> None
    | param :: params ->
        if ((fst param) <> nome) then
           procuraParam nome params
        else
           Some (snd param)


let busca_var_fun amb regfn nome =
  match regfn with
  EntFn regfn ->
    (try (* tenta encontrar variavel local *)
         Hashtbl.find regfn.varLocais nome
     with Not_found -> (* tenta encontrar parametro *)
           (match (procuraParam nome regfn.param) with
                Some v -> v
              | None -> (* tenta encontrar variavel global *)
                (match (Hashtbl.find amb nome) with
                  EntVar v -> v
                  | _ -> failwith "busca_var_fun: erro"
                )
            )
    )
    | _ -> failwith "busca_var_fun: erro"

(* let busca_tipo_fun amb regfn nome =
  match regfn with
  EntFn regfn ->
    (try (* tenta encontrar variavel local *)
         Hashtbl.find regfn.varLocais nome
            with Not_found -> (* tenta encontrar parametro *)
                                 (match (procuraParam nome regfn.param) with
                                      Some v -> v;
                                      v.tipagem
                                    | None -> tenta encontrar variavel global
                                                  (match (Hashtbl.find amb nome) with
                                                      EntVar v -> v;
                                                      v.tipagem
                                                      | _ -> failwith "busca_var_fun: erro"
                                                  )
                                  )
    )
    | _ -> failwith "busca_var_fun: erro" *)

(* Teste *)
let tabela_simb =
    let entA = EntVar (cria_ent_var (Some TInt))
    and params = [("x", cria_ent_var (Some TInt));
                           ("y", cria_ent_var (Some TInt))]
    and locais = (let tabl = Hashtbl.create 5 in
                               Hashtbl.add tabl "b" (cria_ent_var (Some TInt));
                               tabl
                        )
    in
    let entF = EntFn (cria_ent_func TInt params locais)
    and tab = Hashtbl.create 5 in
     ( Hashtbl.add tab "a" entA;
      Hashtbl.add tab "f" entF;
      tab )