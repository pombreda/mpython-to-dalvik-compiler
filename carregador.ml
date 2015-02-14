#load "asa.cmo";;
#load "sintatico.cmo";;
#load "lexico.cmo";;
#load "lexIndenta.cmo";;
#load "semantico.cmo";;(*
#load "interpretador.cmo";; *)

open Asa;;
open Sintatico;;
open Semantico;;
(* open Interpretador;; *)
open Printf;;
open Filename;;

let sintatico lexbuf =
    try
        Sintatico.programa LexIndenta.lexico lexbuf
    with exn ->
    begin
        let tok = Lexing.lexeme lexbuf in
        let pos = lexbuf.Lexing.lex_curr_p in
        let nlin = pos.Lexing.pos_lnum in
        let ncol = pos.Lexing.pos_cnum - pos.Lexing.pos_bol - String.length
            tok in
        let msg1 = sprintf "Erro na linha %d, coluna %d" nlin ncol in
        let msg2 = sprintf "\tA palavra \"%s\" nao era esperada aqui." tok in
            print_endline msg1;
            print_endline msg2;
            flush stdout;
            raise exn
   end

let sint_str str =
  let lexbuf = Lexing.from_string str in
  sintatico lexbuf

let sint_arq arq =
   let ic = open_in arq in
   let lexbuf = Lexing.from_channel ic in
   let arv = sintatico lexbuf in
   close_in ic;
   arv

let sem_arq arq =
  let arv = sint_arq arq in
  semantico arv

(*  let arvtab_str arq =
    let ic = open_in arq in
    let lexbuf = Lexing.from_channel ic in
    let arv = sintatico lexbuf in
    close_in ic;
    let tabSimb = semantico arv in (arv, tabSimb) *)

(* let interp arq =
  let arv = sint_arq arq in
  let amb = semantico arv in
  interpretador amb arv *)

(*
let interp arq =
  let lexbuf = Lexing.from_string arq in
  let arv = sintatico lexbuf in
  let tabSimb = semantico arv in
    interpretador tabSimb arv *)


