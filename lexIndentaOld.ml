open Sintatico
open Printf

let preprocessa lexbuf =
	let pilha = Stack.create ()
	and npar = ref 0 in
		let _ = Stack.push 0 pilha  in
		let off_side toks nivel =
		if !npar != 0 (* nova linha entre parênteses *)
		then toks    (* não faça nada *)
		else if nivel > Stack.top pilha
				 then begin
					Stack.push nivel pilha;
					INDENTA :: toks
				 end
			else if nivel = Stack.top pilha
				then toks
				else begin
						let prefixo = ref toks  in
						while nivel < Stack.top pilha do
							ignore (Stack.pop pilha);
							if nivel > Stack.top pilha
								then failwith "Erro de indentação"
							else prefixo := DEDENTA :: !prefixo
						done;
						!prefixo
					 end
  in
	let rec dedenta sufixo =
		if Stack.top pilha != 0
			then let _ = Stack.pop pilha in
			dedenta (DEDENTA :: sufixo)
		else sufixo
	in
	let rec get_tokens () =
		let tok = Lexico.preprocessador 0 lexbuf in
		match tok with
		LINHA(nivel,npars,toks) ->
			let new_toks = off_side toks nivel in
			npar := npars;
			new_toks @ (if npars = 0 then NOVALINHA :: get_tokens ()
						else get_tokens ())
		| _ -> dedenta []
		in get_tokens ()

(* Imprime na tela cada token gerado *)
let print_tok tok =
	match tok with
	| INT i   -> printf "INT %d\n" i
	| FLOAT f -> printf "FLOAT %f\n" f
	| ID s    -> printf "ID %s\n" s
	| STRING s -> printf "STRING %s\n" s
	| LINHA (i, p, toks) -> printf "LINHA (%d, %d, \n" i p
	| MAIS -> print_endline("MAIS")
	| MENOS -> print_endline("MENOS")
	| VEZES -> print_endline("VEZES")
	| DIVIDIDO -> print_endline("DIVIDIDO")
	| POT -> print_endline("POT")
	| MAIOR -> print_endline("MAIOR")
	| MENOR -> print_endline("MENOR")
	| IGUAL -> print_endline("IGUAL")
	| DIFERENTE -> print_endline("DIFERENTE")
	| MAIORIGUAL -> print_endline("MAIORIGUAL")
	| MENORIGUAL -> print_endline("MENORIGUAL")
	| APAR -> print_endline("APAR")
	| FPAR -> print_endline("FPAR")
	| ACOL -> print_endline("ACOL")
	| FCOL -> print_endline("FCOL")
	| ACHA -> print_endline("ACHA")
	| FCHA -> print_endline("FCHA")
	| DPONTOS -> print_endline("DPTOS")
	| PTO -> print_endline("PTO")
	| PTVIRG -> print_endline("PTVIRG")
	| IF -> print_endline("IF")
	| ELSE -> print_endline("ELSE")
	| WHILE -> print_endline("WHILE")
	| FOR -> print_endline("FOR")
	| IN -> print_endline("IN")
	| RANGE -> print_endline("RANGE")
	| VIRG -> print_endline("VIRG")
	| DEF -> print_endline("DEF")
	| RETURN -> print_endline("RETURN")
	| INDENTA -> print_endline("INDENTA")
	| DEDENTA -> print_endline("DEDENTA")
	| NOT -> print_endline("NOT")
	| TRUE -> print_endline("TRUE")
	| FALSE -> print_endline("FALSE")
	| ATRIB -> print_endline("ATRIB")
	| ATRIBMAIS -> print_endline("ATRIBMAIS")
	| ATRIBMENOS -> print_endline("ATRIBMENOS")
	| ATRIBVEZES -> print_endline("ATRIBVEZES")
	| ATRIBDIV -> print_endline("ATRIBDIV")
	| ATRIBMOD -> print_endline("ATRIBMOD")
	| EOF -> print_endline("EOF")
	| AND -> print_endline("AND")
	| OR -> print_endline("OR")
	| IS -> print_endline("IS")
	| FROM -> print_endline "FROM"
	| YIELD -> print_endline "YIELD"
	| NOVALINHA -> print_endline "NOVALINHA"
	| SETA -> print_endline "SETA"
	| MODULO -> print_endline "MODULO"
	| BOOL _ -> print_endline "BOOL"
	| PRINT -> print_endline "PRINT"
	| INPUT -> print_endline "INPUT"
	| INT_PARSE -> print_endline "INT_PARSE"

(* Chama o analisador lexico *)
let lexico =
	let tokbuf = ref None in
		let carrega lexbuf =
			let toks = preprocessa lexbuf in
			(match toks with
			tok::toks ->
				 tokbuf := Some toks;
				 print_tok tok;
				 tok
			| [] -> print_endline "EOF";
			        EOF)
  in
  fun lexbuf ->
	match !tokbuf with
	Some tokens ->
		(match tokens with
		tok::toks ->
			tokbuf := Some toks;
			print_tok tok;
			tok
		| [] -> carrega lexbuf)
	| None -> carrega lexbuf



