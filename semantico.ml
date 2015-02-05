open Asa;;
open Printf;;

let current_func = ref ""
(* Funcoes para mostrar os erros na tela *)
let erro nome pos msg =
	let nlin = pos.Posicao.lin_inicial
	and ncol = pos.Posicao.col_inicial in
	let msglc = sprintf "Erro na linha %d, coluna %d" nlin ncol in
	print_endline msglc;
	print_endline (msg ^ nome);
	failwith "Erro semantico"
(* Tabela de simbolos para as operacoes *)
let tab2list tab = Hashtbl.fold (fun c v ls -> (c,v) :: ls) tab []
let ambfun =
	let amb = Hashtbl.create 23 in
		Hashtbl.add amb Mais [ ((Some TInt), (Some TInt), (Some TInt)) ; ((Some TFloat), (Some TFloat), (Some TFloat)); ((Some TInt),(Some TFloat), (Some TFloat)); ((Some TFloat),(Some TInt), (Some TFloat))] ;
		Hashtbl.add amb Menos [ ((Some TInt), (Some TInt), (Some TInt)); ((Some TFloat), (Some TFloat), (Some TFloat)); ((Some TFloat),(Some TFloat),(Some TInt)); ((Some TFloat),(Some TInt),(Some TFloat))] ;
		Hashtbl.add amb Mult [ ((Some TInt), (Some TInt), (Some TInt)); ((Some TFloat),(Some TFloat), (Some TFloat)); ( (Some TFloat), (Some TFloat),(Some TInt)); ((Some TFloat),(Some TInt),(Some TFloat))] ;
		Hashtbl.add amb Div [ ((Some TInt), (Some TInt), (Some TInt)); ((Some TFloat),(Some TFloat), (Some TFloat)); ( (Some TFloat),(Some TFloat),(Some TInt)); ((Some TFloat),(Some TInt),(Some TFloat))] ;
		Hashtbl.add amb Menor [ ((Some TBool), (Some TInt), (Some TInt)); ((Some TBool), (Some TFloat), (Some TFloat)); ((Some TBool),(Some TInt),(Some TFloat));((Some TBool),(Some TFloat),(Some TInt))] ;
		Hashtbl.add amb Maior [ ((Some TBool), (Some TInt), (Some TInt)); ((Some TBool), (Some TFloat), (Some TFloat)); ((Some TBool),(Some TInt),(Some TFloat));((Some TBool),(Some TFloat),(Some TInt))] ;
		Hashtbl.add amb Igual [ ((Some TBool), (Some TInt), (Some TInt)); ((Some TBool), (Some TFloat), (Some TFloat)); ((Some TBool),(Some TInt),(Some TFloat));((Some TBool),(Some TFloat),(Some TInt))] ;
		Hashtbl.add amb Diferente [ ((Some TBool), (Some TInt), (Some TInt));  ((Some TBool), (Some TFloat), (Some TFloat)); ((Some TBool),(Some TInt),(Some TFloat));((Some TBool),(Some TFloat),(Some TInt))] ;
		Hashtbl.add amb MaiorIgual [ ((Some TBool), (Some TInt), (Some TInt));((Some TBool), (Some TFloat), (Some TFloat)); ((Some TBool),(Some TInt),(Some TFloat));((Some TBool),(Some TFloat),(Some TInt))] ;
		Hashtbl.add amb MenorIgual [ ((Some TBool), (Some TInt), (Some TInt)); ((Some TBool), (Some TFloat), (Some TFloat)); ((Some TBool),(Some TInt),(Some TFloat));((Some TBool),(Some TFloat),(Some TInt))] ;
		Hashtbl.add amb Modulo [ ((Some TInt), (Some TInt), (Some TInt)); ((Some TFloat),(Some TFloat), (Some TFloat))] ;
		(*nao admite outros tipos de and e or que nao seja com bool*)
		Hashtbl.add amb And [ ((Some TBool), (Some TBool), (Some TBool))];
		Hashtbl.add amb Or [ ((Some TBool), (Some TBool), (Some TBool))];
	amb
let tipo e = e.tipo
(* Verifica se os tipos dos termos sao compativeis com o tipo do operador*)
let rec verifica_op t1 t2 ls =
	match ls with
		(a1,a2,r)::ls -> if ((t1 == a1) && (t2 == a2)) then r
		else verifica_op t1 t2 ls
		| [] -> failwith "verifica_op: O tipo dos operandos deveria ser o mesmo"

(* Insere variavel em uma tabela e retorna o tipo, caso jÃ¡ exista apenas retorna o tipo *)
let insere_var amb nome tipo current =
	if(current<>"") then
	(
		try
			let tabVar = Hashtbl.find amb current in
				 match tabVar with
				  EntFn tabFn ->
				    (try (* tenta encontrar variavel local *)
				         let reg = Hashtbl.find tabFn.varLocais nome in
				         reg.tipagem
				     		with Not_found -> (* tenta encontrar parametro *)
				           			(match (procuraParam nome tabFn.param) with
				                			  Some v ->  v.tipagem;
				              			| None -> print_endline("None");(* tenta encontrar variavel global *)
							                try (match (Hashtbl.find amb nome) with
							                    (EntVar v) ->   imprime_tbl amb;v.tipagem
							            	| _ -> print_endline("Não Acha!");Hashtbl.add (tabFn.varLocais) nome  (cria_ent_var (Some tipo));
							                  	     (Some TInt)
							                )
							            with Not_found -> Hashtbl.add (tabFn.varLocais) nome  (cria_ent_var (Some tipo));
							                  	     (Some TInt)
				            		)
				    )

		  	with e-> failwith("Erro: insere_var1");
		  	raise e
	) else
	(
		try
			let ent = Hashtbl.find amb nome in
				( match ent with
					(EntVar t) -> t.tipagem
					| EntFn _ -> print_endline("O nome esta associado a funcao.");
					failwith "Erro semantico: insere_var2"
				)
		with
			Not_found ->  Hashtbl.add amb nome (EntVar (cria_ent_var (Some tipo)));
					(Some TInt)
				         | _ -> failwith("Erro: insere_var3")

	)

let verifica_var amb nome current =
		let tabVar = Hashtbl.find amb current in
			 match tabVar with
			  EntFn tabFn ->
			    (try (* tenta encontrar variavel local *)
			         let reg = Hashtbl.find tabFn.varLocais nome in
			         reg.tipagem
			     		with Not_found -> (* tenta encontrar parametro *)
			           			(match (procuraParam nome tabFn.param) with
			                			  Some v ->   v.tipagem
			              			| None ->  ("O nome ’" ^ current ^ "’ aparece.");(* tenta encontrar variavel global *)
			              			   try
				              			 let entrada = Hashtbl.find amb nome in
							                (match entrada with
							                    (EntVar v) ->   v.tipagem
							                  | _ -> failwith "busca_var_fun: erro"
							                )
						             with e-> print_endline "Erro: verifica_var";
						                           raise e
			            		)
			    )
			    | _ -> failwith "busca_var_fun: erro"


let rec verifica_var_dir amb pos var current =
	match var with
	| VarSimples nome ->  verifica_var amb nome current


let rec verifica_var_esq amb pos var tipo current =
	match var with
	| VarSimples nome ->  insere_var amb nome tipo current

(* Verifica os tipos da atribuicao e infere os tipos para as variaveis *)
and verifica_tipos_atrib e1 e2 amb =
	let t1 = tipo e1 and t2 = tipo e2 in
	begin
		if(t1 == (Some TInt)) then
			e2.tipo <- t2
		else e1.tipo <- t1
	end

(* Verifica se as parcelas correspondem aos tipos dos operadores *)
and verifica_primitiva op t1 t2 =
     try
	let tipos_op = Hashtbl.find ambfun op in
		verifica_op t1 t2 tipos_op
     with e-> print_endline "Erro: verifica_primitiva" ;
                  raise e

(* Verifica expressao *)
and verifica_exp_dir amb expr current =
	match expr.valor with
		Some (ExpInt _ )-> expr.tipo <- (Some TInt)
		| Some(ExpFloat _ )-> expr.tipo <- (Some TFloat)
		| Some (ExpString _) -> expr.tipo <- (Some TString)
		| Some(ExpBool _)    -> expr.tipo <- (Some TBool)
		| Some(ExpVar v) -> expr.tipo <-  (verifica_var_dir amb expr.pos v current)
		| Some(ExpUn (not,expressao)) -> verifica_exp_dir amb expressao current;
									(if (expressao.tipo == (Some TGen)) then
										erro "verifica_exp_dir" expressao.pos " operador not usado com variavel nao inicializada ");
										expr.tipo <-(Some TBool) (*nao importa qual a expressao o not sempre vai ser bool*)
		| Some(ExpBin (op,e1,e2)) -> verifica_exp_dir amb e1 current;
							   verifica_exp_dir amb e2 current;
							   	expr.tipo <- verifica_primitiva op (tipo e1) (tipo e2)
		| _ -> print_endline ("A expressao contem erros: ");
		failwith "Erro semantico: verifica_exp_dir"

and verifica_exp_esq amb expr tipo current =
	match expr.valor with
		Some (ExpInt _ )-> expr.tipo <- (Some TInt)
		| Some(ExpFloat _ )-> expr.tipo <- (Some TFloat)
		| Some (ExpString _) -> expr.tipo <- (Some TString)
		| Some(ExpBool _)    -> expr.tipo <- (Some TBool)
		| Some(ExpVar v) -> expr.tipo <-  (verifica_var_esq amb expr.pos v tipo current)
		| Some(ExpUn (not,expressao)) -> verifica_exp_dir amb expressao current;
									(if (expressao.tipo == (Some TGen)) then
										erro "verifica_exp_dir" expressao.pos " operador not usado com variavel nao inicializada ");
										expr.tipo <-(Some TBool) (*nao importa qual a expressao o not sempre vai ser bool*)
		| Some(ExpBin (op,e1,e2)) -> verifica_exp_dir amb e1 current;
							   verifica_exp_dir amb e2 current;
							   	expr.tipo <- verifica_primitiva op  e1.tipo e2.tipo
		| _ -> print_endline ("A expressao contem erros: ");
		failwith "Erro semantico: verifica_exp_dir"
and verifica_retorno_func amb nomeFunc =
     try
	let reg = Hashtbl.find amb nomeFunc in
		match reg with
		| EntFn tab -> tab.tiporetorno
		| _ -> failwith "ChamaFuncaoAtrib: não é funcao"
 with e-> print_endline "Erro: verifica_retorno_func";
                  raise e
let tipo_var_retorno amb regfn v =
     try
	let reg = busca_var_fun amb regfn v in
		reg.tipagem
      with e-> print_endline "Erro: tipo_var_retorno";
                  raise e


(* Retorna o tipo de retorno de uma funcao *)
let rec tipo_retorno amb expr current param =
    try
	let entrada = Hashtbl.find amb current in
		(match entrada with
			 EntFn entFun -> (match expr.valor with
					 	(Some ExpInt _) -> expr.tipo <- (Some TInt); expr.tipo
						| (Some ExpFloat _) -> expr.tipo <- (Some TFloat); expr.tipo
						| (Some ExpString _) -> expr.tipo <- (Some TString); expr.tipo
						| (Some ExpBool _ )   -> expr.tipo <- (Some TBool); expr.tipo
						| (Some ExpUn (not,expressao)) -> expr.tipo <-(Some TBool); expr.tipo (*nao importa qual a expressao o not sempre vai ser bool*)
						| (Some ExpGen) -> expr.tipo <- (Some TGen); expr.tipo
						| (Some ExpVar (VarSimples v)) -> expr.tipo <- (tipo_var_retorno amb entrada v);
												    expr.tipo
						| (Some ExpBin (op,e1,e2)) -> ignore(tipo_retorno amb e1 current param);
											   ignore(tipo_retorno amb e2 current param);
											   expr.tipo <- (verifica_primitiva op (tipo e1) (tipo e2)); expr.tipo
						| _ -> print_endline ("O nome ’" ^ current ^ "’ esta associado a uma variavel.");
						failwith "Erro semantico: tipo_var_retorno")
			| _ -> print_endline ("O nome ’" ^ current ^ "’ esta associado a uma variavel.");
			failwith "Erro semantico: tipo_var_retorno"
		)
     with e-> print_endline "Erro: tipo_retorno";
                  raise e

(* Verifica o retorno de uma funcao *)
let retorna_tipo_funcao amb nomeFuncao =
	try
	let tab = Hashtbl.find amb nomeFuncao in
	(match tab with
		| EntFn entFun -> (match entFun.tiporetorno with Some tipo -> tipo
					| None -> print_endline ("A funcao ’" ^ nomeFuncao ^ "’ nao	 tem um tipo definido.");
				 failwith "Erro semantico: retorna_tipo_funcao")
		| _ -> print_endline ("O nome ’" ^ nomeFuncao ^ "’ esta associado a uma variavel.");
	failwith "Erro semantico: retorna_tipo_funcao")
	with
	Not_found -> print_endline ("A funcao ’" ^ nomeFuncao ^ "’ nao foi definida.");
	failwith "Erro semantico: retorna_tipo_funcao"

(* Verifica os argumentos que sao variaveis *)
let verifica_var_arg amb var =
	try
	let entrada = Hashtbl.find amb var in
	(match entrada with
		| EntVar entVar -> entVar.tipagem
		| _ -> print_endline ("O nome ’" ^ var ^ " ’esta associado a uma funcao.");
		failwith "Erro semantico: verifica_var_arg")
	with
	Not_found -> print_endline ("Variavel ’" ^ var ^ "’ nao definida.");
	failwith "Erro semantico: verifica_var_arg"

(* Verifica argumentos *)
let rec verifica_args args =
	(match args with [] -> ignore()
		 | arg :: args -> verifica_arg arg;
				  verifica_args args)

(* Verifica argumento *)
and verifica_arg arg =
	(match arg.valor with
		(Some ExpInt _ )-> arg.tipo <- (Some TInt)
		| (Some ExpFloat _ ) -> arg.tipo <- (Some TFloat)
		| (Some ExpString _ )-> arg.tipo <- (Some TString)
	(* 	| (Some ExpVar (VarSimples v)) -> arg.tipo <- (verifica_var_arg amb v) *)
		| _ -> print_endline ("Nao e permitido usar operacao como argumento de chamada de funcao");
		failwith "Erro semantico: verifica_arg")

(* Verifica comandos *)
let rec verifica_cmds amb cmds current param =
	imprime_tbl amb;
	match cmds with [] -> ignore()
		| cmd :: cmds -> verifica_cmd amb cmd current param;
				   verifica_cmds amb cmds current param


(* Verifica comando *)
and verifica_cmd amb cmd current param =
	imprime_tbl amb;
	match cmd.vcmd with
		| ChamaFuncaoAtrib (e1, nomeFunc, arg) -> let tiporetorno = verifica_retorno_func amb nomeFunc in
								(match tiporetorno with
									| (Some TInt) -> verifica_exp_esq amb e1 TInt current
									| (Some TFloat) -> verifica_exp_esq amb e1 TFloat current
									| (Some TString) -> verifica_exp_esq amb e1 TString current
									| (Some TBool) -> verifica_exp_esq amb e1 TBool current
									| _ -> failwith("Erro: CmdAtrib")(* ;
								verifica_args arg *)
					)
		| CmdPrint (e) -> verifica_exp_dir amb e current
		| CmdInput (e1, e2) -> e1.tipo <- (Some TString);
					verifica_exp_esq amb e1 TString current;
					verifica_exp_dir amb e2 current

		| CmdIntParse (e1, e2) ->  ignore()
		| CmdAtrib (e1,e2) ->  imprime_tbl amb;verifica_exp_dir amb e2 current;
					let t1=tipo e2 in
					(match t1 with
						| (Some TInt) -> verifica_exp_esq amb e1 TInt current
						| (Some TFloat) -> verifica_exp_esq amb e1 TFloat current
						| (Some TString) -> verifica_exp_esq amb e1 TString current
						| (Some TBool) -> verifica_exp_esq amb e1 TBool current
						| _ -> failwith("Erro: CmdAtrib")
					)
					(* verifica_tipos_atrib e1 e2 amb *)
 		| CmdIf (e, ce, cs) -> verifica_exp_dir amb e current;
			begin verifica_cmds amb ce current param;
				(match cs with
					| None -> ignore()
					| Some cmds -> verifica_cmds amb cmds current param)
			end
		| CmdWhile (e, cs) -> verifica_exp_dir amb e current;
			let te = tipo e in
			begin
				if (te == (Some TGen)) then erro " (comando while)" cmd.pcmd "Condicao possui tipo generico";
				verifica_cmds amb cs current param;
			end
		| CmdFor (v, range, cmds) -> verifica_exp_dir amb v current;
	  		 v.tipo <- (Some TInt);
			(match range.vcmd with
				| CmdRange (ini, fim, inc) -> ignore ()
				| _ -> erro "range" cmd.pcmd "Range invalida");
	  		verifica_cmds amb cmds current param
		| CmdReturn (e) -> verifica_exp_dir amb e current
		| _ -> erro "verifica_cmd" cmd.pcmd "Comando nao definido. Erro Semantico."
(*Insere uma nova funcao na tabela*)
let insere_nova_funcao amb func =
	try
		let entrada = Hashtbl.find amb func.idF in
		(match entrada with
			| EntVar _ -> print_endline ("O nome ’" ^ func.idF ^ "’ esta associado a uma variavel.");
					 	 failwith "Erro semantico: insere_nova_funcao "
			| _ -> print_endline ("A funcao ’" ^ func.idF ^ "’ ja foi definida."
		);
	failwith "Erro semantico: insere_nova_funcao")
	with
	(* Not_found -> Hashtbl.add amb func.idF (EntFn (cria_ent_func func.returnF func.paramsF func.varLocaisF)) *)
	Not_found -> let t1= func.returnF in
		         (match t1 with
			  | (Some TInt) -> Hashtbl.add amb func.idF (EntFn (cria_ent_func TInt func.paramsF func.varLocaisF))(* ;
			  			imprime_tbl amb; *)
			  | (Some TFloat) -> Hashtbl.add amb func.idF (EntFn (cria_ent_func TFloat func.paramsF func.varLocaisF))
			  | (Some TString) -> Hashtbl.add amb func.idF (EntFn (cria_ent_func TString func.paramsF func.varLocaisF))
			  | (Some TBool) -> Hashtbl.add amb func.idF (EntFn (cria_ent_func TBool func.paramsF func.varLocaisF))
			  | _ -> failwith("Erro: insere_nova_funcao")
		         )

(* Verificacao das funcoes *)
let rec verifica_funcs amb funcs =
	match funcs with [] -> ignore()
		| func :: funcs -> verifica_func amb func; verifica_funcs amb funcs
(* Verificacao de funcao *)
and verifica_func amb func = insere_nova_funcao amb func;
				current_func := func.idF;
				print_endline ("A funcao ’" ^ func.idF ^ "’ ja foi inserida.");
				verifica_cmds amb func.cmdsF !current_func func.paramsF;
	try
		let entFun = Hashtbl.find amb !current_func in
			(match entFun with
				| EntFn funcao ->
					(* let params = verifica_params funcao.varLocais func.paramsF in *)
					let novo_reg = { varLocais = funcao.varLocais;
									tiporetorno = funcao.tiporetorno;
									param = funcao.param } in
					Hashtbl.replace amb !current_func (EntFn novo_reg);
					func.varLocaisF <- funcao.varLocais
				| _ -> print_endline ("O nome ’" ^ !current_func ^ "’ esta associado a uma varivel.");
			failwith "Erro semantico: verifica_func")
	with e-> print_endline "Erro: verifica_func EntFn";
	             raise e;
	try
		let entTab = Hashtbl.find amb !current_func in
			(match entTab with
				| EntFn entFunc -> (if (entFunc.tiporetorno == None) then
					entFunc.tiporetorno <- Some TVoid);
					func.returnF <- entFunc.tiporetorno
				| _ -> print_endline ("O nome ’" ^ !current_func ^ "’ esta associado a uma varivel.");
			failwith "Erro semantico: verifica_func")
	with e-> print_endline "Erro: verifica_func entTab";
		  raise e

(* Verifica o programa *)
let verifica_prog amb arv = verifica_funcs amb arv.funcsP;
			       current_func := "";
			       verifica_cmds amb arv.cmdsP !current_func []
let semantico arv =
	let ambiente = Hashtbl.create 23 in
		verifica_prog ambiente arv;
	imprime_tbl ambiente;
	ambiente
