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
(*Insere uma nova funcao na tabela*)
let insere_nova_funcao amb func =
	try
	let entrada = Hashtbl.find amb func.idF in
	(match entrada with
	| EntVar _ -> print_endline ("O nome ’" ^ func.idF ^ "’ esta associado a uma variavel.");
				  failwith "Erro semantico: insere_nova_funcao "
	| _ -> print_endline ("A funcao ’" ^ func.idF ^ "’ ja foi definida.");
	failwith "Erro semantico: insere_nova_funcao")
	with
	Not_found -> Hashtbl.add amb func.idF (EntFn (cria_ent_func TGen func.paramsF func.varLocaisF))


(* Retorna a tabela das variaveis locais de uma funcao *)
let var_locais_func tabGeral nomeFun =
	try
	let entFn = Hashtbl.find tabGeral nomeFun in
		(match entFn with
		EntFn entFunc -> entFunc.varLocais
				  | _ -> print_endline ("O nome ’" ^ nomeFun ^ "’ esta associado a uma variavel.");
		failwith "Erro semantico: var_locais_func ")
	with
	Not_found -> failwith "var_locais_func"
(* Insere variavel em uma tabela e retorna o tipo, caso jÃ¡ exista apenas retorna o tipo *)
let insere_var tab nome tipo current =
	(* Variaveis na funcao corrente *)
	(* if (current <> "") then
	( *)
		let tabVar = Hashtbl.find tab current in
		  (* let reg = busca_var_fun tab tabVar nome in
		  	reg.tipagem
 *)
			 match tabVar with
			  EntFn tabVar ->
			    (try (* tenta encontrar variavel local *)
			         let reg = Hashtbl.find tabVar.varLocais nome in
			         reg.tipagem
			     		with Not_found -> (* tenta encontrar parametro *)
			           			(match (procuraParam nome tabVar.param) with
			                			Some v -> v.tipagem
			                			| None -> (* tenta encontrar variavel global *)
						                (match (Hashtbl.find tab nome) with
						                  EntVar v -> v.tipagem
						                  | _ -> Hashtbl.add tab nome (EntVar (cria_ent_var (Some tipo)));
						                  	     (Some tipo)
						                )
			            )
			    )
			    | _ -> failwith "busca_var_fun: erro"

		(* (match tabVar with
			EntFn entFun ->
			(try
			 let var =
			 	Hashtbl.find entFun.varLocais nome in
					(match var with
						EntVar t -> t.tipagem
					         | EntFn _ -> print_endline ("O nome ’" ^ nome ^ "’ esta associado a uma funcao.");
						failwith "Erro semantico: insere_var"
					) *)
				(* with
				Not_found -> Printf.printf "%s " nome;
				Hashtbl.add entFun.varLocais nome (EntVar	(cria_ent_var TGen)); TGen
				| _ -> failwith "Erro insere_var" *)
		(* ) *)



	(* (*Variaveis fora das funcoes*)
	else
	(
		try
		let ent =
			Hashtbl.find tab nome in
			(match ent with
				EntVar t -> t.tipagem
		   | EntFn _ -> print_endline ("O nome ’" ^ nome ^ "’ esta associado a uma funcao.");
		    failwith "Erro semantico: insere_var")
			with
			Not_found -> Hashtbl.add tab nome (EntVar (cria_ent_var TGen));
			TGen
	) *)
(* Adiciona variavel se nao existir *)
let rec verifica_var amb pos var tipo current =
	match var with
	| VarSimples nome ->  insere_var amb nome tipo current

(* Retorna tabela de simbolos corrente (de variaveis locais ou a geral)*)
and ret_tabela amb current =
	if (current <> "") then
		let entFun = Hashtbl.find amb current in
		(match entFun with
			| EntFn f -> f.varLocais
			| _ -> print_endline ("O nome ’" ^ current ^ "’ esta associado a uma variavel.");
		failwith "Erro semantico: ret_var")
	else  let entVar = Hashtbl.find amb current in
					(match entVar with
						| EntVar v -> let tab = Hashtbl.create 5 in
									     ( Hashtbl.add tab current v;
									      tab )
						| _ -> print_endline("Erro");
		failwith "Erro semantico: ret_var"
					)

(* Verifica os tipos da atribuicao e infere os tipos para as variaveis *)
and verifica_tipos_atrib e1 e2 amb =
	let t1 = tipo e1 and t2 = tipo e2 in
	begin
		if(t1 == (Some TInt)) then e2.tipo <- t2
		else e1.tipo <- t1
	end
											(*a*)
(* Verifica se as parcelas correspondem aos tipos dos operadores *)
and verifica_primitiva op t1 t2 =
	let tipos_op = Hashtbl.find ambfun op in
		verifica_op t1 t2 tipos_op

(* Verifica expressao *)
and verifica_exp_esq amb expr current =
	match expr.valor with
		Some (ExpInt _ )-> expr.tipo <- (Some TInt)
		| Some(ExpFloat _ )-> expr.tipo <- (Some TFloat)
		| Some (ExpString _) -> expr.tipo <- (Some TString)
		| Some(ExpBool _)    -> expr.tipo <- (Some TBool)
		| Some(ExpGen) -> expr.tipo <- (Some TGen)
		| Some(ExpVar v) -> expr.tipo <-  (verifica_var amb expr.pos v expr.tipo current)
		| Some(ExpUn (not,expressao)) -> verifica_exp amb expressao current;
									(if (expressao.tipo == (Some TGen)) then
										erro "verifica_exp" expressao.pos " operador not usado com variavel nao inicializada ");
										expr.tipo <-(Some TBool) (*nao importa qual a expressao o not sempre vai ser bool*)
		| Some(ExpBin (op,e1,e2)) -> verifica_exp amb e1 current;
							   verifica_exp amb e2 current;
							   	expr.tipo <- verifica_primitiva op (tipo e1) (tipo e2)
		| _ -> print_endline ("A expressão contem erros.");
		failwith "Erro semantico: verifica_exp"

and verifica_exp_dir amb expr current =
	match expr.valor with
		Some (ExpInt _ )-> expr.tipo <- (Some TInt)
		| Some(ExpFloat _ )-> expr.tipo <- (Some TFloat)
		| Some (ExpString _) -> expr.tipo <- (Some TString)
		| Some(ExpBool _)    -> expr.tipo <- (Some TBool)
		| Some(ExpGen) -> expr.tipo <- (Some TGen)
		| Some(ExpVar v) -> expr.tipo <-  (verifica_var amb expr.pos v expr.tipo current)
		| Some(ExpUn (not,expressao)) -> verifica_exp amb expressao current;
									(if (expressao.tipo == (Some TGen)) then
										erro "verifica_exp" expressao.pos " operador not usado com variavel nao inicializada ");
										expr.tipo <-(Some TBool) (*nao importa qual a expressao o not sempre vai ser bool*)
		| Some(ExpBin (op,e1,e2)) -> verifica_exp amb e1 current;
							   verifica_exp amb e2 current;
							   	expr.tipo <- verifica_primitiva op (tipo e1) (tipo e2)
		| _ -> print_endline ("A expressão contem erros.");
		failwith "Erro semantico: verifica_exp"

(* Retorna o tipo de uma variavel que sera retornada*)
(* let tipo_var_retorno v locais param =
	try
	let entrada = Hashtbl.find locais v in
		(match entrada with
			 EntVar entVar -> entVar.tipagem
			| _ -> print_endline ("O nome ’" ^ v ^ "’ esta associado a uma funcao.");
			failwith "Erro semantico: tipo_var_retorno")
		with
		Not_found -> (let arg = procuraParam v param in
					(match arg with
					 	| Some param -> (Some TGen)
					 	| None -> print_endline ("A variavel ’" ^ v ^ "’ nao esta definida.");
						failwith "Erro semantico: tipo_var_retorno"
					)
				) *)

let tipo_var_retorno amb regfn v =
	let reg = busca_var_fun amb regfn v in
		reg.tipagem


(* Retorna o tipo de retorno de uma funcao *)
let rec tipo_retorno amb expr current param =
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
		failwith "Erro semantico: tipo_var_retorno")


(* Verifica o retorno de uma funcao *)
let verifica_retorno amb expr current param =
	if (current <> "") then
		let entrada = Hashtbl.find amb current in
		(match entrada with
			EntFn entFunc -> entFunc.tiporetorno <- tipo_retorno amb expr current param
			| _ -> print_endline ("O nome ’" ^ current ^ "’ esta associado a uma variavel.");
		failwith "Erro semantico: verifica_retorno");
	else
		erro "verifica_retorno" expr.pos "O comando return deve ser usado dentro de uma funcao"
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

(* Retorna a lista de parametros de uma funcao *)
let retorna_param_funcao amb nomeFuncao =
	try
	let tab = Hashtbl.find amb nomeFuncao in
		(match tab with
			| EntFn entFun -> entFun.param
			| _ -> print_endline ("O nome ’" ^ nomeFuncao ^ "’ esta associado a uma variavel.");
		failwith "Erro semantico: retorna_param_funcao")
		with
		Not_found -> print_endline ("A funcao ’" ^ nomeFuncao ^ "’ nao foi definida.");
		failwith "Erro semantico: retorna_param_funcao"
(* Verifica os tipos dos parametros de uma funcao *)
(* let rec verifica_tipos_parametros param arg nomeFun =
	(match param with [] -> ignore()
		| p1 :: param -> (match arg with [] -> ignore()
		| arg1 :: arg -> (if (p1.tipoP = arg1.tipo) then verifica_tipos_parametros param arg nomeFun
				  else print_endline ("Os tipos dos argumentos nao correspondem aos tipos dos parametros da funcao " ^ nomeFun);
	failwith "Erro semantico:verifica_tipos_parametros"))) *)

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
	match cmds with [] -> ignore()
		| cmd :: cmds -> verifica_cmd amb cmd current param; verifica_cmds amb cmds current param
(* Verifica comando *)
and verifica_cmd amb cmd current param =
	match cmd.vcmd with
		| ChamaFuncaoAtrib (e1, nomeFunc, arg) ->
		(* verifica se pode ser feita a atribuicao *)
		  verifica_exp amb e1 current;
			let tipoFunc = retorna_tipo_funcao amb nomeFunc in
					(match tipoFunc with
						| TVoid -> erro "verifica_cmd" e1.pos ("Nao existe tipo para atribuicao. A funcao ’" ^ nomeFunc ^ "’ tem tipo Void.")
						| TGen -> ignore() (*limitacao*)
										(* | tip -> (match t1 with
										| (Some TGen) -> (match e1.valor with
													| (Some ExpVar (VarSimples var)) -> let entrada = Hashtbl.find (ret_tabela amb current) var in
																						(match entrada
																							with EntVar entVar -> entVar.tipagem <- tip;
																										e1.tipo <- tip
																							 	| _-> print_endline ("O nome" ^ current ^ "esta associado a uma funcao.");
																								failwith "Erro semantico: verifica_cmd")
													| _ -> erro "verifica_cmd" e1.pos "Expressao nao eh variavel.")
										| _ -> if (t1 <> tip) then erro "verifica_cmd" e1.pos "Os tipos para a atribuicao devem ser iguais. ") *)
					);
					verifica_args (*  (ret_tabela amb current)  *) arg
		(* verifica_tipos_parametros param arg nomeFunc *)
	 	| ChamaFuncaoVoid (nomeFunc, arg) -> verifica_args (*  (ret_tabela amb current) *) arg
			(* verifica_tipos_parametros param arg nomeFunc *)
		| CmdPrint (e) -> verifica_exp amb e current
		| CmdInput (e1, e2) -> verifica_exp amb e1 current;
			verifica_exp amb e2 current;
			e1.tipo <- (Some TString);
			(match e1.valor with
				| (Some ExpVar (VarSimples v)) -> (let tabVar = Hashtbl.find amb current in
		  							let reg = busca_var_fun amb tabVar v in
		  								reg.tipagem <- (Some TString))

					(* try let ent = Hashtbl.find (ret_tabela amb current) v in
									ent.tipagem *)(*
												(match ent with
													| EntVar var -> var.tipagem <- TString
													| _ -> failwith "Espera variavel, encontrou funcao") *)
											(* with
											Not_found -> Hashtbl.add (ret_tabela amb current) v (EntVar (cria_ent_var TString)) )*)
				| _ -> failwith "Espera variavel"
			)
		| CmdIntParse (e1, e2) -> verifica_exp amb e1 current;
			verifica_exp amb e2 current;
			e1.tipo <- (Some TInt);
			(match e1.valor with
				| (Some ExpVar (VarSimples v)) ->(let tabVar = Hashtbl.find amb current in
		  							let reg = busca_var_fun amb tabVar v in
		  								reg.tipagem <- (Some TInt))
								(*  (try let ent = Hashtbl.find (ret_tabela amb current) v in
											(match ent with
												| EntVar var -> var.tipagem <- TInt
												| _ -> failwith "Espera variavel, encontrou funcao")
											with
											Not_found -> Hashtbl.add (ret_tabela amb current) v (
											EntVar (cria_ent_var TInt))) *)
				| _ -> failwith "Espera variavel"
			)
		| CmdAtrib (e1,e2) -> verifica_exp amb e2 current;
			verifica_exp amb e2 current;
			verifica_tipos_atrib e1 e2 amb
 		| CmdIf (e, ce, cs) -> verifica_exp amb e current;
			begin verifica_cmds amb ce current param;
				(match cs with
					| None -> ignore()
					| Some cmds -> verifica_cmds amb cmds current param)
			end
		| CmdWhile (e, cs) -> verifica_exp amb e current;
			let te = tipo e in
			begin
				if (te == (Some TGen)) then erro " (comando while)" cmd.pcmd "Condicao possui tipo generico";
				verifica_cmds amb cs current param;
			end
		| CmdFor (v, range, cmds) -> verifica_exp amb v current;
	  		 v.tipo <- (Some TInt);
			(match range.vcmd with
				| CmdRange (ini, fim, inc) -> ignore ()
				| _ -> erro "range" cmd.pcmd "Range invalida");
	  		verifica_cmds amb cmds current param
		| CmdReturn (e) -> verifica_retorno amb e current param
		| _ -> erro "verifica_cmd" cmd.pcmd "Comando nao definido. Erro Semantico."
(* Verifica lista de parametros *)
(* let rec verifica_params locais par =
	match par with [] -> []
		| par :: params -> let tpar = verifica_param locais par in tpar :: verifica_params locais params
		  		and verifica_param locais param =
					try
					let entVar = Hashtbl.find locais param.idP in
					(match entVar with
						| EntVar var -> ( param.tipoP <- var.tipagem;
										  Hashtbl.remove locais param.idP;
										param )
						| _-> print_endline ("O nome ’" ^ param.idP ^ "’ esta associado a uma funcao.");
						failwith "Erro semantico: verifica_param"
					)
					with
					Not_found -> param *)

(* Verificacao das funcoes *)
let rec verifica_funcs amb funcs =
	match funcs with [] -> ignore()
		| func :: funcs -> verifica_func amb func; verifica_funcs amb funcs
(* Verificacao de funcao *)
and verifica_func amb func = insere_nova_funcao amb func;
current_func := func.idF;
verifica_cmds amb func.cmdsF !current_func func.paramsF;
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
	failwith "Erro semantico: verifica_func");

let entTab = Hashtbl.find amb !current_func in
	(match entTab with
		| EntFn entFunc -> (if (entFunc.tiporetorno == None) then
			entFunc.tiporetorno <- Some TVoid);
			func.returnF <- entFunc.tiporetorno
		| _ -> print_endline ("O nome ’" ^ !current_func ^ "’ esta associado a uma varivel.");
	failwith "Erro semantico: verifica_func")

(* Verifica o programa *)
let verifica_prog amb arv = verifica_funcs amb arv.funcsP;
current_func := "";
verifica_cmds amb arv.cmdsP !current_func []
let semantico arv =
	let ambiente = Hashtbl.create 23 in
		verifica_prog ambiente arv;
	ambiente
