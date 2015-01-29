CAMLC=ocamlc
CAMLLEX=ocamllex
CAMLYACC=ocamlyacc

compInter: compSem interpretador.cmo

compSem: compSint semantico.cmo

compSint: lexIndenta.cmo sintatico.cmo

interpretador.cmo: asa.cmi interpretador.ml
	$(CAMLC) -c interpretador.ml

asa.cmi: asa.ml
	$(CAMLC) -c asa.ml

sintatico.cmo: sintatico.cmi sintatico.ml
	$(CAMLC) -c sintatico.ml

sintatico.cmi: sintatico.mli
	$(CAMLC) -c sintatico.mli

sintatico.ml: asa.cmi sintatico.mly
	$(CAMLYACC) -v sintatico.mly

sintatico.mli: asa.cmi sintatico.mly
	$(CAMLYACC) -v sintatico.mly

semantico.cmo: asa.cmi semantico.ml
	$(CAMLC) -c semantico.ml

lexico.cmo: sintatico.cmi lexico.ml
	$(CAMLC) -c lexico.ml

lexico.cmi: sintatico.cmi lexico.ml
	$(CAMLC) -c lexico.ml

lexico.ml: lexico.mll
	$(CAMLLEX) lexico.mll

lexIndenta.cmo: sintatico.cmi lexico.cmi lexIndenta.ml
	$(CAMLC) -c lexIndenta.ml

clean:
	rm *.cmo *.cmi lexico.ml sintatico.ml sintatico.mli *.output