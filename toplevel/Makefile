
NAME=toplevel
OBJS=toplevel.cmo

all: $(NAME).js

include ../Makefile.conf
-include ../Makefile.local

ifeq ($(OCAML4),"YES")
OCAMLLIB=ocamlcommon.cma ocamlbytecomp.cma ocamltoplevel.cma
else
OCAMLLIB=toplevellib.cma
endif

COMPILER_LIBS=$(shell ocamlc -where)/compiler-libs

ifeq ($(METAOCAML),1)
BER=metalib.cma bertop.cmo
BER_MODULE=trx runcode
endif

OCPINDENT=$(shell ocamlfind query ocp-indent -format "-package %p.lib" 2> /dev/null)

ifneq ($(OCPINDENT),)
OPTCOMP_OPT=-ppopt -let -ppopt ocpindent=true
endif

COMP=../compiler/$(COMPILER)
JSFILES=../runtime/runtime.js ../runtime/weak.js ../runtime/toplevel.js
OCAMLC=ocamlfind ocamlc -package lwt,optcomp -syntax camlp4o $(OCPINDENT) $(OPTCOMP_OPT) -ppopt "../lib/syntax/pa_js.cmo" \
	-I $(COMPILER_LIBS) -I ../compiler \
	-I ../lib
STDLIB= ../lib/$(LIBNAME).cma ../compiler/compiler.cma $(OCAMLLIB) $(BER)
EXPUNGE=$(shell ocamlc -where)/expunge
# Removed gc and sys
STDLIB_MODULES=\
  arg \
  array \
  arrayLabels \
  buffer \
  callback \
  camlinternalLazy \
  camlinternalMod \
  camlinternalOO \
  char \
  complex \
  digest \
  filename \
  format \
  genlex \
  hashtbl \
  int32 \
  int64 \
  lazy \
  lexing \
  list \
  listLabels \
  map \
  marshal \
  moreLabels \
  nativeint \
  obj \
  oo \
  parsing \
  pervasives \
  printexc \
  printf \
  queue \
  random \
  scanf \
  set \
  sort \
  stack \
  stdLabels \
  stream \
  string \
  stringLabels \
  sys \
  weak \
	lwt \
	bigarray

JOO=js dom typed_array dom_html file dom_events firebug lwt_js sys_js regexp CSS url \
  form xmlHttpRequest event_arrows lwt_js_events json webGL webSockets keycode
JOO=js

PERVASIVES=$(STDLIB_MODULES) $(JOO) $(BER_MODULE) outcometree topdirs toploop

#toplevel.byte: $(OBJS:cmx=cmo) toplevel.cmo
#	ocamlfind ocamlc -linkall -g -package str -linkpkg toplevellib.cma -o $@.tmp $^

$(NAME).js: $(NAME).byte $(COMP) $(JSFILES)
	$(COMP) -toplevel -noruntime $(JSFILES) $(NAME).byte \
	-I $(COMPILER_LIBS) \
	-I $(shell ocamlfind query lwt) \
	-I ./ -I ../lib/ \
	$(OPTIONS)

$(NAME).byte: $(OBJS) ../compiler/compiler.cma
	$(OCAMLC) -linkall -package findlib,lwt,bigarray $(OCPINDENT) -linkpkg -o $@.tmp $(STDLIB) $(OBJS)
	$(EXPUNGE) $@.tmp $@ $(PERVASIVES)
	rm -f $@.tmp

%.cmo: %.ml ../compiler/compiler.cma
	$(OCAMLC) -c $<

../compiler/compiler.cma:
	$(MAKE) -C ../compiler compiler.cma

clean::
	rm -f *.cm[io] $(NAME).byte $(NAME).js

depend:
	ocamlfind ocamldep -syntax camlp4o -package optcomp -ppopt "../lib/syntax/pa_js.cmo" -I ../compiler *.ml *.mli > .depend

include .depend
