BIN=./bngl-to-kappa

all: $(BIN)

$(BIN):
	ocamlbuild -use-ocamlfind -yaccflags "--unused-token BACK_SLASH" main.native
	mv main.native $(BIN)

tests/%.ka: tests/%.bngl $(BIN)
	cat $< | $(BIN) > $@

clean:
	rm -rf _build $(BIN)
	rm -rf out

.PHONY: all clean
