.PHONY: all compile clean test

EMACS ?= emacs
BATCH := $(EMACS) -Q --batch -L .

EL_FILES := soql-ts-mode.el soql-capf.el soql-company.el ob-soql-core.el
ELC_FILES := $(EL_FILES:.el=.elc)

EXT_FILES := $(wildcard extensions/*.el)
EXT_ELC := $(EXT_FILES:.el=.elc)

all: compile

compile: $(ELC_FILES)

compile-extensions: $(EXT_ELC)

%.elc: %.el
	$(BATCH) -f batch-byte-compile $<

extensions/%.elc: extensions/%.el
	$(BATCH) -L extensions -f batch-byte-compile $<

clean:
	rm -f $(ELC_FILES) $(EXT_ELC)

test:
	$(BATCH) -l ert -l soql-ts-mode.el -l test/*.el -f ert-run-tests-batch-and-exit

lint:
	$(BATCH) -l package-lint -f package-lint-batch-and-exit $(EL_FILES)

check-parens:
	$(BATCH) --eval '(dolist (f (list $(patsubst %,"%",$(EL_FILES)))) (find-file f) (check-parens))'
