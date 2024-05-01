TS_PARSER_DIR ?= $(HOME)/.emacs.d/tree-sitter

all: ts-lang-module.so

.PHONY: ts-lang-module.so
ts-lang-module.so:
	@cmake -B build && make -C build

define ELISP
(progn
 (ielm)
 (dolist (cmd (list
    "(setq lang (ts-lang-load \"$(TS_PARSER_DIR)/libtree-sitter-query.so\"))"
    "(ts-lang--parser-info lang)"))
   (insert cmd)
   (ielm-send-input)))
endef
export ELISP

emacs: ts-lang-module.so
	@emacs -Q --module-assertions -L . -L build \
		--eval "(require 'ts-lang-module)" --eval "$$ELISP"
