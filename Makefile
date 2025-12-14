EMACS ?= emacs
DEP_DIR := libs/markdown-mode
DEP_FILE := $(DEP_DIR)/markdown-mode.el

.PHONY: all compile test clean

all: compile test

# This rule only runs if the file is missing
$(DEP_FILE):
	@echo "Installing dependency: markdown-mode..."
	@mkdir -p libs
	git clone --depth 1 https://github.com/jrblevin/markdown-mode.git $(DEP_DIR)
	rm -rf $(DEP_DIR)/.git

# Dependencies are automatically checked before compilation
compile: $(DEP_FILE)
	@echo "Compiling..."
	$(EMACS) -Q -batch -L $(DEP_DIR) -L . \
		--eval '(setq byte-compile-error-on-warn t)' \
		-f batch-byte-compile markdown-mermaid.el

test: $(DEP_FILE)
	@echo "Running tests..."
	$(EMACS) -Q -batch -L $(DEP_DIR) -L . \
		-l markdown-mermaid.el \
		-l tests/markdown-mermaid-tests.el \
		-f ert-run-tests-batch-and-exit

clean:
	@echo "Cleaning..."
	rm -f *.elc tests/*.elc mermaid-block-* mermaid-config-*
	rm -rf libs
