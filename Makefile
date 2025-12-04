EMACS ?= emacs
DEPS_DIR := ./libs
MARKDOWN_MODE_DIR := $(DEPS_DIR)/markdown-mode

# Setup Load Path to include the downloaded dependency
LOAD_PATH := -L $(MARKDOWN_MODE_DIR)

.PHONY: all compile test clean deps

all: compile test

# 1. Dependency Target
# Checks if the directory exists. If not, it clones the repo.
deps:
	@mkdir -p $(DEPS_DIR)
	@if [ ! -d "$(MARKDOWN_MODE_DIR)" ]; then \
		echo "Downloading markdown-mode..."; \
		git clone --depth 1 https://github.com/jrblevin/markdown-mode.git $(MARKDOWN_MODE_DIR); \
	else \
		echo "Dependencies already installed."; \
	fi

# 2. Compilation
# We add 'deps' as a prerequisite so it runs BEFORE compilation
compile: deps
	@echo "Compiling..."
	$(EMACS) -Q -batch $(LOAD_PATH) -L . \
		--eval '(setq byte-compile-error-on-warn t)' \
		-f batch-byte-compile markdown-mermaid.el

# 3. Testing
# We add 'deps' as a prerequisite so it runs BEFORE testing
test: deps
	@echo "Running tests..."
	$(EMACS) -Q -batch $(LOAD_PATH) -L . \
		-l markdown-mermaid.el \
		-l tests/markdown-mermaid-tests.el \
		-f ert-run-tests-batch-and-exit

clean:
	@echo "Cleaning artifacts..."
	rm -f *.elc
	rm -f tests/*.elc
	rm -f mermaid-block-*

clean-deps: clean
	@echo "Removing dependencies..."
	rm -rf $(DEPS_DIR)
