EMACS ?= emacs
DEPS_DIR := ./libs
MARKDOWN_MODE_DIR := $(DEPS_DIR)/markdown-mode

# Set load path to point to the auto-downloaded directory
LOAD_PATH := -L $(MARKDOWN_MODE_DIR)

.PHONY: all compile test clean deps

all: compile test

# 1. The Dependency Target
# Checks if libs/markdown-mode exists. If not, clones it.
deps: $(MARKDOWN_MODE_DIR)

$(MARKDOWN_MODE_DIR):
	@echo "Dependency 'markdown-mode' missing. Downloading..."
	@mkdir -p $(DEPS_DIR)
	git clone --depth 1 https://github.com/jrblevin/markdown-mode.git $(MARKDOWN_MODE_DIR)

# 2. Compilation
# Depends on 'deps' so we know markdown-mode is present before compiling
compile: deps
	@echo "Compiling..."
	$(EMACS) -Q -batch $(LOAD_PATH) -L . \
		--eval '(setq byte-compile-error-on-warn t)' \
		-f batch-byte-compile markdown-mermaid.el

# 3. Testing
# Depends on 'deps' to ensure environment is ready
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

# Optional: cleans dependencies too if you want a fresh start
clean-deps: clean
	@echo "Removing dependencies..."
	rm -rf $(DEPS_DIR)
