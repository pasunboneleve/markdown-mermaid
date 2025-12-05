EMACS ?= emacs
DEPS_DIR := libs
MARKDOWN_MODE_DIR := $(DEPS_DIR)/markdown-mode

# Explicitly point to the directory relative to root
LOAD_PATH := -L ./$(MARKDOWN_MODE_DIR) -L .

.PHONY: all compile test clean

all: compile test

# Logic: Clone repo, then immediately remove its internal .git folder
# so GitHub Actions treats it as just files, not a submodule.
compile:
	@echo "Checking dependencies..."
	@mkdir -p $(DEPS_DIR)
	@if [ ! -f "$(MARKDOWN_MODE_DIR)/markdown-mode.el" ]; then \
		echo "Downloading markdown-mode..."; \
		git clone --depth 1 https://github.com/jrblevin/markdown-mode.git $(MARKDOWN_MODE_DIR); \
		rm -rf $(MARKDOWN_MODE_DIR)/.git; \
	fi
	@echo "Compiling..."
	$(EMACS) -Q -batch $(LOAD_PATH) \
		--eval '(setq byte-compile-error-on-warn t)' \
		-f batch-byte-compile markdown-mermaid.el

test:
	@echo "Checking dependencies..."
	@mkdir -p $(DEPS_DIR)
	@if [ ! -f "$(MARKDOWN_MODE_DIR)/markdown-mode.el" ]; then \
		echo "Downloading markdown-mode..."; \
		git clone --depth 1 https://github.com/jrblevin/markdown-mode.git $(MARKDOWN_MODE_DIR); \
		rm -rf $(MARKDOWN_MODE_DIR)/.git; \
	fi
	@echo "Running tests..."
	$(EMACS) -Q -batch $(LOAD_PATH) \
		-l markdown-mermaid.el \
		-l tests/markdown-mermaid-tests.el \
		-f ert-run-tests-batch-and-exit

clean:
	@echo "Cleaning artifacts..."
	rm -f *.elc
	rm -f tests/*.elc
	rm -f mermaid-block-*
	rm -rf $(DEPS_DIR)
