# Makefile for poly-translate

EMACS ?= emacs
BATCH = $(EMACS) -Q -batch -L . -L backends -L tests

# Source files
MAIN_FILES = poly-translate.el \
             poly-translate-core.el \
             poly-translate-backend.el \
             poly-translate-ui.el

BACKEND_FILES = backends/poly-translate-google.el \
                backends/poly-translate-deepl.el \
                backends/poly-translate-llm.el

TEST_FILES = tests/test-helper.el \
             tests/poly-translate-test.el

ALL_FILES = $(MAIN_FILES) $(BACKEND_FILES)

# Default target
.PHONY: all
all: compile

# Run tests
.PHONY: test
test:
	@echo "Running tests..."
	@$(BATCH) -l ert -l tests/poly-translate-test.el -f ert-run-tests-batch-and-exit

# Run specific test
.PHONY: test-one
test-one:
	@echo "Running test: $(TEST)"
	@$(BATCH) -l ert -l tests/poly-translate-test.el --eval "(ert-run-tests-batch-and-exit '$(TEST))"

# Compile all files
.PHONY: compile
compile: clean
	@echo "Compiling files..."
	@$(BATCH) -f batch-byte-compile $(ALL_FILES)

# Clean compiled files
.PHONY: clean
clean:
	@echo "Cleaning compiled files..."
	@rm -f *.elc backends/*.elc tests/*.elc

# Check for issues (basic linting)
.PHONY: check
check:
	@echo "Checking for issues..."
	@$(BATCH) --eval "(progn \
	  (setq byte-compile-error-on-warn t) \
	  (batch-byte-compile))" $(ALL_FILES) 2>&1 | grep -E "(Warning|Error)" || echo "No issues found."

# Load poly-translate interactively for testing
.PHONY: run
run:
	@$(EMACS) -Q -L . -L backends --eval "(require 'poly-translate)"

# Install dependencies (gptel)
.PHONY: install-deps
install-deps:
	@echo "Installing dependencies..."
	@$(EMACS) -Q --batch --eval "\
	  (progn \
	    (require 'package) \
	    (add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t) \
	    (package-initialize) \
	    (package-refresh-contents) \
	    (package-install 'gptel))"

# Generate autoloads
.PHONY: autoloads
autoloads:
	@echo "Generating autoloads..."
	@$(BATCH) --eval "\
	  (progn \
	    (require 'autoload) \
	    (let ((generated-autoload-file (expand-file-name \"poly-translate-autoloads.el\"))) \
	      (update-directory-autoloads \".\" \"backends\")))"

# Help
.PHONY: help
help:
	@echo "poly-translate Makefile targets:"
	@echo "  make test          - Run all tests"
	@echo "  make test-one TEST=test-name - Run specific test"
	@echo "  make compile       - Byte compile all files"
	@echo "  make clean         - Remove compiled files"
	@echo "  make check         - Check for compilation warnings/errors"
	@echo "  make run           - Start Emacs with poly-translate loaded"
	@echo "  make install-deps  - Install required dependencies (gptel)"
	@echo "  make autoloads     - Generate autoload file"
	@echo "  make help          - Show this help message"