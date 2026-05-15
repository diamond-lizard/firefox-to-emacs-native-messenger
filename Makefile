# Makefile for firefox-to-emacs-native-messenger.

.PHONY: help compile test test-unit test-integration
.DEFAULT_GOAL := help

EMACS ?= emacs
SOURCE := firefox-to-emacs-native-messenger.el
COMPILED := $(SOURCE)c
TEST_FILE := firefox-to-emacs-native-messenger-tests.el

help:
	@echo "firefox-to-emacs-native-messenger -- available targets"
	@echo ""
	@echo "  compile           byte-compile the bridge with warnings-as-errors"
	@echo "  test              run all ERT tests (rebuilds .elc first)"
	@echo "  test-unit         run :unit-tagged ERT tests (rebuilds .elc first)"
	@echo "  test-integration  run :integration-tagged ERT tests (rebuilds .elc first)"
	@echo "  help              show this message (default)"

# Byte-compile the bridge with warnings promoted to errors so any warning
# fails the build and, by transitive dependency, fails the test target.
# Every test target depends on $(COMPILED) so the .elc is always rebuilt
# before tests run; this eliminates the stale-.elc class of bug where
# `(require ...)` would load an outdated byte-compiled file because Emacs
# prefers .elc over .el unless `load-prefer-newer' is set.
$(COMPILED): $(SOURCE)
	$(EMACS) --batch -Q -L . \
	  --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile $(SOURCE)

compile: $(COMPILED)

test: $(COMPILED)
	$(EMACS) --batch -Q -L . -l $(TEST_FILE) \
	  --eval '(ert-run-tests-batch-and-exit t)'

test-unit: $(COMPILED)
	$(EMACS) --batch -Q -L . -l $(TEST_FILE) \
	  --eval '(ert-run-tests-batch-and-exit (quote (tag :unit)))'

test-integration: $(COMPILED)
	$(EMACS) --batch -Q -L . -l $(TEST_FILE) \
	  --eval '(ert-run-tests-batch-and-exit (quote (tag :integration)))'
