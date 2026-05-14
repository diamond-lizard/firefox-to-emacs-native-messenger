# Makefile for firefox-to-emacs-native-messenger.

.PHONY: help test test-unit test-integration
.DEFAULT_GOAL := help

EMACS ?= emacs
TEST_FILE := firefox-to-emacs-native-messenger-tests.el

help:
	@echo "firefox-to-emacs-native-messenger -- available targets"
	@echo ""
	@echo "  test              run all ERT tests"
	@echo "  test-unit         run :unit-tagged ERT tests"
	@echo "  test-integration  run :integration-tagged ERT tests"
	@echo "  help              show this message (default)"

test:
	$(EMACS) --batch -Q -L . -l $(TEST_FILE) \
	  --eval '(ert-run-tests-batch-and-exit t)'

test-unit:
	$(EMACS) --batch -Q -L . -l $(TEST_FILE) \
	  --eval '(ert-run-tests-batch-and-exit (quote (tag :unit)))'

test-integration:
	$(EMACS) --batch -Q -L . -l $(TEST_FILE) \
	  --eval '(ert-run-tests-batch-and-exit (quote (tag :integration)))'
