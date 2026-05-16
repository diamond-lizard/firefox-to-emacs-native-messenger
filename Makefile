# Makefile for firefox-to-emacs-native-messenger.

.PHONY: help compile test test-unit test-integration install-support activate install
.DEFAULT_GOAL := help

EMACS ?= emacs
SOURCE := firefox-to-emacs-native-messenger.el
COMPILED := $(SOURCE)c
TEST_FILE := firefox-to-emacs-native-messenger-tests.el
WRAPPER := firefox-to-emacs-native-messenger-wrapper
MANIFEST := tridactyl.json

PROJECT_DIR := $(shell pwd -L)
ELISP_SOURCE := $(PROJECT_DIR)/$(SOURCE)
WRAPPER_SOURCE := $(PROJECT_DIR)/$(WRAPPER)
MANIFEST_SOURCE := $(PROJECT_DIR)/$(MANIFEST)

# Installation paths.  DESTDIR is prepended for staging-root install tests
# and is empty for normal user installs.  Source paths intentionally do not
# use DESTDIR; only destination paths do.
CACHE_DIR := $(DESTDIR)$(HOME)/.cache/firefox-to-emacs-native-messenger
ELISP_TARGET := $(DESTDIR)$(HOME)/.emacs.d/soma/packages/firefox-to-emacs-native-messenger.el
WRAPPER_TARGET := $(DESTDIR)$(HOME)/bin/firefox-to-emacs-native-messenger-wrapper
MANIFEST_TARGET := $(DESTDIR)$(HOME)/.mozilla/native-messaging-hosts/tridactyl.json

help: ## Show this message (default target)
	@awk 'BEGIN {FS = ":.*##[ \t]*"} /^[A-Za-z0-9_.-]+:.*##/ {printf "  %-20s %s\n", $$1, $$2}' $(MAKEFILE_LIST)

# Byte-compile the bridge with warnings promoted to errors so any warning
# fails the build and, by transitive dependency, fails the test target.
# Every test target depends on $(COMPILED) so the .elc is always rebuilt
# before tests run; this eliminates the stale-.elc class of bug where
# (require ...) would load an outdated byte-compiled file because Emacs
# prefers .elc over .el unless load-prefer-newer is set.
$(COMPILED): $(SOURCE)
	$(EMACS) --batch -Q -L . \
  --eval '(setq byte-compile-error-on-warn t)' \
  -f batch-byte-compile $(SOURCE)

compile: $(COMPILED) ## Byte-compile the bridge with warnings-as-errors

test: $(COMPILED) ## Run all ERT tests (rebuilds .elc first)
	$(EMACS) --batch -Q -L . -l $(TEST_FILE) \
  --eval '(ert-run-tests-batch-and-exit t)'

test-unit: $(COMPILED) ## Run :unit-tagged ERT tests (rebuilds .elc first)
	$(EMACS) --batch -Q -L . -l $(TEST_FILE) \
  --eval '(ert-run-tests-batch-and-exit (quote (tag :unit)))'

test-integration: $(COMPILED) ## Run :integration-tagged ERT tests (rebuilds .elc first)
	$(EMACS) --batch -Q -L . -l $(TEST_FILE) \
  --eval '(ert-run-tests-batch-and-exit (quote (tag :integration)))'

# install-support: create the runtime cache directory at mode 0700 and
# install the elisp and wrapper symlinks under their per-user locations.
# Branched per symlink target: already-correct symlink is a no-op; foreign
# symlink is replaced; pre-existing regular file aborts the rule.
install-support: ## Create cache directory and elisp/wrapper symlinks
	@umask 077 && mkdir -p "$(CACHE_DIR)"
	@chmod 0700 "$(CACHE_DIR)"
	@mkdir -p "$(dir $(ELISP_TARGET))"
	@mkdir -p "$(dir $(WRAPPER_TARGET))"
	@sh -c 't="$(ELISP_TARGET)"; s="$(ELISP_SOURCE)"; \
  if [ -L "$$t" ]; then \
    if [ "$$(readlink "$$t")" = "$$s" ]; then \
      echo "$$t: already linked"; \
    else \
      rm "$$t" && ln -s "$$s" "$$t" && echo "$$t: replaced foreign symlink"; \
    fi; \
  elif [ -e "$$t" ]; then \
    echo "$$t: refusing to clobber non-symlink" >&2; exit 1; \
  else \
    ln -s "$$s" "$$t" && echo "$$t: created"; \
  fi'
	@sh -c 't="$(WRAPPER_TARGET)"; s="$(WRAPPER_SOURCE)"; \
  if [ -L "$$t" ]; then \
    if [ "$$(readlink "$$t")" = "$$s" ]; then \
      echo "$$t: already linked"; \
    else \
      rm "$$t" && ln -s "$$s" "$$t" && echo "$$t: replaced foreign symlink"; \
    fi; \
  elif [ -e "$$t" ]; then \
    echo "$$t: refusing to clobber non-symlink" >&2; exit 1; \
  else \
    ln -s "$$s" "$$t" && echo "$$t: created"; \
  fi'

# activate: install the Firefox native-messaging manifest symlink at the
# live path.  Five branches: absent (create); symlink to source (no-op);
# regular file with matching sha256 (delete + symlink, byte-equivalent so
# safe); regular file with non-matching sha256 (mv to backup + symlink);
# foreign symlink (mv to backup + symlink).  Backup name is the target
# plus a UTC timestamp; on collision the current PID is appended; if both
# names are taken the rule fails loudly rather than silently clobbering.
activate: ## Activate the manifest at the Firefox native-messaging path
	@mkdir -p "$(dir $(MANIFEST_TARGET))"
	@sh -c 't="$(MANIFEST_TARGET)"; s="$(MANIFEST_SOURCE)"; \
  ts=$$(date -u +%Y%m%dT%H%M%SZ); \
  pick_backup() { \
    b="$$1.$$ts"; \
    [ -e "$$b" ] && b="$$b.$$$$"; \
    if [ -e "$$b" ]; then \
      echo "$$1: backup target exists: $$b" >&2; \
      return 1; \
    fi; \
    printf "%s" "$$b"; \
  }; \
  if [ -L "$$t" ]; then \
    if [ "$$(readlink "$$t")" = "$$s" ]; then \
      echo "$$t: already activated"; \
    else \
      b=$$(pick_backup "$$t") || exit 1; \
      mv "$$t" "$$b" && ln -s "$$s" "$$t" \
        && echo "$$t: backed up foreign symlink to $$b; activated"; \
    fi; \
  elif [ -f "$$t" ]; then \
    tsha=$$(sha256sum < "$$t" | cut -d" " -f1); \
    ssha=$$(sha256sum < "$$s" | cut -d" " -f1); \
    if [ "$$tsha" = "$$ssha" ]; then \
      rm "$$t" && ln -s "$$s" "$$t" \
        && echo "$$t: replaced byte-equivalent regular file; activated"; \
    else \
      b=$$(pick_backup "$$t") || exit 1; \
      mv "$$t" "$$b" && ln -s "$$s" "$$t" \
        && echo "$$t: backed up to $$b; activated"; \
    fi; \
  elif [ -e "$$t" ]; then \
    echo "$$t: not a regular file, not a symlink; refusing" >&2; \
    exit 1; \
  else \
    ln -s "$$s" "$$t" && echo "$$t: activated"; \
  fi'

install: install-support activate ## Run install-support then activate
