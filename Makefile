EMACS = emacs
SHELL := /bin/bash

all: build

.PHONY: lint clean checkdoc package-lint

build: chat.elc

clean:
	@# We do this so removed files are listed
	@if compgen -G "*.elc" > /dev/null; then \
	  for f in *.elc; do                     \
		echo "rm $$f" && rm $$f;         \
	  done                                   \
	fi
	@rm -rf bin

# Here we want run checkdoc-file, and error if it finds a lint.
lint: checkdoc package-lint

package-lint:
	$(EMACS) -Q --batch                                                        \
	  --eval "(progn (setq package-user-dir \"$$(pwd)/bin\")                   \
      (require 'package)                                                     \
      (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives) \
	    (package-initialize)                                                   \
      (unless package-archive-contents                                       \
        (package-refresh-contents))                                          \
	    (package-install 'package-lint))"                                      \
	  -f package-lint-batch-and-exit chat.el

checkdoc:
	$(EMACS) -Q --batch \
    --eval '(checkdoc-file "chat.el")' 2> lint.log
	@cat lint.log # Display the lint to the user.
	@[ "$$(cat lint.log)" == "" ] # Error if something was written

%.elc: %.el
	$(EMACS) -Q --batch -L .                       \
	  --eval "(setq byte-compile-error-on-warn t)" \
	  -f batch-byte-compile $<

