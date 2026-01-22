# SPDX-License-Identifier: AGPL-3.0-or-later
# FlatRacoon TUI - Makefile

.PHONY: all build clean run install test

all: build

build:
	gprbuild -P flatracoon_tui.gpr

clean:
	gprclean -P flatracoon_tui.gpr

run: build
	./bin/flatracoon_tui

install: build
	@echo "Installing to /usr/local/bin..."
	@sudo cp bin/flatracoon_tui /usr/local/bin/
	@echo "Installed successfully"

test:
	@echo "Running TUI tests..."
	./bin/flatracoon_tui help
	./bin/flatracoon_tui status
	./bin/flatracoon_tui health
	./bin/flatracoon_tui order
