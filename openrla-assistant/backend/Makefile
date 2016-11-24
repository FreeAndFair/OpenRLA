local_install_root = $(shell stack path --local-install-root)
local_install_bin = ${local_install_root}/bin

db_path = ${local_install_bin}/openrla.db


all: build

.PHONY: clean
clean:
	rm -f ${db_path}

.PHONY: build
build:
	stack build

.PHONY: run
run:
	stack exec open-rla-exe
