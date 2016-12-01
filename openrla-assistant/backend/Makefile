local_install_root = $(shell stack path --local-install-root)
local_install_bin = ${local_install_root}/bin

data_dir = ${local_install_bin}/data


all: build

.PHONY: clean
clean:
	rm -f ${data_dir}

.PHONY: build
build:
	stack build

.PHONY: run
run:
	stack exec open-rla-exe
