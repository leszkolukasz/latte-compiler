.PHONY : build clean test

TRANSPILER_DIR = "./src/transpiler"
COMPILER_DIR = "./src/compiler"

all: build

build:
	make -C ${TRANSPILER_DIR}
	make -C ${COMPILER_DIR}

	mkdir -p ./lib
	cp ${TRANSPILER_DIR}/Transpiler ./lib/transpiler
	cp ${COMPILER_DIR}/bin/latc_x86_64 .

test: build
	cp ./latc_x86_64 ./test
	cp ./lib/* ./test/lib
	cd ./test && ./test.sh ./latc_x86_64

clean:
	make -C ${TRANSPILER_DIR} clean
	make -C ${COMPILER_DIR} clean
	rm -f ./lib/transpiler latc_x86_64
	rm -f ./test/latc_x86_64
	rm -f ./test/lib/*
	find ./test/cases -type f \( ! -name "*.*" -o -name "*.s" \) -exec rm -f {} +
