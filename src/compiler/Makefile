.PHONY : build clean

build:
	mkdir -p ./bin

	cargo build --release
	 cp ./target/release/compiler ./bin/latc_x86_64

clean:
	rm -rf bin target