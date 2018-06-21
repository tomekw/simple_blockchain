all: build run

build:
	mkdir -p target object
	gnatmake -Psimple_blockchain

run:
	target/simple_blockchain
