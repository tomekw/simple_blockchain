all: build run

build:
	mkdir target
	mkdir object
	gnatmake -Psimple_blockchain

run:
	target/simple_blockchain
