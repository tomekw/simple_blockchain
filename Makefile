all: build run

build:
	gnatmake -Psimple_blockchain

run:
	target/simple_blockchain
