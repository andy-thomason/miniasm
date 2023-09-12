

test:
	cargo run --bin gen_asm
	x86_64-linux-gnu-as -al --listing-lhs-width=3 /tmp/testall.s -o /tmp/testall.o > tests/testall.list
	cargo test
