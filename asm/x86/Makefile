

test:
	cargo run --bin testall
	# x86_64-linux-gnu-as -al --listing-lhs-width=3 test.s -o test.o > test.list
	x86_64-linux-gnu-as -al --listing-lhs-width=3 testall.s -o testall.o > testall.list
	x86_64-linux-gnu-as -al --listing-lhs-width=3 modrm2.s -o modrm2.o > modrm2.list
	objdump -d modrm2.o > modrm2.dis

