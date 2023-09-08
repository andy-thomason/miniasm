

test:
	x86_64-linux-gnu-as -al --listing-lhs-width=3 test.s -o test.o > test.list
	x86_64-linux-gnu-as -al --listing-lhs-width=3 testmodrm.s -o testmodrm.o > testmodrm.list
	x86_64-linux-gnu-as -al --listing-lhs-width=3 modrm.s -o modrm.o > modrm.list
	objdump -d modrm.o > modrm.dis

