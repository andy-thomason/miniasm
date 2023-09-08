
    .text

    addb %al, (%rax)
    addb %al, (%rcx)
    addb %al, (%rdx)
    addb %al, (%rbx)
    addb %al, (%rsp)
    addb %al, (%rbp)
    addb %al, (%rsi)
    addb %al, (%rdi)

    addb %al, 1(%rax)
    addb %al, 1(%rcx)
    addb %al, 1(%rdx)
    addb %al, 1(%rbx)
    addb %al, 1(%rsp)
    addb %al, 1(%rbp)
    addb %al, 1(%rsi)
    addb %al, 1(%rdi)

    addb %al, 127(%rax)
    addb %al, 127(%rcx)
    addb %al, 127(%rdx)
    addb %al, 127(%rbx)
    addb %al, 127(%rsp)
    addb %al, 127(%rbp)
    addb %al, 127(%rsi)
    addb %al, 127(%rdi)

    addb %al, 128(%rax)
    addb %al, 128(%rcx)
    addb %al, 128(%rdx)
    addb %al, 128(%rbx)
    addb %al, 128(%rsp)
    addb %al, 128(%rbp)
    addb %al, 128(%rsi)
    addb %al, 128(%rdi)

    addb %al, 256(%rax)
    addb %al, 256(%rcx)
    addb %al, 256(%rdx)
    addb %al, 256(%rbx)
    addb %al, 256(%rsp)
    addb %al, 256(%rbp)
    addb %al, 256(%rsi)
    addb %al, 256(%rdi)

    addb %al, %al
    addb %al, %cl
    addb %al, %dl
    addb %al, %bl
    addb %al, %spl
    addb %al, %bpl
    addb %al, %sil
    addb %al, %dil
    addb %al, %al
    addb %cl, %al
    addb %dl, %al
    addb %bl, %al
    addb %spl, %al
    addb %bpl, %al
    addb %sil, %al
    addb %dil, %al

    addb %bl, %cl
    # addb %bh, %ch
    addw %bx, %cx
    addl %ebx, %ecx
    addq %rbx, %rcx

    addb %bl, %r9b
    addw %bx, %r9w
    addl %ebx, %r9d
    addq %rbx, %r9

    # byte
    addb %al, %cl
    addb %al, %r9b

    # 66 prefix
    addw %ax, %cx
    addw %ax, %r9w
    addw %r8w, %r9w

    addl %eax, %ecx
    addl %eax, %r9d
    addl %r8d, %ecx
    addl %r8d, %r9d

    addq %rax, %rcx
    addq %rax, %r9
    addq %r8, %rcx
    addq %r8, %r9

    # 67 prefix
    addl %eax, (%ecx)
    addl %r8d, (%ecx)

    # 67 66 prefix
    addw %ax, (%ecx)

    # sib
    addb %al, (%rax,%rax,1)
    addb %al, (%rax,%rcx,1)
    addb %al, (%rax,%rdx,1)
    addb %al, (%rax,%rbx,1)
    # no rsp
    addb %al, (%rax,%rbp,1)
    addb %al, (%rax,%rsi,1)
    addb %al, (%rax,%rdi,1)

    addb %al, 1(%rax,%rax,1)
    addb %al, 1(%rax,%rcx,1)
    addb %al, 1(%rax,%rdx,1)
    addb %al, 1(%rax,%rbx,1)
    # no rsp
    addb %al, 1(%rax,%rbp,1)
    addb %al, 1(%rax,%rsi,1)
    addb %al, 1(%rax,%rdi,1)

    addb %al, (%rax,%rax,1)
    addb %al, (%rcx,%rax,1)
    addb %al, (%rdx,%rax,1)
    addb %al, (%rbx,%rax,1)
    addb %al, (%rsp,%rax,1)
    addb %al, (%rbp,%rax,1)
    addb %al, (%rsi,%rax,1)
    addb %al, (%rdi,%rax,1)

    addb %al, (%rax,%rax,1)
    addb %al, (%rax,%rax,2)
    addb %al, (%rax,%rax,4)
    addb %al, (%rax,%rax,8)

    addb %al, 10(%rbp)
    addb %al, 128(%rbp)
    addb %al, 10(%r12)
    addb %al, 128(%r12)

    addb %al, (,%rax,1)

    retw
    ret
    retq

