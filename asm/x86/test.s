
    .text

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

    addq %rax, (%rcx)
    addq %rax, 305419896(%rcx)
    addq %rax, 0(%rcx,%r10,1)
    addq %rax, 0(,%r10,1)

    # 67 prefix
    addl %eax, (%ecx)
    addl %r8d, (%ecx)

    # 67 66 prefix
    addw %ax, (%ecx)

    retq
