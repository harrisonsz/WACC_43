 .data
 .text
 .global main
 main:
  push {fp, lr}
  push {r4, r5, r6, r7, r8, r10, r12}
  mov fp, sp
  mov r4, #5
  push {r4}
  mov r4, #3
  push {r4}
  push {r0, r1, r2, r3}
  ldr r0, [fp, #-4]
  ldr r1, [fp, #-8]
  cmp r1, #0
  bleq _errDivZero
  bl __aeabi_idivmod
  mov r4, r0
  pop {r0, r1, r2, r3}
  push {r4}
  mov r0, #0
  add sp, sp, #12
  pop {r4, r5, r6, r7, r8, r10, r12}
  pop {fp, pc}
 .data
    .word 0
 .L._prints_str0:
    .asciz "%.*s"
 .text
 _prints:
  push {lr}
  mov r2, r0
  ldr r1, [r0, #-4]
  bl printf
  mov r0, #0
  bl fflush
  pop {pc}
