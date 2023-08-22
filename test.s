 .text
 .global main
 main:
  push {fp, lr}
  push {r4, r5, r6, r7, r8, r10, r12}
  mov fp, sp
  mov r4, #7
  push {r4}
  mov r4, #1
  blvs _errOverflow
  mov r5, #1
  adds r4, r4, r5
  blvs _errOverflow
  push {r4}
  ldr r4, [fp, #-8]
  mov r0, r4
  bl _printi
  bl _println
  mov r0, #0
  adds sp, sp, #8
  pop {r4, r5, r6, r7, r8, r10, r12}
  pop {fp, pc}
 .data
  .word 4
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
 .data
  .word 2
 .L._printi_str0:
  .asciz "%d"
 .text
 _printi:
  push {lr}
  mov r1, r0
  ldr r0, =.L._printi_str0
  bl printf
  mov r0, #0
  bl fflush
  pop {pc}
 .data
  .word 52
 .L._errOverflow_str0:
  .asciz "fatal error: integer overflow or underflow occurred"
 .text
 _errOverflow:
  ldr r0, =.L._errOverflow_str0
  bl _prints
  mov r0, #255
  bl exit
 .data
  .word 0
 .L._println_str0:
  .asciz ""
 .text
 _println:
  push {lr}
  ldr r0, =.L._println_str0
  bl puts
  mov r0, #0
  bl fflush
  pop {pc}
 .data
