 .text
 .global main
 main:
  push {fp, lr}
  push {r4, r5, r6, r7, r8, r10, r12}
  mov fp, sp
  ldr r4, =.L.str1
  mov r0, r4
  bl _prints
  bl _println
  mov r0, #0
  adds sp, sp, #0
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
  .word 10
 .L.str0:
  .asciz "looping..."
  .word 11
 .L.str1:
  .asciz "end of loop"
