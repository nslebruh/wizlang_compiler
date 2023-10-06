section .data
NULL equ 0
SYS_exit equ 60
EXIT_SUCCESS equ 0
SYS_write equ 1
STDOUT equ 1
LF equ 10
message1 db "Hello world", LF, NULL

section .text
global _start
_start:
  mov rdi, message1 
  call printString
done:
  mov rax, SYS_exit
  mov rdi, EXIT_SUCCESS
  syscall

global printString
printString:
  push rbx
  ; -----
  ; Count characters in string.
  mov rbx, rdi
  mov rdx, 0
strCountLoop:
cmp byte [rbx], NULL
  je strCountDone
  inc rdx
  inc rbx
  jmp strCountLoop
strCountDone:
  cmp rdx, 0
  je prtDone
  ; -----
  ; Call OS to output string.
  mov rax, SYS_write ; system code for write()
  mov rsi, rdi ; address of chars to write
  mov rdi, STDOUT ; standard out
; RDX=count to write, set above
  syscall ; system call
; -----
; String printed, return to calling routine.
prtDone:
  pop rbx
  ret