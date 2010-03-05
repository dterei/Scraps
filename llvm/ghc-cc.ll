; RUN: llc < %s -tailcallopt -mtriple=i686-linux-gnu | FileCheck %s

; Check the GHC call convention works (x86-32)

@base = external global i32 ; assigned to register: EBX
@sp   = external global i32 ; assigned to register: EBP
@hp   = external global i32 ; assigned to register: EDI
@r1   = external global i32 ; assigned to register: ESI

define void @zap(i32 %a, i32 %b) nounwind {
entry:
	; CHECK: movl {{[0-9]*}}(%esp), %ebx
	; CHECK-NEXT: movl {{[0-9]*}}(%esp), %ebp
	; CHECK-NEXT: call _addtwo
	%0 = call cc 10 i32 @addtwo(i32 %a, i32 %b)
	; CHECK: call _foo
	call void @foo() nounwind
	ret void
}

define cc 10 i32 @addtwo(i32 %x, i32 %y) nounwind {
entry:
	; CHECK: leal (%ebx,%ebp), %eax
	%0 = add i32 %x, %y
	; CHECK-NEXT: ret
	ret i32 %0
}

define cc 10 void @foo() nounwind {
entry:
	; CHECK: movl base, %ebx
	; CHECK-NOT: popl %ebx
	%0 = load i32* @r1
	; CHECK: movl sp, %ebp
	; CHECK-NOT: popl %ebp
	%1 = load i32* @hp
	; CHECK: movl hp, %edi
	; CHECK-NOT: popl %edi
	%2 = load i32* @sp
	; CHECK: movl r1, %esi
	; CHECK-NOT: popl %esi
	%3 = load i32* @base
	; CHECK: jmp {{L?_?}}bar{{(\$stub)?}}
	tail call cc 10 void @bar( i32 %3, i32 %2, i32 %1, i32 %0 ) nounwind
	;tail call cc 10 void @bar( i32 %3, i32 %2, i32 %1, i32 %0 ) nounwind
	ret void
}

declare cc 10 void @bar(i32, i32, i32, i32)

