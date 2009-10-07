; ModuleID = 'asm.c'
target datalayout = "e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f32:32:32-f64:32:64-v64:64:64-v128:128:128-a0:0:64-f80:32:32"
target triple = "i486-linux-gnu"
@.str = internal constant [26 x i8] c"a= %d, b = %d, base = %d\0A\00"		; <[26 x i8]*> [#uses=1]

define i32 @main() {
entry:
	%retval = alloca i32		; <i32*> [#uses=2]
	%r1 = alloca i32		; <i32*> [#uses=2]
	%hp = alloca i32		; <i32*> [#uses=2]
	%sp = alloca i32		; <i32*> [#uses=2]
	%base = alloca i32		; <i32*> [#uses=3]
	%b = alloca i32		; <i32*> [#uses=2]
	%a = alloca i32		; <i32*> [#uses=3]
	%tmp = alloca i32		; <i32*> [#uses=2]
	%"alloca point" = bitcast i32 0 to i32		; <i32> [#uses=0]
	store i32 10, i32* %a, align 4
	%tmp1 = load i32* %a, align 4		; <i32> [#uses=1]
	%tmp2 = call i32 asm "movl $1, %eax; movl %eax, $0;", "=r,r,~{dirflag},~{fpsr},~{flags},~{ax}"( i32 %tmp1 ) nounwind 		; <i32> [#uses=1]
	store i32 %tmp2, i32* %b
	call void asm "movl %ebx, $0; movl %ebp, $1; movl %edi, $2; movl %esi, $3;", "=*r,=*r,=*r,=*r,~{dirflag},~{fpsr},~{flags}"( i32* %base, i32* %sp, i32* %hp, i32* %r1 ) nounwind 		; <i32> [#uses=1]
	%tmp4 = load i32* %base, align 4		; <i32> [#uses=1]
	%tmp5 = load i32* %sp, align 4		; <i32> [#uses=1]
	%tmp6 = load i32* %hp, align 4		; <i32> [#uses=1]
	%tmp7 = load i32* %r1, align 4		; <i32> [#uses=1]
	call void asm sideeffect "movl $0, %ebx; movl $1, %ebp; movl $2, %edi; movl $3, %esi;", "r,r,r,r,~{dirflag},~{fpsr},~{flags}"( i32 %tmp4, i32 %tmp5, i32 %tmp6, i32 %tmp7 ) nounwind 
	%tmp8 = getelementptr [26 x i8]* @.str, i32 0, i32 0		; <i8*> [#uses=1]
	%tmp9 = load i32* %a, align 4		; <i32> [#uses=1]
	%tmp10 = load i32* %b, align 4		; <i32> [#uses=1]
	%tmp11 = load i32* %base, align 4		; <i32> [#uses=1]
	%tmp12 = call i32 (i8*, ...)* @printf( i8* noalias  %tmp8, i32 %tmp9, i32 %tmp10, i32 %tmp11 ) nounwind 		; <i32> [#uses=0]
	store i32 0, i32* %tmp, align 4
	%tmp13 = load i32* %tmp, align 4		; <i32> [#uses=1]
	store i32 %tmp13, i32* %retval, align 4
	br label %return

return:		; preds = %entry
	%retval14 = load i32* %retval		; <i32> [#uses=1]
	ret i32 %retval14
}

declare i32 @printf(i8* noalias , ...) nounwind 
