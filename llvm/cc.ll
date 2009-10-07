; ModuleID = './cc.c'
target datalayout = "e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f32:32:32-f64:32:64-v64:64:64-v128:128:128-a0:0:64-f80:32:32"
target triple = "i486-linux-gnu"
@.str = internal constant [13 x i8] c"test_cc: %d\0A\00"		; <[13 x i8]*> [#uses=1]
@.str1 = internal constant [10 x i8] c"Base: %d\0A\00"		; <[10 x i8]*> [#uses=1]

define i32 @main() {
entry:
	%retval = alloca i32		; <i32*> [#uses=2]
	%sum = alloca i32		; <i32*> [#uses=2]
	%tmp = alloca i32		; <i32*> [#uses=2]
	%"alloca point" = bitcast i32 0 to i32		; <i32> [#uses=0]
	%tmp1R = call x86_fastcallcc {i32,i32,i32,i32} @test_cc( i32 1, i32 2, i32 3, i32 4 ) nounwind 		; <i32> [#uses=1]
	%tmp1 = extractvalue {i32,i32,i32,i32} %tmp1R, 0
	store i32 %tmp1, i32* %sum, align 4
	%tmp2 = getelementptr [13 x i8]* @.str, i32 0, i32 0		; <i8*> [#uses=1]
	%tmp3 = load i32* %sum, align 4		; <i32> [#uses=1]
	%tmp4 = call i32 (i8*, ...)* @printf( i8* noalias  %tmp2, i32 %tmp3 ) nounwind 		; <i32> [#uses=0]
	store i32 0, i32* %tmp, align 4
	%tmp5 = load i32* %tmp, align 4		; <i32> [#uses=1]
	store i32 %tmp5, i32* %retval, align 4
	br label %return

return:		; preds = %entry
	%retval6 = load i32* %retval		; <i32> [#uses=1]
	ret i32 %retval6
}

declare i32 @printf(i8* noalias , ...) nounwind 

define x86_fastcallcc {i32,i32,i32,i32} @test_cc(i32 %base, i32 %sp, i32 %hp, i32 %r1) {
entry:
	%base_addr = alloca i32		; <i32*> [#uses=2]
	%sp_addr = alloca i32		; <i32*> [#uses=2]
	%hp_addr = alloca i32		; <i32*> [#uses=2]
	%r1_addr = alloca i32		; <i32*> [#uses=2]
	%sum = alloca i32		; <i32*> [#uses=3]
	%tmp = alloca i32		; <i32*> [#uses=2]
	%"alloca point" = bitcast i32 0 to i32		; <i32> [#uses=0]
	store i32 %base, i32* %base_addr
	store i32 %sp, i32* %sp_addr
	store i32 %hp, i32* %hp_addr
	store i32 %r1, i32* %r1_addr
	%tmp1 = load i32* %base_addr, align 4		; <i32> [#uses=1]
	%tmp2 = load i32* %sp_addr, align 4		; <i32> [#uses=1]
	%tmp3 = add i32 %tmp1, %tmp2		; <i32> [#uses=1]
	%tmp4 = load i32* %hp_addr, align 4		; <i32> [#uses=1]
	%tmp5 = add i32 %tmp3, %tmp4		; <i32> [#uses=1]
	%tmp6 = load i32* %r1_addr, align 4		; <i32> [#uses=1]
	%tmp7 = add i32 %tmp5, %tmp6		; <i32> [#uses=1]
	store i32 %tmp7, i32* %sum, align 4
	%tmp8 = getelementptr [10 x i8]* @.str1, i32 0, i32 0		; <i8*> [#uses=1]
	%tmp9 = load i32* %sum, align 4		; <i32> [#uses=1]
	%tmp10 = call i32 (i8*, ...)* @printf( i8* noalias  %tmp8, i32 %tmp9 ) nounwind 		; <i32> [#uses=0]
	br label %return

return:		; preds = %entry
	%lbase = load i32* %base_addr
	%lsp = load i32* %sp_addr
	%lhp = load i32* %hp_addr
	%lr1 = load i32* %r1_addr
	%retval1 = insertvalue {i32,i32,i32,i32} {i32 0, i32 0, i32 0, i32 0}, i32 %lbase, 0
	%retval2 = insertvalue {i32,i32,i32,i32} %retval1, i32 %lsp, 1
	%retval3 = insertvalue {i32,i32,i32,i32} %retval2, i32 %lhp, 2
	%retval4 = insertvalue {i32,i32,i32,i32} %retval3, i32 %lr1, 3
	ret {i32,i32,i32,i32} %retval4
}

