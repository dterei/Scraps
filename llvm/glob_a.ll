; ModuleID = 'glob_a.c'
target datalayout = "e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f32:32:32-f64:32:64-v64:64:64-v128:128:128-a0:0:64-f80:32:32"
target triple = "i486-linux-gnu"
@Mp = external global [0 x i32]		; <[0 x i32]*> [#uses=5]
@.str = internal constant [22 x i8] c"a = %d\0Ab = %d\0Ac = %d\0A\00"		; <[22 x i8]*> [#uses=1]
@.str1 = internal constant [15 x i8] c"d = %d\0Ae = %d\0A\00"		; <[15 x i8]*> [#uses=1]

define i32 @main() {
entry:
	%retval = alloca i32		; <i32*> [#uses=2]
	%e = alloca i32		; <i32*> [#uses=2]
	%d = alloca i32		; <i32*> [#uses=2]
	%c = alloca i32		; <i32*> [#uses=2]
	%b = alloca i32		; <i32*> [#uses=2]
	%a = alloca i32		; <i32*> [#uses=2]
	%tmp = alloca i32		; <i32*> [#uses=2]
	%"alloca point" = bitcast i32 0 to i32		; <i32> [#uses=0]
	%tmp1 = getelementptr [0 x i32]* @Mp, i32 0, i32 0		; <i32*> [#uses=1]
	%tmp2 = load i32* %tmp1, align 4		; <i32> [#uses=1]
	store i32 %tmp2, i32* %a, align 4
	%tmp3 = getelementptr [0 x i32]* @Mp, i32 0, i32 1		; <i32*> [#uses=1]
	%tmp4 = load i32* %tmp3, align 4		; <i32> [#uses=1]
	store i32 %tmp4, i32* %b, align 4
	%tmp5 = getelementptr [0 x i32]* @Mp, i32 0, i32 2		; <i32*> [#uses=1]
	%tmp6 = load i32* %tmp5, align 4		; <i32> [#uses=1]
	store i32 %tmp6, i32* %c, align 4
	ptrtoint i32* getelementptr ([0 x i32]* @Mp, i32 0, i32 0) to i32		; <i32>:0 [#uses=0]
	store i32 ptrtoint ([0 x i32]* @Mp to i32), i32* %d, align 4
	store i32 ptrtoint ([0 x i32]* @Mp to i32), i32* %e, align 4
	%tmp7 = getelementptr [22 x i8]* @.str, i32 0, i32 0		; <i8*> [#uses=1]
	%tmp8 = load i32* %a, align 4		; <i32> [#uses=1]
	%tmp9 = load i32* %b, align 4		; <i32> [#uses=1]
	%tmp10 = load i32* %c, align 4		; <i32> [#uses=1]
	%tmp11 = call i32 (i8*, ...)* @printf( i8* noalias  %tmp7, i32 %tmp8, i32 %tmp9, i32 %tmp10 ) nounwind 		; <i32> [#uses=0]
	%tmp12 = getelementptr [15 x i8]* @.str1, i32 0, i32 0		; <i8*> [#uses=1]
	%tmp13 = load i32* %d, align 4		; <i32> [#uses=1]
	%tmp14 = load i32* %e, align 4		; <i32> [#uses=1]
	%tmp15 = call i32 (i8*, ...)* @printf( i8* noalias  %tmp12, i32 %tmp13, i32 %tmp14 ) nounwind 		; <i32> [#uses=0]
	store i32 0, i32* %tmp, align 4
	%tmp16 = load i32* %tmp, align 4		; <i32> [#uses=1]
	store i32 %tmp16, i32* %retval, align 4
	br label %return

return:		; preds = %entry
	%retval17 = load i32* %retval		; <i32> [#uses=1]
	ret i32 %retval17
}

declare i32 @printf(i8* noalias , ...) nounwind 
