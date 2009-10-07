; ModuleID = 'lt.c'
target datalayout = "e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f32:32:32-f64:32:64-v64:64:64-v128:128:128-a0:0:64-f80:32:32"
target triple = "i486-linux-gnu"
@.str = internal constant [8 x i8] c"a = %d\0A\00"		; <[8 x i8]*> [#uses=1]

define i32 @main() {
entry:
	%retval = alloca i32		; <i32*> [#uses=2]
	%a = alloca i32		; <i32*> [#uses=2]
	%z = alloca i32		; <i32*> [#uses=2]
	%y = alloca i32		; <i32*> [#uses=2]
	%x = alloca i32		; <i32*> [#uses=2]
	%tmp = alloca i32		; <i32*> [#uses=2]
	%"alloca point" = bitcast i32 0 to i32		; <i32> [#uses=0]
	store i32 4, i32* %x, align 4
	store i32 2, i32* %y, align 4
	store i32 2, i32* %z, align 4
	%tmp1 = load i32* %x, align 4		; <i32> [#uses=1]
	%tmp2 = load i32* %y, align 4		; <i32> [#uses=1]
	%tmp3 = icmp ugt i32 %tmp1, %tmp2		; <i1> [#uses=1]
	%tmp34 = zext i1 %tmp3 to i32		; <i32> [#uses=1]
	%tmp5 = load i32* %z, align 4		; <i32> [#uses=1]
	%tmp6 = shl i32 %tmp34, %tmp5		; <i32> [#uses=1]
	store i32 %tmp6, i32* %a, align 4
	%tmp7 = getelementptr [8 x i8]* @.str, i32 0, i32 0		; <i8*> [#uses=1]
	%tmp8 = load i32* %a, align 4		; <i32> [#uses=1]
	%tmp9 = call i32 (i8*, ...)* @printf( i8* noalias  %tmp7, i32 %tmp8 ) nounwind 		; <i32> [#uses=0]
	store i32 0, i32* %tmp, align 4
	%tmp10 = load i32* %tmp, align 4		; <i32> [#uses=1]
	store i32 %tmp10, i32* %retval, align 4
	br label %return

return:		; preds = %entry
	%retval11 = load i32* %retval		; <i32> [#uses=1]
	ret i32 %retval11
}

declare i32 @printf(i8* noalias , ...) nounwind 
