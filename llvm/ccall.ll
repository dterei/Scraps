; ModuleID = 'ccall.c'
target datalayout = "e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f32:32:32-f64:32:64-v64:64:64-v128:128:128-a0:0:64-f80:32:32"
target triple = "i486-linux-gnu"
@.str = internal constant [12 x i8] c"Hello World\00"		; <[12 x i8]*> [#uses=1]

define i32 @main() {
entry:
	%retval = alloca i32		; <i32*> [#uses=2]
	%tmp = alloca i32		; <i32*> [#uses=2]
	%"alloca point" = bitcast i32 0 to i32		; <i32> [#uses=0]
	%tmp1 = getelementptr [12 x i8]* @.str, i32 0, i32 0		; <i8*> [#uses=1]
	%tmp2 = call ccc i32 @puts( i8* %tmp1 ) nounwind 		; <i32> [#uses=0]
	store i32 0, i32* %tmp, align 4
	%tmp3 = load i32* %tmp, align 4		; <i32> [#uses=1]
	store i32 %tmp3, i32* %retval, align 4
	br label %return

return:		; preds = %entry
	%retval4 = load i32* %retval		; <i32> [#uses=1]
	ret i32 %retval4
}

declare ccc i32 @puts(i8*)
