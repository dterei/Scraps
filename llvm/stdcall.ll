; ModuleID = 'stdcall.c'
target datalayout = "e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f32:32:32-f64:32:64-v64:64:64-v128:128:128-a0:0:64-f80:32:32"
target triple = "i486-linux-gnu"
@.str = internal constant [14 x i8] c"Hello from f!\00"		; <[14 x i8]*> [#uses=1]

define x86_stdcallcc i32 @f() {
entry:
	%retval = alloca i32		; <i32*> [#uses=1]
	%"alloca point" = bitcast i32 0 to i32		; <i32> [#uses=0]
	%tmp = getelementptr [14 x i8]* @.str, i32 0, i32 0		; <i8*> [#uses=1]
	%tmp1 = call i32 @puts( i8* %tmp ) nounwind 		; <i32> [#uses=0]
	br label %return

return:		; preds = %entry
	%retval2 = load i32* %retval		; <i32> [#uses=1]
	ret i32 %retval2
}

declare i32 @puts(i8*)

define i32 @main() {
entry:
	%retval = alloca i32		; <i32*> [#uses=2]
	%tmp = alloca i32		; <i32*> [#uses=2]
	%"alloca point" = bitcast i32 0 to i32		; <i32> [#uses=0]
	%tmp1 = call x86_stdcallcc i32 @f( ) nounwind 		; <i32> [#uses=0]
	store i32 0, i32* %tmp, align 4
	%tmp2 = load i32* %tmp, align 4		; <i32> [#uses=1]
	store i32 %tmp2, i32* %retval, align 4
	br label %return

return:		; preds = %entry
	%retval3 = load i32* %retval		; <i32> [#uses=1]
	ret i32 %retval3
}
