; ModuleID = 'infin.c'
target datalayout = "e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f32:32:32-f64:32:64-v64:64:64-v128:128:128-a0:0:64-f80:32:32"
target triple = "i486-linux-gnu"

define i32 @main() {
entry:
	%retval = alloca i32		; <i32*> [#uses=2]
	%x = alloca float		; <float*> [#uses=1]
	%tmp = alloca i32		; <i32*> [#uses=2]
	%"alloca point" = bitcast i32 0 to i32		; <i32> [#uses=0]
	store float 0x7FF8000000000000, float* %x, align 4
	store i32 0, i32* %tmp, align 4
	%tmp1 = load i32* %tmp, align 4		; <i32> [#uses=1]
	store i32 %tmp1, i32* %retval, align 4
	br label %return

return:		; preds = %entry
	%retval2 = load i32* %retval		; <i32> [#uses=1]
	ret i32 %retval2
}
