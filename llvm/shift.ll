; ModuleID = 'shift.c'
target datalayout = "e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f32:32:32-f64:32:64-v64:64:64-v128:128:128-a0:0:64-f80:32:32"
target triple = "i486-linux-gnu"

define i32 @main() {
entry:
	%retval = alloca i32		; <i32*> [#uses=2]
	%b = alloca i32		; <i32*> [#uses=4]
	%a = alloca i32		; <i32*> [#uses=2]
	%tmp = alloca i32		; <i32*> [#uses=2]
	%"alloca point" = bitcast i32 0 to i32		; <i32> [#uses=0]
	store i32 4, i32* %a, align 4
	store i32 1, i32* %b, align 4
	%tmp1 = load i32* %b, align 4		; <i32> [#uses=1]
	%tmp2 = load i32* %a, align 4		; <i32> [#uses=1]
	%tmp3 = ashr i32 %tmp1, i32 4		; <i32> [#uses=1]
	store i32 %tmp3, i32* %b, align 4
	%tmp4 = load i32* %b, align 4		; <i32> [#uses=1]
	store i32 %tmp4, i32* %tmp, align 4
	%tmp5 = load i32* %tmp, align 4		; <i32> [#uses=1]
	store i32 %tmp5, i32* %retval, align 4
	br label %return

return:		; preds = %entry
	%retval6 = load i32* %retval		; <i32> [#uses=1]
	ret i32 %retval6
}
