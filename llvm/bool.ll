; ModuleID = 'bool.c'
target datalayout = "e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f32:32:32-f64:32:64-v64:64:64-v128:128:128-a0:0:64-f80:32:32"
target triple = "i486-linux-gnu"
@.str = internal constant [2 x i8] c"A\00"		; <[2 x i8]*> [#uses=1]
@.str1 = internal constant [3 x i8] c"!A\00"		; <[3 x i8]*> [#uses=1]

define i32 @main() {
entry:
	%retval = alloca i32		; <i32*> [#uses=2]
	%a = alloca i32		; <i32*> [#uses=2]
	%tmp = alloca i32		; <i32*> [#uses=2]
	%"alloca point" = bitcast i32 0 to i32		; <i32> [#uses=0]
	store i32 1, i32* %a, align 4
	%tmp1 = load i32* %a, align 4		; <i32> [#uses=1]
	%tmp2 = icmp ne i32 %tmp1, 0		; <i1> [#uses=1]
	%tmp23 = zext i1 %tmp2 to i8		; <i8> [#uses=1]
	%toBool = icmp ne i8 %tmp23, 0		; <i1> [#uses=1]
	;br i1 %toBool, label %bb, label %bb6
	br i1 -1, label %bb, label %bb6

bb:		; preds = %entry
	%tmp4 = getelementptr [2 x i8]* @.str, i32 0, i32 0		; <i8*> [#uses=1]
	%tmp5 = call i32 @puts( i8* %tmp4 ) nounwind 		; <i32> [#uses=0]
	br label %bb9

bb6:		; preds = %entry
	%tmp7 = getelementptr [3 x i8]* @.str1, i32 0, i32 0		; <i8*> [#uses=1]
	%tmp8 = call i32 @puts( i8* %tmp7 ) nounwind 		; <i32> [#uses=0]
	br label %bb9

bb9:		; preds = %bb6, %bb
	store i32 0, i32* %tmp, align 4
	%tmp10 = load i32* %tmp, align 4		; <i32> [#uses=1]
	store i32 %tmp10, i32* %retval, align 4
	br label %return

return:		; preds = %bb9
	%retval11 = load i32* %retval		; <i32> [#uses=1]
	ret i32 %retval11
}

declare i32 @puts(i8*)
