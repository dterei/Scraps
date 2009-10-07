; ModuleID = 'voidPtr.c'
target datalayout = "e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f32:32:32-f64:32:64-v64:64:64-v128:128:128-a0:0:64-f80:32:32"
target triple = "i486-linux-gnu"
@s = global i8* getelementptr ([12 x i8]* @.str, i32 0, i32 0)		; <i8**> [#uses=1]
@.str = internal constant [12 x i8] c"Hello World\00"		; <[12 x i8]*> [#uses=1]

define i32 @main() {
entry:
	%retval = alloca i32		; <i32*> [#uses=2]
	%b = alloca i8*		; <i8**> [#uses=2]
	%a = alloca i8*		; <i8**> [#uses=1]
	%tmp = alloca i32		; <i32*> [#uses=2]
	%"alloca point" = bitcast i32 0 to i32		; <i32> [#uses=0]
	store i8* inttoptr (i64 1232131 to i8*), i8** %a, align 4
	%tmp1 = load i8** @s, align 4		; <i8*> [#uses=1]
	store i8* %tmp1, i8** %b, align 4
	%tmp2 = load i8** %b, align 4		; <i8*> [#uses=1]
	call void @mputs( i8* %tmp2 ) nounwind 
	store i32 0, i32* %tmp, align 4
	%tmp3 = load i32* %tmp, align 4		; <i32> [#uses=1]
	store i32 %tmp3, i32* %retval, align 4
	br label %return

return:		; preds = %entry
	%retval4 = load i32* %retval		; <i32> [#uses=1]
	ret i32 %retval4
}

declare void @mputs(i8*)
