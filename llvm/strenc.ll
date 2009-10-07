; ModuleID = 'strenc.c'
target datalayout = "e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f32:32:32-f64:32:64-v64:64:64-v128:128:128-a0:0:64-f80:32:32"
target triple = "i486-linux-gnu"
@test = global i8* getelementptr ([19 x i8]* @.str, i32 0, i32 0)		; <i8**> [#uses=1]
@.str = internal constant [19 x i8] c"\22'@hello\0A\0D\09Word!'~\00"		; <[19 x i8]*> [#uses=1]
@.str1 = internal constant [11 x i8] c"Test = %s\0A\00"		; <[11 x i8]*> [#uses=1]

define i32 @main() {
entry:
	%retval = alloca i32		; <i32*> [#uses=2]
	%tmp = alloca i32		; <i32*> [#uses=2]
	%"alloca point" = bitcast i32 0 to i32		; <i32> [#uses=0]
	%tmp1 = load i8** @test, align 4		; <i8*> [#uses=1]
	%tmp2 = getelementptr [11 x i8]* @.str1, i32 0, i32 0		; <i8*> [#uses=1]
	%tmp3 = call i32 (i8*, ...)* @printf( i8* noalias  %tmp2, i8* %tmp1 ) nounwind 		; <i32> [#uses=0]
	store i32 0, i32* %tmp, align 4
	%tmp4 = load i32* %tmp, align 4		; <i32> [#uses=1]
	store i32 %tmp4, i32* %retval, align 4
	br label %return

return:		; preds = %entry
	%retval5 = load i32* %retval		; <i32> [#uses=1]
	ret i32 %retval5
}

declare i32 @printf(i8* noalias , ...) nounwind 
