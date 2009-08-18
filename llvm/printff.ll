; ModuleID = 'printff.o'
target datalayout = "e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f32:32:32-f64:32:64-v64:64:64-v128:128:128-a0:0:64-f80:32:32"
target triple = "i486-linux-gnu"
@.str = internal constant [6 x i8] c"%.2f\0A\00"		; <[6 x i8]*> [#uses=1]

define i32 @main(i32 %argc, i8** %argv) {
entry:
	%argc_addr = alloca i32		; <i32*> [#uses=1]
	%argv_addr = alloca i8**		; <i8***> [#uses=1]
	%retval = alloca i32		; <i32*> [#uses=2]
	%f = alloca float		; <float*> [#uses=2]
	%tmp = alloca i32		; <i32*> [#uses=2]
	%"alloca point" = bitcast i32 0 to i32		; <i32> [#uses=0]
	store i32 %argc, i32* %argc_addr
	store i8** %argv, i8*** %argv_addr
	store float 7.000000e+00, float* %f, align 4
	%tmp1 = load float* %f, align 4		; <float> [#uses=1]
	%tmp12 = fpext float %tmp1 to double		; <double> [#uses=1]
	%tmp3 = getelementptr [6 x i8]* @.str, i32 0, i32 0		; <i8*> [#uses=1]
	%tmp4 = call i32 (i8*, ...)* @printf(i8* noalias %tmp3, double %tmp12) nounwind		; <i32> [#uses=0]
	store i32 0, i32* %tmp, align 4
	%tmp5 = load i32* %tmp, align 4		; <i32> [#uses=1]
	store i32 %tmp5, i32* %retval, align 4
	br label %return

return:		; preds = %entry
	%retval6 = load i32* %retval		; <i32> [#uses=1]
	ret i32 %retval6
}

declare i32 @printf(i8* noalias, ...) nounwind
