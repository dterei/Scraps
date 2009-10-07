; ModuleID = './fmul.c'
target datalayout = "e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f32:32:32-f64:32:64-v64:64:64-v128:128:128-a0:0:64-f80:32:32"
target triple = "i486-linux-gnu"
@g = global float 0x41CFD54FE0000000		; <float*> [#uses=0]
@a = global float 0x3FF4CCCCC0000000		; <float*> [#uses=0]
@b = global double 1.300000e+00, align 8		; <double*> [#uses=0]
@.str = internal constant [4 x i8] c"%f\0A\00"		; <[4 x i8]*> [#uses=1]

define i32 @main() {
entry:
	%retval = alloca i32		; <i32*> [#uses=2]
	%h = alloca float		; <float*> [#uses=2]
	%g = alloca float		; <float*> [#uses=2]
	%f = alloca float		; <float*> [#uses=2]
	%tmp = alloca i32		; <i32*> [#uses=2]
	%"alloca point" = bitcast i32 0 to i32		; <i32> [#uses=0]
	store float 0x3FF7333340000000, float* %f, align 4
	store float 0x4002666660000000, float* %g, align 4
	%tmp1 = load float* %f, align 4		; <float> [#uses=1]
	%tmp2 = load float* %g, align 4		; <float> [#uses=1]
	%tmp3 = mul float %tmp1, %tmp2		; <float> [#uses=1]
	store float %tmp3, float* %h, align 4
	%tmp4 = load float* %h, align 4		; <float> [#uses=1]
	%tmp45 = fpext float %tmp4 to double		; <double> [#uses=1]
	%tmp6 = getelementptr [4 x i8]* @.str, i32 0, i32 0		; <i8*> [#uses=1]
	%tmp7 = call i32 (i8*, ...)* @printf( i8* noalias  %tmp6, double %tmp45 ) nounwind 		; <i32> [#uses=0]
	store i32 0, i32* %tmp, align 4
	%tmp8 = load i32* %tmp, align 4		; <i32> [#uses=1]
	store i32 %tmp8, i32* %retval, align 4
	br label %return

return:		; preds = %entry
	%retval9 = load i32* %retval		; <i32> [#uses=1]
	ret i32 %retval9
}

declare i32 @printf(i8* noalias , ...) nounwind 
