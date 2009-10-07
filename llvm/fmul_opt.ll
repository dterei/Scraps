; ModuleID = 'fmul_opt.bc'
target datalayout = "e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f32:32:32-f64:32:64-v64:64:64-v128:128:128-a0:0:64-f80:32:32"
target triple = "i486-linux-gnu"
@g = global float 0x41CFD54FE0000000		; <float*> [#uses=0]
@a = global float 0x3FF4CCCCC0000000		; <float*> [#uses=0]
@b = global double 1.300000e+00, align 8		; <double*> [#uses=0]
@.str = internal constant [4 x i8] c"%f\0A\00"		; <[4 x i8]*> [#uses=1]

define i32 @main() {
entry:
	%tmp7 = tail call i32 (i8*, ...)* @printf(i8* noalias getelementptr ([4 x i8]* @.str, i32 0, i32 0), double 0x400AAE1480000000) nounwind		; <i32> [#uses=0]
	ret i32 0
}

declare i32 @printf(i8* noalias nocapture, ...) nounwind
