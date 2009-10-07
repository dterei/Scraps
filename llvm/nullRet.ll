; ModuleID = 'nullRet.c'
target datalayout = "e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f32:32:32-f64:32:64-v64:64:64-v128:128:128-a0:0:64-f80:32:32"
target triple = "i486-linux-gnu"
@.str = internal constant [13 x i8] c"Hello World!\00"		; <[13 x i8]*> [#uses=1]

define void @fun(i32 %a) {
entry:
	%a_addr = alloca i32		; <i32*> [#uses=2]
	%"alloca point" = bitcast i32 0 to i32		; <i32> [#uses=0]
	store i32 %a, i32* %a_addr
	%tmp = load i32* %a_addr, align 4		; <i32> [#uses=1]
	%tmp1 = icmp eq i32 %tmp, 1		; <i1> [#uses=1]
	%tmp12 = zext i1 %tmp1 to i8		; <i8> [#uses=1]
	%toBool = icmp ne i8 %tmp12, 0		; <i1> [#uses=1]
	br i1 %toBool, label %bb5, label %bb

bb:		; preds = %entry
	%tmp3 = getelementptr [13 x i8]* @.str, i32 0, i32 0		; <i8*> [#uses=1]
	%tmp4 = call i32 @puts( i8* %tmp3 ) nounwind 		; <i32> [#uses=0]
	br label %bb5

bb5:		; preds = %bb, %entry
	br label %return

return:		; preds = %bb5
	ret void
}

declare i32 @puts(i8*)

define i32 @main() {
entry:
	%retval = alloca i32		; <i32*> [#uses=2]
	%tmp = alloca i32		; <i32*> [#uses=2]
	%"alloca point" = bitcast i32 0 to i32		; <i32> [#uses=0]
	call void @fun( i32 2 ) nounwind 
	store i32 0, i32* %tmp, align 4
	%tmp1 = load i32* %tmp, align 4		; <i32> [#uses=1]
	store i32 %tmp1, i32* %retval, align 4
	br label %return

return:		; preds = %entry
	%retval2 = load i32* %retval		; <i32> [#uses=1]
	ret i32 %retval2
}
