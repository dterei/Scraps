; ModuleID = 'strucPtr.c'
target datalayout = "e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f32:32:32-f64:32:64-v64:64:64-v128:128:128-a0:0:64-f80:32:32"
target triple = "i486-linux-gnu"
	%struct.StrStruct = type { [10 x i8], i32, i32 }
@.str = internal constant [11 x i8] c"Ex called!\00"		; <[11 x i8]*> [#uses=1]
@m2 = global %struct.StrStruct {
    [10 x i8] c"2Hello M2\00", 
    i32 1, 
    i32 2 }		; <%struct.StrStruct*> [#uses=7]
@m1 = global %struct.StrStruct {
    [10 x i8] c"1Hello M1\00", 
    i32 ptrtoint (void ()* @ex to i32), 
    i32 add (i32 ptrtoint (%struct.StrStruct* @m2 to i32), i32 3) }		; <%struct.StrStruct*> [#uses=7]
@g = global i32 1		; <i32*> [#uses=2]
@.str1 = internal constant [19 x i8] c"StrStruct.a  = %s\0A\00"		; <[19 x i8]*> [#uses=1]
@.str2 = internal constant [19 x i8] c"StrStruct.b  = %s\0A\00"		; <[19 x i8]*> [#uses=1]
@.str3 = internal constant [19 x i8] c"StrStruct.c  = %s\0A\00"		; <[19 x i8]*> [#uses=1]
@.str4 = internal constant [19 x i8] c"StrStruct.c  = %d\0A\00"		; <[19 x i8]*> [#uses=2]
@.str5 = internal constant [19 x i8] c"StrStruct.m2 = %c\0A\00"		; <[19 x i8]*> [#uses=1]
@.str6 = internal constant [19 x i8] c"StrStruct    = %c\0A\00"		; <[19 x i8]*> [#uses=1]
@.str7 = internal constant [19 x i8] c"m2           = %c\0A\00"		; <[19 x i8]*> [#uses=1]
@.str8 = internal constant [19 x i8] c"m2.a         = %d\0A\00"		; <[19 x i8]*> [#uses=1]
@.str9 = internal constant [19 x i8] c"&m2          = %d\0A\00"		; <[19 x i8]*> [#uses=1]
@.str10 = internal constant [19 x i8] c"&m2.a        = %d\0A\00"		; <[19 x i8]*> [#uses=1]
@.str11 = internal constant [12 x i8] c"\0Ag    = %d\0A\00"		; <[12 x i8]*> [#uses=1]
@.str12 = internal constant [12 x i8] c"\0A&g   = %d\0A\00"		; <[12 x i8]*> [#uses=1]

define void @ex() {
entry:
	%tmp = getelementptr [11 x i8]* @.str, i32 0, i32 0		; <i8*> [#uses=1]
	%tmp1 = call i32 (i8*, ...)* @printf( i8* noalias  %tmp ) nounwind 		; <i32> [#uses=0]
	br label %return

return:		; preds = %entry
	ret void
}

declare i32 @printf(i8* noalias , ...) nounwind 

define i32 @main() {
entry:
	%retval = alloca i32		; <i32*> [#uses=2]
	%k = alloca i32		; <i32*> [#uses=1]
	%tmp = alloca i32		; <i32*> [#uses=2]
	%"alloca point" = bitcast i32 0 to i32		; <i32> [#uses=0]
	store i32 ptrtoint (%struct.StrStruct* @m1 to i32), i32* %k, align 4
	%tmp1 = getelementptr [19 x i8]* @.str1, i32 0, i32 0		; <i8*> [#uses=1]
	%tmp2 = getelementptr %struct.StrStruct* @m1, i32 0, i32 0		; <[10 x i8]*> [#uses=1]
	%tmp3 = getelementptr [10 x i8]* %tmp2, i32 0, i32 2		; <i8*> [#uses=1]
	%tmp4 = call i32 (i8*, ...)* @printf( i8* noalias  %tmp1, i8* %tmp3 ) nounwind 		; <i32> [#uses=0]
	%tmp5 = getelementptr %struct.StrStruct* @m1, i32 0, i32 1		; <i32*> [#uses=1]
	%tmp6 = load i32* %tmp5, align 4		; <i32> [#uses=1]
	%tmp67 = inttoptr i32 %tmp6 to i8*		; <i8*> [#uses=1]
	%tmp8 = getelementptr [19 x i8]* @.str2, i32 0, i32 0		; <i8*> [#uses=1]
	%tmp9 = call i32 (i8*, ...)* @printf( i8* noalias  %tmp8, i8* %tmp67 ) nounwind 		; <i32> [#uses=0]
	%tmp10 = getelementptr %struct.StrStruct* @m1, i32 0, i32 2		; <i32*> [#uses=1]
	%tmp11 = load i32* %tmp10, align 4		; <i32> [#uses=1]
	%tmp1112 = inttoptr i32 %tmp11 to i8*		; <i8*> [#uses=1]
	%tmp13 = getelementptr [19 x i8]* @.str3, i32 0, i32 0		; <i8*> [#uses=1]
	%tmp14 = call i32 (i8*, ...)* @printf( i8* noalias  %tmp13, i8* %tmp1112 ) nounwind 		; <i32> [#uses=0]
	%tmp15 = getelementptr %struct.StrStruct* @m1, i32 0, i32 2		; <i32*> [#uses=1]
	%tmp16 = load i32* %tmp15, align 4		; <i32> [#uses=1]
	%tmp17 = getelementptr [19 x i8]* @.str4, i32 0, i32 0		; <i8*> [#uses=1]
	%tmp18 = call i32 (i8*, ...)* @printf( i8* noalias  %tmp17, i32 %tmp16 ) nounwind 		; <i32> [#uses=0]
	%tmp19 = getelementptr [19 x i8]* @.str5, i32 0, i32 0		; <i8*> [#uses=1]
	%tmp20 = call i32 (i8*, ...)* @printf( i8* noalias  %tmp19, %struct.StrStruct* byval  @m2 ) nounwind 		; <i32> [#uses=0]
	%tmp21 = getelementptr [19 x i8]* @.str6, i32 0, i32 0		; <i8*> [#uses=1]
	%tmp22 = call i32 (i8*, ...)* @printf( i8* noalias  %tmp21, %struct.StrStruct* byval  @m1 ) nounwind 		; <i32> [#uses=0]
	%tmp23 = getelementptr %struct.StrStruct* @m1, i32 0, i32 2		; <i32*> [#uses=1]
	%tmp24 = load i32* %tmp23, align 4		; <i32> [#uses=1]
	%tmp25 = getelementptr [19 x i8]* @.str4, i32 0, i32 0		; <i8*> [#uses=1]
	%tmp26 = call i32 (i8*, ...)* @printf( i8* noalias  %tmp25, i32 %tmp24 ) nounwind 		; <i32> [#uses=0]
	%tmp27 = getelementptr [19 x i8]* @.str7, i32 0, i32 0		; <i8*> [#uses=1]
	%tmp28 = call i32 (i8*, ...)* @printf( i8* noalias  %tmp27, %struct.StrStruct* byval  @m2 ) nounwind 		; <i32> [#uses=0]
	%tmp29 = getelementptr %struct.StrStruct* @m2, i32 0, i32 1		; <i32*> [#uses=1]
	%tmp30 = load i32* %tmp29, align 4		; <i32> [#uses=1]
	%tmp31 = getelementptr [19 x i8]* @.str8, i32 0, i32 0		; <i8*> [#uses=1]
	%tmp32 = call i32 (i8*, ...)* @printf( i8* noalias  %tmp31, i32 %tmp30 ) nounwind 		; <i32> [#uses=0]
	%tmp33 = getelementptr [19 x i8]* @.str9, i32 0, i32 0		; <i8*> [#uses=1]
	%tmp34 = call i32 (i8*, ...)* @printf( i8* noalias  %tmp33, %struct.StrStruct* @m2 ) nounwind 		; <i32> [#uses=0]
	%tmp35 = getelementptr [19 x i8]* @.str10, i32 0, i32 0		; <i8*> [#uses=1]
	%tmp36 = getelementptr %struct.StrStruct* @m2, i32 0, i32 0		; <[10 x i8]*> [#uses=1]
	%tmp37 = call i32 (i8*, ...)* @printf( i8* noalias  %tmp35, [10 x i8]* %tmp36 ) nounwind 		; <i32> [#uses=0]
	%tmp38 = load i32* @g, align 4		; <i32> [#uses=1]
	%tmp39 = getelementptr [12 x i8]* @.str11, i32 0, i32 0		; <i8*> [#uses=1]
	%tmp40 = call i32 (i8*, ...)* @printf( i8* noalias  %tmp39, i32 %tmp38 ) nounwind 		; <i32> [#uses=0]
	%tmp41 = getelementptr [12 x i8]* @.str12, i32 0, i32 0		; <i8*> [#uses=1]
	%tmp42 = call i32 (i8*, ...)* @printf( i8* noalias  %tmp41, i32* @g ) nounwind 		; <i32> [#uses=0]
	store i32 0, i32* %tmp, align 4
	%tmp43 = load i32* %tmp, align 4		; <i32> [#uses=1]
	store i32 %tmp43, i32* %retval, align 4
	br label %return

return:		; preds = %entry
	%retval44 = load i32* %retval		; <i32> [#uses=1]
	ret i32 %retval44
}
