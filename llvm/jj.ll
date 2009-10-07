declare i32 @printf(i8* noalias , ...) nounwind 

%struct.StrStruct = type { i8*, i32 }

@m = global %struct.StrStruct { i8* getelementptr ([12 x i8]* @.str, i32 0, i32 0), i32 1 }		

@.str = internal constant [12 x i8] c"Hello World\00"		
@.str1 = internal constant [18 x i8] c"StrStruct.a = %s\0A\00"		
@.str2 = internal constant [18 x i8] c"StrStruct   = %s\0A\00"		

define i32 @main() {
entry:
	%retval = alloca i32		
	%tmp = alloca i32		
	%tmp1 = getelementptr %struct.StrStruct* @m, i32 0, i32 0		
	%tmp2 = load i8** %tmp1
	%tmp3 = getelementptr [18 x i8]* @.str1, i32 0, i32 0		
	%tmp4 = call i32 (i8*, ...)* @printf( i8* noalias  %tmp3, i8* %tmp2 ) nounwind 		
	%tmp5 = getelementptr [18 x i8]* @.str2, i32 0, i32 0		
	%tmp6 = call i32 (i8*, ...)* @printf( i8* noalias  %tmp5, %struct.StrStruct* byval  @m ) nounwind 		
	store i32 0, i32* %tmp
	%tmp7 = load i32* %tmp
	store i32 %tmp7, i32* %retval
	br label %return

return:		
	%retval8 = load i32* %retval		
	ret i32 %retval8
}

