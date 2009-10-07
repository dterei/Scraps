declare i32 @printf(i8* noalias , ...) nounwind 

@.str = internal constant [14 x i8] c"Hello World!\0A\00"		
@.str1 = internal constant [14 x i8] c"Hello Mumma!\0A\00"		

define i32 @main() {
entry:
	%retval = alloca i32		
	%tmp = alloca i32		
	%tmp1 = getelementptr [14 x i8]* @.str, i32 0, i32 0		
	%tmp2 = call i32 (i8*, ...)* @printf( i8* noalias  %tmp1 ) nounwind 		
	%tmp3 = getelementptr [14 x i8]* @.str, i32 0, i32 0		
	%tmp4 = call i32 (i8*, ...)* @printf( i8* noalias  %tmp3 ) nounwind 		
	%tmp5 = getelementptr [14 x i8]* @.str1, i32 0, i32 0		
	%tmp6 = call i32 (i8*, ...)* @printf( i8* noalias  %tmp5 ) nounwind 		
	store i32 0, i32* %tmp
	%tmp7 = load i32* %tmp
	store i32 %tmp7, i32* %retval
	br label %return

return:		
	%retval8 = load i32* %retval		
	ret i32 %retval8
}

