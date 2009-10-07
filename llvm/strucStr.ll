declare i32 @printf(i8* noalias , ...) nounwind 

%struct.StrStruct = type { [12 x i8] , i32, [12 x i32], float}

@m = global %struct.StrStruct { [12 x i8] c"Hello_World\00", i32 1, [12 x i32] undef, float undef }

@m3 = global %struct.StrStruct* @m

@m1 = global i8* getelementptr (%struct.StrStruct* @m, i32 0, i32 0, i32 0)

@m102 = global i8* inttoptr (i32 add ( i32 1, i32 ptrtoint (i8* getelementptr (%struct.StrStruct* @m, i32 0, i32 0, i32 0) to i32)) to i8*)

@m2 = global i32* getelementptr (%struct.StrStruct* @m, i32 0, i32 2, i32 6)

@.str1 = constant [26 x i8] c"StrStruct.a = %s, d = %d\0A\00"		

define i32 @main() {
entry:
	%retval = alloca i32		
	%tmp = alloca i32		
	%tmp3 = getelementptr [26 x i8]* @.str1, i32 0, i32 0		
	%tmp31 = load i8** @m102
	%tmp32 = load i32** @m2
	%tmp33 = load i32* %tmp32
	%tmp4 = call i32 (i8*, ...)* @printf( i8* noalias  %tmp3, i8* %tmp31, i32 %tmp33 ) nounwind 		
	store i32 0, i32* %tmp
	%tmp7 = load i32* %tmp
	store i32 %tmp7, i32* %retval
	br label %return

return:		
	%retval8 = load i32* %retval		
	ret i32 %retval8
}

