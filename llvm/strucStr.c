#include <stdio.h>

struct StrStruct {
	char *a;
};

struct StrStruct m = {"Hello World"};

int main(void)
{
	printf("StrStruct.a = %s\n", &(m.a[2]));
	printf("StrStruct   = %s\n", m);
	return 0;
}

