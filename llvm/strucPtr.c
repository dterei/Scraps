#include <stdio.h>

void ex(void)
{
	printf("Ex called!");
	return;
}

struct StrStruct {
	char  a[10];
	int   b;
	int   c;
};

struct StrStruct m2 = {"2Hello M2", 0x01, 0x02};

struct StrStruct m1 = {"1Hello M1", (int) (&ex), (int) (&m2.a) + 3};

int g = 1;

int main(void)
{
   int k = (int) &m1;
	printf("StrStruct.a  = %s\n", &(m1.a[2]));
	printf("StrStruct.b  = %s\n", (char*) m1.b);
	printf("StrStruct.c  = %s\n", (char*) m1.c);
	printf("StrStruct.c  = %d\n", m1.c);
	printf("StrStruct.m2 = %c\n", m2);
	printf("StrStruct    = %c\n", m1);
	printf("StrStruct.c  = %d\n", m1.c);
	printf("m2           = %c\n", m2);
	printf("m2.a         = %d\n", m2.b);
	printf("&m2          = %d\n", &m2);
	printf("&m2.a        = %d\n", &(m2.a));
	
	printf("\ng    = %d\n", g);
	printf("\n&g   = %d\n", &g);

	return 0;
}

