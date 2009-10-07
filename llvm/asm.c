#include <stdio.h>

int main(void)
{
	int a=10, b;
	asm ("movl %1, %%eax; movl %%eax, %0;"
			:"=r"(b)        /* output */
			:"r"(a)         /* input */
			:"%eax"         /* clobbered register */
	);       

	int base, sp, hp, r1;

	/* read */
	asm ("movl %%ebx, %0; movl %%ebp, %1; movl %%edi, %2; movl %%esi, %3;" :"=r"(base),"=r"(sp),"=r"(hp),"=r"(r1));       

	printf("a= %d, b = %d, base = %d, r1 = %d\n", a, b, base, r1);

	/* write */
	asm ("movl %0, %%ebx; movl %1, %%ebp; movl %2, %%edi; movl %3, %%esi;" ::"r"(base), "r"(sp), "r"(hp), "r"(r1));       

	printf("a= %d, b = %d, base = %d, r1 = %d\n", a, b, base, r1);
	return 0;
}

