void _start(void)
{
  asm(
    "movl $1,%eax;"
    "xorl %ebx,%ebx;"
    "int $0x80"
  );
}
