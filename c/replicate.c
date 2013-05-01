#include <stdio.h>
#define S(s) int main(void) { printf("#include <stdio.h>\n#define S(s) %s\nS(%s)\n", #s, #s); }
S(int main(void) { printf("#include <stdio.h>\n#define S(s) %s\nS(%s)\n", #s, #s); })
