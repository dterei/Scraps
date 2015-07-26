#include <stdio.h>
#include <stdlib.h>

int p( const char * str )
{
  return puts( str );
}

const int cglobal1 = p("const global variable 1");
int global2 = p("global variable 2");
const int cglobal3 = p("const global variable 3");

int f1( void )
{
  static int x = p( "static local variable" );
  return x++;
}

int f2( int y )
{
  const static int x = p( "const static local variable" );
  return y - x;
}

int main(void)
{
  p("\n- program start -\n");
  
  f1();
  f2(10);

  p("\n- program end -\n");
  return EXIT_SUCCESS;
}

