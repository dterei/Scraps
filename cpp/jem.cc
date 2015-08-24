#define JEMALLOC_NO_RENAME
#include <jemalloc/jemalloc.h>

void alloc( void )
{
  je_malloc( 100 );
}

int main( void )
{
  alloc();
  return 0;
}
