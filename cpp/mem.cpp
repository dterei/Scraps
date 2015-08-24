#include <iostream>
#include <unistd.h>

using namespace std;

int main( void )
{
  long pages = sysconf( _SC_PHYS_PAGES );
  long page_sz = sysconf( _SC_PAGE_SIZE );
  long avail_sz = sysconf( _SC_AVPHYS_PAGES );
  cout << "Pages: " << pages << endl;
  cout << "Avail: " << avail_sz << endl;
  cout << "Size:  " << page_sz << endl;
  cout << "Mem: " << pages * page_sz << endl;
  cout << "Mem: " << pages * page_sz / 1024 / 1024 << " MB" << endl;
  cout << "Mem: " << avail_sz * page_sz / 1024 / 1024 << " MB" << endl;
  return 0;
}
