#include "gmp.h"

int main (void){
  mpz_t integ;
  mpz_init_set_str (integ, "3141592653589793238462643383279502884", 10);
  mpz_clear(integ);
  return 0;
}

