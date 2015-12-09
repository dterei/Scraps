#include <iostream>
#include <stdexcept>
#include <system_error>

void our_terminate(void);

namespace {
  /* Use C++ static initialization order to set termination handler before main
   * called.
   */
  static const bool SET_TERMINATE = std::set_terminate(our_terminate);
}

int main(void)
{
  // std::terminate();
  // throw 20;
  // throw 'a';
  // throw std::runtime_error("error!");

  // (int, error_category, string = ""
  throw std::system_error(2, std::system_category(), "system call failed!");
}

void our_terminate(void)
{
  std::cerr << "our_terminate called..." << std::endl;

  /* How do we determine if there is an uncaught exception, and if so, access
   * it?
   *
   * Before C++11, no standard way. But C++11 exposed a `current_exception`
   * variable to access.
   */
  std::exception_ptr pexc = std::current_exception();

  if (pexc) {
    try {
      std::rethrow_exception(pexc);
    } catch (const std::system_error &e) {
      std::cerr << "Caught std::system_error!" << std::endl;
      std::cerr << " - what: " << e.what() << std::endl;
      std::cerr << " - code.value: " << e.code().value() << std::endl;
      std::cerr << " - code.message: " << e.code().message() << std::endl;
      std::cerr << " - code.category.name: " << e.code().category().name() << std::endl;
    } catch (const std::runtime_error &e) {
      std::cerr << "Caught std::runtime_error: " << e.what() << std::endl;
    } catch (const std::exception &e) {
      std::cerr << "Caught std::exception: " << e.what() << std::endl;
    } catch (int e) {
      std::cerr << "Caught int exception: " << e << std::endl;
    } catch (char e) {
      std::cerr << "Caught char exception: " << e << std::endl;
    } catch (...) {
      std::cerr << "Caught unknown exception" << std::endl;
    }
  } else {
    std::cerr << "Caught explicit terminate call" << std::endl;
  }

  std::exit(EXIT_SUCCESS);
}
