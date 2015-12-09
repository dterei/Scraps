/**
 * Playing with ways to exit (normally and abnormally a C++ program).
 *
 * C offers 'exit' and 'abort'.
 *
 * C++ offers 'exit', 'quick_exit', '_Exit', 'abort', 'terminate' and throwing
 * an uncaught exception. Generally, for abnormal exit should throw an
 * exception, catch it in main and rethrow or log and exit.
 */
#include <cstdlib>
#include <exception>
#include <iostream>
#include <stdexcept>

void on_normal_exit(void)
{
  std::cout << "on_normal_exit..." << std::endl;
}

[[noreturn]]
void normal_exit(void)
{
  /* atexit registered functions called */
  atexit(on_normal_exit);

  /* exit for normal termination -- can have an non-zero error code, but should
   * be due to an expected reason (e.g., corruport configuration file, bad
   * arguments...)
   */
  std::exit(EXIT_SUCCESS);
}

[[noreturn]]
void quick_normal_exit(void)
{
  /* NOT called */
  atexit(on_normal_exit);

  /* Called */
  at_quick_exit(on_normal_exit);

  /* exit for normal termination -- main difference is we don't execute static
   * destructors, which is valuable in a multi-threaded environment where we
   * can't easily ensure all threads exit cleanly */
  std::quick_exit(EXIT_SUCCESS);
}

[[noreturn]]
void unsafe_normal_exit(void)
{
  /* NOT called */
  atexit(on_normal_exit);

  /* NOT called */
  at_quick_exit(on_normal_exit);

  /* just exit as quick as possible, no deconstructors or flushing IO */
  std::_Exit(EXIT_SUCCESS);
}

[[noreturn]]
void abnormal_exit(void)
{
  /* atexit registered functions ARENT called */
  atexit(on_normal_exit);

  /* exit for abnormal termination -- i.e., a bug, a failed invariant,
   * raises the SIGABRT signal, which if uncaught terminates the program
   */
  std::abort();
}

void my_terminate(void)
{
  std::cerr << "terminate handler called" << std::endl;
  std::abort();
}

[[noreturn]]
void abnormal_terminate(void)
{
  /* C++ interface to C std::abort(), by default terminate is just a wrapper
   * around abort, but it allows a termination handler to be installed that can
   * respond to the call.
   *
   * std::terminate (and so the termination handler) is also what is called for
   * uncaught exceptions.
   */
  std::terminate();
}

int main(void)
{
  /* Custom terminate handler (just does default) */
  std::set_terminate(my_terminate);

  // normal_exit();
  // quick_normal_exit();
  // unsafe_normal_exit();
  // abnormal_exit();
  abnormal_terminate();

  // throw std::runtime_error("error!");
}
