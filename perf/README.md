# Linux Tracing

Linux has a lot of tracing options now.

## Technology

Linux Kernel:
* Tracepoints -- static kernel tracepoints
* KProbes -- dynamic kernel tracepoints
* UProbes -- dynamic user-level tracepoints

GCC: 
* `-finstrument-functions` -- Instrument all function entries and
  exits with a call to a function the user can define.
  https://gcc.gnu.org/onlinedocs/gcc-4.9.1/gcc/Code-Gen-Options.html

## Tools

Kernel:
* ftrace -- see /sys/kernel/debug/tracing
* perf
* LTTng -- support all kernel + `-finstrument-functions`.
* SystemTap
* KTag -- supports an interpreter inside kernel for more expressive
  tracing

Visual Tools:
* Trace Compass
* KernelShark

