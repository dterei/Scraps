#!/usr/bin/m4
dnl deletes the new-line after the macro definition
define(AUTHOR, William Shakespeare)dnl
dnl it also servers as a comment that isn't echoed
# The other comment type is `#', which is echoed, but isn't interpreted by m4
# so can contain macro names, E.g., AUTHOR here is not expanded.
A Midsummer Night's Dream
by AUTHOR
