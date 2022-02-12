
## NAME

matl - Memory Analysis Tool Launcher

## SYNOPSIS

*matl* [ -o
_name_ ] [ -s
_host[:port]_ ]
_command_ 

## DESCRIPTION

_matl_ is a simple memory analysis tool launcher to help in instrumenting
the program memory by using the
*mat*(1) application.  The launcher sets the
_LD_PRELOAD_ environment variable to load the
*libmat.so* shared library in the program.  This library will instrument the memory allocation.
The launcher also defines the
_MAT_SERVER_ environment variables which is used by
*libmat.so* to control and activate the memory analysis in the program.

When the environment variables are set, _matl_ launches the program with its arguments.

The information collected by
*libmat.so* can be saved to a file or directed to the
*mat*(1) server.  In that case, the server must be started first.

## OPTIONS

The following options are recognized by _matl_:


*-o name* Instrument the program and write the results in a file.  The generated file name
will contain the process ID and will use the form *name*-_pid_.mat


*-s host[:port]* 
Instrument the program by connecting to the
_mat_ TCP/IP server.  The host and optional port defines the host name or IP address
of the host where the
_mat_ server is running.  The default port is the port 4606.  The
_mat_ server must be started first.

## SEE ALSO

_mat(1)_, _ld.so(8)_

## AUTHOR

Written by Stephane Carrez.
