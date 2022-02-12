
## NAME

mat - Memory Analysis Tool

## SYNOPSIS

*mat* [ -i ] [-d path] [-s] [ -b
_ip[:port]_ ] [ -ns ] [
_mat-event-file_ ]


## DESCRIPTION

_mat_ is a memory analysis tool intended to help understand what areas of a program
use the most memory.  The tool works with a small shared library
_libmat.so_ that must be loaded by the program for the analysis. The shared library catches a number of
memory allocation operations such as
*malloc*, *realloc* and
*free* and informs
_mat_ about the memory allocation.  It then writes or sends probe events which contain
enough information for
_mat_ to tell what, when, where and by whom the memory allocation was done.

The memory analysis can be done when the program has finished or while the program is still running
or debugged through
*gdb*(1). In the first case, the
_libmat.so_ library will have to write the information to a file that is later read by
_mat_ for analysis.  In the second case, the
_libmat.so_ library will send the information to the
_mat_ server through a TCP/IP connection.  The
_mat_ server and the
_libmat.so_ client can run on different hosts and they can use different architectures.

The
_mat_ tool offers a number of commands to analyze the memory used.  The first commands you may use
are the
*info* and
*timeline* commands that give a summary of events and give you some entry point to find out more.
The
*sizes* and
*frames* commands are then interesting to give an indication about the allocation sizes and their location.
To go deeper in the analysis, you will use the
*events* and
*event* command to get a more detailed description of the allocation event with the complete stack frame.

## OPTIONS

The following options are recognized by _mat_:


*-i* Enable the interactive mode.


*-d [path]* Define one or several search paths to load symbols for the shared libraries.


*-s* Start the TCP/IP server to receive the events.


*-b [ip:]port* 
Define the TCP/IP port and local address to bind.  By default the
_mat_ server binds to any address and uses the TCP/IP port 4606.


*-ns* 
Disable the automatic symbols loading.

## SETUP

Before starting the program to analyze, two environment variables must be set.

The first environment variable to set is
_LD_PRELOAD_ which tells the linker to force the loading of a shared library at the startup of
a program.  This variable should indicate the absolute path where the
_libmat.so_ shared library is installed on your system.

```
export LD_PRELOAD=/usr/lib/libmat.so
```

The second environment variable is used by the
_libmat.so_ shared library to configure the destination of probe events that the library generates.
The probe events can be written to a file that is later read by
_mat_ for analysis.  The
_MAT_SERVER_ environment variable indicates the path of the file where the probe events will be written
during execution.

```
export MAT_SERVER=file://my-record
```

The probe events can be redirected to a TCP/IP connection that
_mat_ is listening.  In that case, the
_MAT_SERVER_ environment variable indicates the TCP/IP host and port.  The
_mat_ tool must be started before the program under analysis.

```
export MAT_SERVER=tcp://192.168.0.12:4606
```

To help in setting up these environment variables, you can use the
_matl_(1) launcher that can either monitor through a file or through the TCP/IP connection.
If you want to save the probe events to a file, use the following command:

```
matl -o my-record prog args
```

and if you want to send the probe events while your program is running use:

```
matl -s 192.168.0.12:4606 prog args
```

## EXPRESSIONS

Several commands provided by
_mat_ allow to specify a filter to select the events to take into account.
First the event type is filtered easily by using one of the event types
*malloc*, *realloc*, or
*free*. The memory allocation can further be filtered by the size or the address.
You can use logical operators such as
*and*, *or*, and
*not* to create complex selection.


### Filtering on the event type


malloc
Report only
*malloc* events.


realloc
Report only
*realloc* events.


free
Report only
*free* events.


nofree
Report memory allocation events that don't have an associated
_free_ event.  The alias
*leak* can also be used (although this is not a memory leak detection).


### Filtering on the event ID
When
_mat_ reads events, it allocates a unique increasing number for each event.
Several
_mat_ commands report the event ID or a range of event IDs so that you can
easily create filters on them.


event < _number_


event <= _number_
Report events whose ID is less than the given number.


event > _number_


event >= _number_
Report events whose ID is greater than the given number.


event = _number_
Report events whose ID is equal to the given number.


_low_ .. _high_


event = _low_ .. _high_
Report events whose ID is in the range range [_low_ .. _high_].


### Filtering on the address


has _address_
Report events whose associated memory slot contains the given address.  This filter is useful
if you want to know the events that allocated or freed some memory address.  The address may
be within the memory slot (it does not need to be the exact malloc or free address).


addr < _address_


addr <= _address_
Report events whose allocation address is less than the given address.  The address being
checked is the address returned by
_malloc_ or
_realloc_ or the address given to the
_free_ function.


addr > _address_


addr >= _address_
Report events whose allocation address is greater than the given address.


addr = _address_
Report events whose address is equal to the given address.


### Filtering on the size


size < _number_


size <= _number_
Report events whose size is less than the given number.


size > _number_


size >= _number_
Report events whose size is greater than the given number.


size = _number_
Report events whose size is equal to the given number.


size = _low_ .. _high_
Report events whose size is in the range range [_low_ .. _high_].


### Filtering on the time
Each event recorded by
_libmat.so_ is associated with a timestamp that was obtained with
_gettimeofday_(2). _mat_ will use relative time from the program start so that you know when some event occured in time,
relative to the start.  The relative time is always printed and defined in seconds with fractional
digits (such as _3.14_).


after _time_
Report only events that occured after the relative time.


before _time_
Report only events that occured before the relative time.


from _time_ to _time_
Report only events that occured within the given time range.


### Filtering on the stack frame
For each event,
_mat_ knows the stack frame and thread that triggered the event.
You can filter events by looking at the stack frame and keep only the events
in the function or area of code you are interested in.


by _symbol_


by direct _symbol_
Report only events whose stack frame contains calls to the given symbol.  When the
*direct* keyword is added, only the stack frame at level 1 is looked, which means
that the function must directly call one of the
_malloc_, _realloc_ or
_free_ operations.  The
*by* filter uses the symbol table and debugging information so that it is necessary
to build your program with
_-g_. 

in _name_


in direct _name_
The
*in* filter is similar to the
*by* filter but it uses the name of a memory region detected by
_mat_. The name of the memory region is either the program name or the name of a shared library.
Such filter is useful if you want to exclude or take into account all the memory allocation
made by a shared library.

## COMMANDS

The interactive mode of
_mat_ uses the
_readline_(3) library to read interactive commands so that you benefit from all the features provided by
the line editor and its command history.  The
following commands are provided:


addr _addr_
The
*addr* command gives information about an address and possible events related to it.


event _id_
The
*event* command prints the full description of an event with the complete stack frame.


events _[-c] [-l]_ _[filter]_
The
*events* command lists the events which are matched by the filter.
The
*-c* option prints only the number of events matched by the filter.
The
*-l* option prints a more detailed description of events.
For each event, it indicates
the event nnumber, the relative time, the event type (
_malloc_, _realloc_, _free_), the event size and event related addresses.  The event stack frame is not printed.
This command is useful to identify the interesting events that have been collected.
Having the event number, the
_event_ command is then used to get more information.


exit
The
*exit* command terminates the analysis.


frames level _[-c] [-l]_ _[filter]_
The
*frames* command reports the functions that have made a memory allocation directly or indirectly.
The
_level_ argument indicates the stack frame level to report and the
_filter_ is the optional expression to filter out the events that must be taken into account.
Use the
*frames* command to know the places in the program where memory allocations are done.


help
The
*help* command reports a help description with available commands.


info
The
*info* command gives a short summary description about the program and the events that were collected.
This includes the number of events, the number of
_malloc_, _realloc_, _free_ calls and number of allocated memory slots.


maps
The
*maps* command prints the memory regions that were identified by
_libmat.so_. This command is useful to know the shared libraries that have been loaded by the program.
For each region, it indicates the address ranges, the access mode of the region (
_rwx_ flags), and the path of the shared library.  The memory regions are used internally by
_mat_ to load the symbols from the shared libraries and resolv their symbol names.


open mat-file
The
*open* command loads the file generated by
_libmat.so_ 

sizes _[-c] [-l]_ _[filter]_
The
*sizes* command analyzes the events identified by the filter and reports the different sizes grouped by
event type (
_malloc_ or
_realloc_). For each allocation size, it indicates the number of allocation made, the size of the allocation
and the first and last event number.  This commands helps in looking at the allocations sizes
that are the most used by the program.  The event number range reported can be used to further
print the detailed event information with the
_event_ command.


slots _[-c] [-l]_ _[filter]_
The
*slots* command reports the memory slots that are allocated and that match the filter expression.
This command works on the knowledge that
_mat_ has about the used memory slots.  For each memory slot it indicates the addres, size and the
event that made the allocation.  Then, you may use the
*event* command to know more about the allocation.


symbol path
The
*symbol* command loads the symbol information from the given path.  The program should be compiled
with
*-g* to obtain information about source file and line number.


timeline _[duration]_
The
*timeline* command analyses the events to find interesting groups and report information about them.
The
_duration_ parameter controls the grouping by defining the maximum duration in seconds of a group.
For each group, the command indicates the event ID range, the number of
*malloc*, *realloc* and
*free* calls as well as the memory growths or shrink during the period.


threads
The
*threads* command reports the list of threads with the memory that they allocated.

## BUGS

When using the file generation, the
_libmat.so_ can produce very large files quickly when the program performs many allocation (expect several GB in some cases).

The
_libmat.so_ does not detect shared libraries which are loaded by
_dlopen_(3) after the program is started.  The symbol table of these shared libraries are not loaded but
_libmat.so_ will still report all the memory allocations.

## EXAMPLE

The example below illustrates some commands.
```
$ mat cli-1233.mat
matp>timeline 20
Start     End time  Duration  Event range         # malloc  # realloc # free    Memory
0us       19.83s    19.83s    0..11590            5685      120       5778      +8327
28.71s    48.53s    19.82s    11591..11669        38        3         38        -76
48.87s    67.96s    19.09s    11670..17890        3111      1         3109      +18
69.00s    88.95s    19.95s    17891..18273        193       0         190       +71
matp>events 11591..11669 and leak
Previous  Id        Next      Time      Event
           11605              31.89s    malloc(15) = 0x00455B18
           11616              36.80s    malloc(11) = 0x00455FC8
           11633              43.42s    malloc(18) = 0x00456018
matp>event 11605
15 bytes allocated (never freed)
Id Frame Address         Function
 1 0x00407590            __start
 2 0x2AC4989C            __start (libc.so.0)
 3 0x00409FE4            main (main.c:979)
 4 0x0040B2AC            main_loop (main.c:377)
 5 0x00428ED8            event_process_active (event.c:1667)
 6 0x004284D4            event_process_active_single_queue (event.c:1575)
 7 0x00420C04            bufferevent_trigger_nolock_ (bufferevent-internal.h:366)
```

## SEE ALSO

_gcc(1)_, _gdb(1)_, _ld.so(8)_, _matl(1)_, _valgrind(1)_, _dlopen(3)_, _readline(3)_

## AUTHOR

Written by Stephane Carrez.
