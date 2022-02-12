# Memory Analysis Tool

[![Build Status](https://img.shields.io/jenkins/s/http/jenkins.vacs.fr/Mat.svg)](http://jenkins.vacs.fr/job/Mat/)
[![Download](https://img.shields.io/badge/download-1.0.0-brightgreen.svg)](http://download.vacs.fr/mat/mat-1.0.0.tar.gz)
[![License](http://img.shields.io/badge/license-APACHE2-blue.svg)](LICENSE)
![Commits](https://img.shields.io/github/commits-since/stcarrez/mat/1.0.0.svg)

MAT is a simple memory analysis tool intended to help
understand where the memory is used in a program.

The tool works by using a small shared library that
can be loaded by using the `LD_PRELOAD` dynamic linker option.
The shared library collects memory allocation events and
generates an event file that can be analyzed by MAT tool.

From the event file, the tool is able to provide useful
information.  This includes the details about the memory
allocation (size, address), the complete stack frame where the
memory allocation was made, the timestamp and thread information.

MAT was created to answer these simple questions:

* where is my memory used?
* what is the size of data structures and their impact on the global memory?
* what function or area of my program is using the most memory?

It is similar to the [Massif heap profiler](https://valgrind.org/docs/manual/ms-manual.html)
provided by a [Valgrind](https://valgrind.org/) plugin.

A first version of MAT existed back in 1994 but it was written in C++ when
[Valgrind](https://valgrind.org/) was not available.

## Version 1.2     - Under development

- Update the build process
- Update for Ada Utility Library 2.5.0 and Ada BFD 1.2.1

## Version 1.1     - Apr 2021

- Update for binutils 2.34 and Ada BFD
- Update for Ada Utility Library 2.4.0
- Fix monitoring of `calloc` with glibc
- Add colors in mat analysis

# Using git

The project uses git submodules to integrate several other
projects.  To get all the sources, use the following commands:
```
git clone --recursive git@github.com:stcarrez/mat.git
cd mat
```

# Building mat

The package is composed of three separate components:

- `libmat` is the shared library that instruments the memory allocation.
- `matl` is a small launcher that helps instrument a program with libmat.
- `mat` is the analysis tool.

`libmat` and `matl` are written in C and `mat` is written in Ada which requires
the GNAT Ada compiler.  By default the `mat` component is not built.
If you only need `libmat` and `matl`, configure and build as follows:

```
./configure
make
```

To build a 32-bit or 64-bit version of the shared library you may use:

```
  CC="/usr/bin/gcc -m32" ./configure --disable-mat
  make
```
or

```
  CC="/usr/bin/gcc -m64" ./configure --disable-mat
  make
```

If you're using a cross compilation environment, you should
indicate to the configure your target host.  For example to
build for a remote mips system, use:

```
  ./configure --host="mips-uclibc-linux" --target=mips-uclibc-linux --disable-mat
  make
```

To build the `mat` analysis tool, you must have installed the following
components on your system:

- the GNAT Ada compiler (at least 4.7.3 or higher),
- the GNU Ada Bfd library (https://github.com/stcarrez/ada-bfd.git),
- the Ada Utility Library (https://github.com/stcarrez/ada-util.git)

On Debian-based systems, you may have to install the following packages:

  sudo apt-get install gnat gprbuild binutils-dev libiberty-dev libreadline-dev

If you have not installed Ada Utility Library and Ada Bfd Library on your
system, you can configure and build by using:

```
  ./configure --enable-ada-util --enable-ada-bfd
  make
```

# Instrumenting your application

You can instrument your application passively by recording all events and looking
at the memory allocation after the program has stopped.  It is also possible to
instrument dynamically while the application is running.  Both methods have
they advantages.

## Passive instrumentation

You can instrument the memory allocation by using the matl launcher.

```
  matl -o name my-program
```

While the program runs and the libmat.so collect events,
it generates a file 'name-<pid>.mat'.

Start mat with the generated file:

```
  mat name-xxx.mat
```

Once the memory events are loaded, you can use the interactive
commands to look at the events.  The first commands you may use
are 'info', 'timeline' and 'sizes' as they give a short summary
and analysis of the events.


## Dynamic instrumentation

The dynamic instrumentation requires that the `mat` analyser is started in
the server mode: it is started first, before the application to analyse.
The server is activated by the `-s` option.  It listens to the TCP/IP port 4606
and then enter in the interactive mode:

```
mat -s
```

Then, you can launch your application through the same `matl` launcher
but you will specify the host name to connect:

```
  matl -s localhost my-program
```


# Embedded systems

On embedded systems, you only need to build the libmat.so and matl parts.
For Mips and ARM, for the backtrace to work, you should compile your program
with the -funwind-tables gcc option.  Instrument your program and copy
the generated .mat files on your Linux host.  Make sure your program is
not stripped and available to the mat program to get the symbols
(use the -s path option if necessary).

# Documentation

* [Memory Analysis Tool (1)](https://github.com/stcarrez/mat/blob/master/docs/mat.md)
* [Memory Analysis Tool Launcher (1)](https://github.com/stcarrez/mat/blob/master/docs/matl.md)

# Articles

* [Using MAT the Memory Analysis Tool](https://blog.vacs.fr/vacs/blogs/post.html?post=2015/05/15/Using-MAT-the-Memory-Analysis-Tool)
