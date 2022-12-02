# ATS2 Project Template

GNUMakefile for ATS2-Postiats

## Usage Information

Place this Makefile at the root directory of your ATS project. After running commands `make`, `make all` or `make run`, a new directory `build` containing the project executable and intermediate `.c` and `.o` files will be automatically created.

Example (project: helloworld):

```text
\build
   \bin (project executable)
      helloworld
   \obj (intermediate .o files)
      main_dats.o
   \src (intermediate .c files)
      main_dats.c
\src
   \dats (project .dats files)
      main.dats
   \sats (project .sats files)
Makefile (this file)
```

The following commands are available:

- `make all`  : build the project
- `make run`  : build then run the project executable
- `make clean`: cleanup project
