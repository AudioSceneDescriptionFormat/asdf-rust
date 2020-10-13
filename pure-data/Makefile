lib.name = asdf

class.sources = asdf~.c

# all extra files to be included in binary distribution of the library
datafiles =

PKG_CONFIG ?= pkg-config

CPPFLAGS = `$(PKG_CONFIG) --cflags asdf`
ldlibs = `$(PKG_CONFIG) --libs asdf`

include pd-lib-builder/Makefile.pdlibbuilder
