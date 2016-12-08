VER = 0.1.0
FC = gfortran

PREFIX ?= /usr/local
SRCDIR = src
LIBDIR = lib
INCDIR = include
TESTDIR = test

MODFLAG = -J
FCFLAGS ?= $(MODFLAG)$(INCDIR) -g -Wall -DUSE_DOUBLE
LDFLAGS ?= -L$(HOME)/local/xdrfile/lib
LIBS ?= -lxdrfile

LIBNAME = trjfort
LIB_TRJFORT := $(addsuffix .a, $(addprefix lib, $(LIBNAME)))
LIB_TRJFORT := $(addprefix $(LIBDIR)/, $(LIB_TRJFORT))

OBJS = varpars.o xyz.o xdr.o trjfort.o 
OBJS := $(addprefix $(SRCDIR)/, $(OBJS))

TEST1 = test1.out
TESTBINS = $(TEST1)
TESTBINS := $(addprefix $(TESTDIR)/, $(TESTBINS))
TESTOBJS = test.o
TESTOBJS := $(addprefix $(TESTDIR)/, $(TESTOBJS))

.PHONY: all build test install clean

all: build test

# =========================
build: $(LIB_TRJFORT) 

$(LIB_TRJFORT): $(OBJS)
	ar cr $@ $^

$(OBJS): | $(LIBDIR) $(INCDIR)
 
$(LIBDIR):
	mkdir -p $(LIBDIR)

$(INCDIR):
	mkdir -p $(INCDIR)

$(SRCDIR)/trjfort.o: $(addprefix $(SRCDIR)/, varpars.o xyz.o xdr.o)

$(SRCDIR)/xyz.o: $(SRCDIR)/varpars.o

$(SRCDIR)/xdr.o: $(SRCDIR)/varpars.o

# =========================
test: $(TESTBINS)
	cd $(TESTDIR) && ./$(TEST1)

$(TESTDIR)/$(TEST1): $(LIB_TRJFORT) $(TESTOBJS)
	$(FC) $(FCFLAGS) $^ -o $@ -L$(LIBDIR) -l$(LIBNAME) $(LDFLAGS) $(LIBS)

# =========================
%.o : %.F90
	$(FC) -c $(FCFLAGS) $< -o $@

# =========================
clean:
	rm -rf $(INCDIR) $(LIBDIR) $(OBJS) $(SRCDIR)/*.mod
	rm -rf $(TESTBINS) $(TESTOBJS) $(TESTDIR)/*.mod $(TESTDIR)/*.xyz $(TESTDIR)/*.trr

