VER = 0.1.0
FC = gfortran

PREFIX ?= /usr/local
SRCDIR = src
LIBDIR = lib
INCDIR = include
TESTDIR = test

MODFLAG = -J
FCFLAGS ?= $(MODFLAG)$(INCDIR) -fdefault-real-8 -g -Wall

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

trjfort.o: varpars.o xyz.o xdr.o

xyz.o: varpars.o

xdr.o: varpars.o

# =========================
test: $(LIB_TRJFORT) $(TESTBINS)
	cd $(TESTDIR) && ./$(TEST1)

$(TESTDIR)/$(TEST1): $(TESTOBJS)
	$(FC) $(FCFLAGS) $^ -o $@ -L$(LIBDIR) -l$(LIBNAME)

# =========================
%.o : %.F90
	$(FC) -c $(FCFLAGS) $< -o $@

# =========================
clean:
	rm -rf $(INCDIR) $(LIBDIR) $(OBJS) $(SRCDIR)/*.mod
	rm -rf $(TESTBINS) $(TESTOBJS) $(TESTDIR)/*.mod

