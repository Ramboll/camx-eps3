#ifndef TYPE
   TYPE=distrib
#endif
#ifndef COMPILER
   COMPILER=ifx
#endif

INC = -I../inc

.SUFFIXES: .c .f .o .f90 .F90

ifneq (, $(findstring $(COMPILER),pgf90))
   FC = pgf90
   FLGS = -Mextend -O2 -Mlfs -Mdclchk -Mnoframe -byteswapio $(INC) -mcmodel=medium
   FLGS2 =  -Mextend -O2 -Mlfs -Mnoframe -byteswapio $(INC) -mcmodel=medium
endif

ifneq (, $(findstring $(COMPILER),ifx))
   FC = ifx
   FLGS =  -O2 -convert big_endian -extend-source -mcmodel=medium $(INC)
   FLGS2 =  -O2 -convert big_endian -extend-source -mcmodel=medium $(INC)
endif

ifneq (, $(findstring $(COMPILER),gfortran))
   FC = gfortran
   FLGS =  -O2 -fconvert=big-endian -frecord-marker=4 -ffixed-line-length-0 -mcmodel=medium $(INC)
   FLGS2 =  -O2 -fconvert=big-endian -frecord-marker=4 -ffixed-line-length-0 -mcmodel=medium $(INC)
endif

LIBS = \
../lib/eps3lib.$(TYPE).a  


MOD_OBJ = \
$(MOD_DIR)/eps3ncf_incs.o

TARGT = tmpfac.$(TYPE)

OBJCTS = \
clsfac.o \
fndfxf.o \
opnfac.o \
prcafs.o \
rdfprf.o \
rdfxrf.o \
setday.o \
setfxf.o \
setmon.o \
setwek.o \
tmpfac.o \
wrfprf.o \
wrfxrf.o

$(TARGT): $(OBJCTS) $(LIBS)
	$(FC) -o $(TARGT) $(FLGS) $(OBJCTS) $(LIBS)
.f.o	:
	$(FC) -c -o $@ $(FLGS) $<

clean:
	touch jnk.o
	rm -vf *.o $(TARGT)

