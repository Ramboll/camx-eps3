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
   FLGS =  -Mextend -O2 -Mlfs -Mdclchk -Mnoframe -byteswapio $(INC) -mcmodel=medium
   FLGS2 =  -Mextend -O2 -Mlfs -Mnoframe -byteswapio $(INC) -mcmodel=medium
endif

ifneq (, $(findstring $(COMPILER),ifx))
   FC = ifx
   FLGS =  -O2 -convert big_endian -extend-source -mcmodel=medium $(INC)
   FLGS2 =  -O2 -convert big_endian -extend-source -mcmodel=medium $(INC)
   MODULES=-I${MOD_DIR} -module ${MOD_DIR}
endif

ifneq (, $(findstring $(COMPILER),gfortran))
   FC = gfortran
   FLGS =  -O2 -fconvert=big-endian -frecord-marker=4 -ffixed-line-length-0 -mcmodel=medium $(INC)
   FLGS2 =  -O2 -fconvert=big-endian -frecord-marker=4 -ffixed-line-length-0 -mcmodel=medium $(INC)
   MODULES=-M${MOD_DIR}
endif

TARGT = eps3lib.$(TYPE).a

OBJCTS = \
addtotasc.o \
addtotfcd.o \
addtotcty.o \
addtothrs.o \
addtothcd.o \
addtotpid.o \
caljul.o \
chrsrt.o \
filhdr.o \
fndchr.o \
fndhash_asc.o \
fndhash_fips.o \
fndfip.o \
fndint.o \
fndkey.o \
fndpar.o \
fndxrf.o \
fndxasc.o \
getime.o \
getime_camx.o \
gline.o \
grdprep.o \
init_hash_msgtot.o \
julcal.o \
juldate.o \
lcpgeo.o \
lftjst.o \
low2up.o \
mapgtu.o \
maputg.o \
mkhash_asc.o \
mkhash_pair.o \
mkhash_fips.o \
mrcgeo.o \
msgcsv.o \
msgtbl.o \
pspgeo.o \
rdbins.o \
rddfspl.o \
rdcntl.o \
rdcnty.o \
rdcrit.o \
rddate.o \
rdembr.o \
rdemar.o \
rdhdr.o \
rdlayers.o \
rdotbl.o \
rdregn.o \
rdspec.o \
rdtral.o \
rdxref.o \
rgtjst.o \
rpsgeo.o \
setij.o \
skphdr.o \
strlen.o \
strmin.o \
token.o \
uptime.o \
utmgeo.o \
wrctbl.o \
wrctbl8.o \
wrtasc.o \
wrtcty.o \
wrtfcd.o \
wrthrs.o \
wrthcd.o \
wrtfac.o \
wrtstak.o \
wrtpnt.o \
wremar.o \
wrembr.o \
wrhdr.o \
writbl.o 	

$(TARGT): $(OBJCTS)
	ar -r $(TARGT) $(OBJCTS)
.f.o	:
	$(FC) -c -o $@ $(FLGS) $<
.f90.o  :
	$(FC) -c -o $@ $(FLGS) $(MODULES) $<
rpsgeo.o: rpsgeo.f
	$(FC) -c -o $@ $(FLGS2) $(MODULES) $<
utmgeo.o: utmgeo.f
	$(FC) -c -o $@ $(FLGS2) $(MODULES) $<

clean:
	touch jnk.o
	rm -vf *.o $(TARGT)

