#ifndef TYPE
   TYPE=distrib
#endif
#ifndef COMPILER
   COMPILER=ifx
#endif

CHUNK = -DCHUNK

NCF_INST = /usr/local/netcdf
HDF_INST = /usr/local/hdf5
MOD_DIR = ../Mod_src

INC = -I. -I../inc -I$(MOD_DIR) -I$(NCF_DIR) -I$(NCF_INST)/include

.SUFFIXES: .c .f .o .f90 .F90

ifneq (, $(findstring $(COMPILER),pgf90))
   FC = pgf90
   FLGS = -Mbounds -C -g -Mextend -O2 -Mlfs -Mdclchk -Mnoframe -byteswapio $(INC) -mcmodel=medium
   FLGS2 =  -Mbounds -C -g -Mextend -O2 -Mlfs -Mnoframe -byteswapio $(INC) -mcmodel=medium
   MODULES=-I${MOD_DIR} -module ${MOD_DIR}
endif

ifneq (, $(findstring $(COMPILER),ifx))
   FC = ifx
   FLGS =  -O2 -convert big_endian -extend-source -mcmodel=medium $(INC)
   FLGS2 =  -O2 -convert big_endian -extend-source -mcmodel=medium $(INC)
   MODULES=-I${MOD_DIR}
endif

ifneq (, $(findstring $(COMPILER),gfortran))
   FC = gfortran
   FLGS =  -O2 -fconvert=big-endian -frecord-marker=4 -ffixed-line-length-0 -mcmodel=medium $(INC)
   FLGS2 =  -O2 -fconvert=big-endian -frecord-marker=4 -ffixed-line-length-0 -mcmodel=medium $(INC)
   MODULES=-M${MOD_DIR}
endif

MOD_DIR = ../Mod_src
NCF_DIR = ../IO_NCF

LIBS = \
../lib/eps3lib.$(TYPE).a \
-L$(NCF_INST)/lib -lnetcdff -lnetcdf

####LIBS = \
####../lib/eps3lib.$(TYPE).a \
####-L$(NCF_INST)/lib -lnetcdff -lnetcdf \
####-L$(HDF_INST)/lib -lhdf5_hl -lhdf5 -lhdf5_fortran -lhdf5hl_fortran -lz

MOD_OBJ = \
$(MOD_DIR)/eps3ncf_incs.o

TARGT = pts2ems_ncf.$(TYPE)

OBJCTS = \
ncf_xfer_global.o \
ncf_xfer_data_tstep.o \
opnpts2ems.o \
prcpts2ems.o \
pts2ems_ncf.o \
rdupts2ems.o \
totpts2ems.o \
uam2ncf_global.o \
wtlpts2ems.o \
$(NCF_DIR)/ncf_chkfile.o \
$(NCF_DIR)/ncf_createfile.o \
$(NCF_DIR)/ncf_set_compress_flag.o \
$(NCF_DIR)/ncf_set_tstep.o \
$(NCF_DIR)/ncf_wrt_data_tstep.o \
$(NCF_DIR)/ncf_set_specatt_ems.o \
$(NCF_DIR)/ncf_set_vars_base.o \
$(NCF_DIR)/ncf_wrt_data_grid.o \
$(NCF_DIR)/ncf_wrt_dim.o \
$(NCF_DIR)/ncf_wrt_global.o \
$(NCF_DIR)/ncf_wrt_vars_base.o \
$(NCF_DIR)/ncf_wrt_vars_species.o

$(TARGT): $(MOD_OBJ) $(OBJCTS)
	$(FC) -o $(TARGT) $(FLGS) $(MOD_OBJ) $(OBJCTS) $(LIBS)
.f.o	:
	$(FC) -c -o $@ $(FLGS) $(MODULES) $<
.F.o	:
	$(FC) -c -o $@ $(FLGS) $(MODULES) $<
.F90.o	:
	$(FC) -c -o $@ $(CHUNK) $(FLGS) $(MODULES) $<

clean:
	touch jnk.o
	rm -vf *.o $(NCF_DIR)/*.o $(MOD_DIR)/*.mod $(MOD_DIR)/*.o $(TARGT)
