#ifndef TYPE
TYPE=distrib
#endef

#ifndef COMPILER
COMPILER=ifx
#endef

#
#   --- This is the instruction to make the all of the programs ---
#

all:
	@echo $(TYPE)
	@rm -vf inc
	@ln -s inc.$(TYPE) inc
	cd lib; make -f Makefile COMPILER=$(COMPILER) TYPE=$(TYPE)
	cd grarea; make -f Makefile
	cd chmspl; make -f Makefile
	cd cntlem; make -f Makefile
	cd embret; make -f Makefile
	cd emscvt; make -f Makefile
	cd grdem; make -f Makefile
	cd lbase; make -f Makefile
	cd mrguam; make -f Makefile
	cd pigems; make -f Makefile
	cd pream; make -f Makefile
	cd prefir; make -f Makefile
	cd preshp; make -f Makefile
	cd prepnt; make -f Makefile
	cd pstfir; make -f Makefile
	cd pstshp; make -f Makefile
	cd pstpnt; make -f Makefile
	cd pts2ems_ncf; make -f Makefile
	cd spcems; make -f Makefile
	cd spltem; make -f Makefile
	cd tmpfac; make -f Makefile
	cd tmprl; make -f Makefile

#
#   --- This is the instruction to clean the all of the programs ---
#


clean:
	@echo $(TYPE)
	cd lib; make -f Makefile clean
	cd grarea; make -f Makefile clean
	cd chmspl; make -f Makefile clean
	cd cntlem; make -f Makefile clean
	cd embret; make -f Makefile clean
	cd emscvt; make -f Makefile clean
	cd grdem; make -f Makefile clean
	cd lbase; make -f Makefile clean
	cd mrguam; make -f Makefile clean
	cd pigems; make -f Makefile clean
	cd pream; make -f Makefile clean
	cd prefir; make -f Makefile clean
	cd preshp; make -f Makefile clean
	cd prepnt; make -f Makefile clean
	cd pstfir; make -f Makefile clean
	cd pstshp; make -f Makefile clean
	cd pstpnt; make -f Makefile clean
	cd pts2ems_ncf; make -f Makefile clean
	cd spcems; make -f Makefile clean
	cd spltem; make -f Makefile clean
	cd tmpfac; make -f Makefile clean
	cd tmprl; make -f Makefile clean
#
#  --- The following are for the individual programs ----
#
#
#  --- LIB ----
#
build_lib:
	@echo $(TYPE)
	@rm -vf inc
	@ln -s inc.$(TYPE) inc
	cd lib ; make -f Makefile TYPE=$(TYPE)

clean_lib:
#
#  --- GRAREA (library) ----
#
build_grarea:
	@echo $(TYPE)
	@rm -vf inc
	@ln -s inc.$(TYPE) inc
	cd grarea ; make -f Makefile TYPE=$(TYPE)

clean_grarea:
	cd grarea ; make -f Makefile clean
#
#  --- CHMSPL ----
#
build_chmspl:
	@echo $(TYPE)
	@rm -vf inc
	@ln -s inc.$(TYPE) inc
	cd chmspl ; make -f Makefile TYPE=$(TYPE)

clean_chmspl:
	cd chmspl ; make -f Makefile clean
#
#  --- CNTLEM ----
#
build_cntlem:
	@echo $(TYPE)
	@rm -vf inc
	@ln -s inc.$(TYPE) inc
	cd cntlem ; make -f Makefile TYPE=$(TYPE)

clean_cntlem:
	cd cntlem ; make -f Makefile clean
#
#  --- EMBRET ----
#
build_embret:
	@echo $(TYPE)
	@rm -vf inc
	@ln -s inc.$(TYPE) inc
	cd embret ; make -f Makefile TYPE=$(TYPE)

clean_embret:
	cd embret ; make -f Makefile clean
#
#  --- EMSCVT ----
#
build_emscvt:
	@echo $(TYPE)
	@rm -vf inc
	@ln -s inc.$(TYPE) inc
	cd emscvt ; make -f Makefile TYPE=$(TYPE)

clean_emscvt:
	cd emscvt ; make -f Makefile clean
#
#  --- GRDEM ----
#
build_grdem:
	@echo $(TYPE)
	@rm -vf inc
	@ln -s inc.$(TYPE) inc
	cd grdem ; make -f Makefile TYPE=$(TYPE)

clean_grdem:
	cd grdem ; make -f Makefile clean
#
#  --- LBASE ----
#
build_lbase:
	@echo $(TYPE)
	@rm -vf inc
	@ln -s inc.$(TYPE) inc
	cd lbase ; make -f Makefile TYPE=$(TYPE)

clean_lbase:
	cd lbase ; make -f Makefile clean
#
#  --- MRGUAM ----
#
build_mrguam:
	@echo $(TYPE)
	@rm -vf inc
	@ln -s inc.$(TYPE) inc
	cd mrguam ; make -f Makefile TYPE=$(TYPE)

clean_mrguam:
	cd mrguam ; make -f Makefile clean
#
#  --- PIGEMS ----
#
build_pigems:
	@echo $(TYPE)
	@rm -vf inc
	@ln -s inc.$(TYPE) inc
	cd pream ; make -f Makefile TYPE=$(TYPE)

clean_pigems:
	cd pigems ; make -f Makefile clean
#
#  --- PREAM ----
#
build_pream:
	@echo $(TYPE)
	@rm -vf inc
	@ln -s inc.$(TYPE) inc
	cd pream ; make -f Makefile TYPE=$(TYPE)

clean_pream:
	cd pream ; make -f Makefile clean
#
#  --- PREFIR ----
#
build_prefir:
	@echo $(TYPE)
	@rm -vf inc
	@ln -s inc.$(TYPE) inc
	cd prefir ; make -f Makefile TYPE=$(TYPE)

clean_prefir:
#
#  --- PRESHP ----
#
build_preshp:
	@echo $(TYPE)
	@rm -vf inc
	@ln -s inc.$(TYPE) inc
	cd preshp ; make -f Makefile TYPE=$(TYPE)

clean_preshp:
	cd preshp ; make -f Makefile clean
#
#  --- PREPNT ----
#
build_prepnt:
	@echo $(TYPE)
	@rm -vf inc
	@ln -s inc.$(TYPE) inc
	cd prepnt ; make -f Makefile TYPE=$(TYPE)

clean_prepnt:
	cd prepnt ; make -f Makefile clean
#
#  --- PSTFIR ----
#
build_pstfir:
	@echo $(TYPE)
	@rm -vf inc
	@ln -s inc.$(TYPE) inc
	cd pstfir ; make -f Makefile TYPE=$(TYPE)

clean_pstfir:
#
#  --- PSTSHP ----
#
build_pstshp:
	@echo $(TYPE)
	@rm -vf inc
	@ln -s inc.$(TYPE) inc
	cd pstshp ; make -f Makefile TYPE=$(TYPE)

clean_pstshp:
	cd pstshp ; make -f Makefile clean
#
#  --- PSTPNT ----
#
build_pstpnt:
	@echo $(TYPE)
	@rm -vf inc
	@ln -s inc.$(TYPE) inc
	cd pstpnt ; make -f Makefile TYPE=$(TYPE)

clean_pstpnt:
	cd pstpnt ; make -f Makefile clean
#
#  --- SPCEMS ----
#
build_spcems:
	@echo $(TYPE)
	@rm -vf inc
	@ln -s inc.$(TYPE) inc
	cd spcems ; make -f Makefile TYPE=$(TYPE)

clean_spcems:
	cd spcems ; make -f Makefile clean
#
#  --- SPLTEM ----
#
build_spltem:
	@echo $(TYPE)
	@rm -vf inc
	@ln -s inc.$(TYPE) inc
	cd spltem ; make -f Makefile TYPE=$(TYPE)

clean_spltem:
	cd spltem ; make -f Makefile clean
#
#  --- TMPFAC ----
#
build_tmpfac:
	@echo $(TYPE)
	@rm -vf inc
	@ln -s inc.$(TYPE) inc
	cd tmpfac ; make -f Makefile TYPE=$(TYPE)

clean_tmpfac:
	cd tmpfac ; make -f Makefile clean
#
#  --- TMPRL ----
#
build_tmprl:
	@echo $(TYPE)
	@rm -vf inc
	@ln -s inc.$(TYPE) inc
	cd tmprl ; make -f Makefile TYPE=$(TYPE)

clean_tmprl:
	cd tmprl ; make -f Makefile clean
