c***** RDEMIS.INC 
c
c-----------------------------------------------------------------------
c     Include file for emissions data used by the MRGUAM program
c-----------------------------------------------------------------------
c
c   hrtot  R  totals for each file by hour
c   total  R  total for each file
c   u1     R  array for merged emissions
c   uin    R  array for emissions read from input file
c
      real*4 hrtot(MXMRG,MXSPEC)
      real*4 total(MXMRG,MXSPEC)
      real*4 u1(MXCELX,MXCELY,MXSPEC)
      real*4 uin(MXCELX,MXCELY)
c
      common /emidat/ hrtot, total, u1, uin
