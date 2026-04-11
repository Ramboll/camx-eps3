C**** PSTPNTSTK.INC 
c
c-----------------------------------------------------------------------
c
c    Include file for the stack data variables for the PSTPNT program
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Parameters for array bounds:
c-----------------------------------------------------------------------
c
c   MXSTK    I   maximum number of records in a facility
c   MXPTSCC  I   maximum number of SCC per stack
c
      integer*4 MXSTK
      integer*4 MXPTSCC
c
      parameter( MXSTK   = 180000 )
      parameter( MXPTSCC = 100000 )
c
c-----------------------------------------------------------------------
c   Stack data variables:
c-----------------------------------------------------------------------
c
c   infips   C  input FIPS county code
c   inplnt   C  NEDS plant code
c   ptflag   C  flag that determines if stack is elevated
c   stckin   C  stack number
c   nstack   I  number of stacks
c   nstele   I  number of elevated stacks
c   stdiam   R  stack diam (m)
c   stextp   R  stack gas temperature (K)
c   stexvl   R  stack gas exit velocity (m/s)
c   stkht    R  stack height (m)
c   xloc     R  X coordinate of point (UTM E or grid units)
c   yloc     R  Y coordinate of point (UTM N or grid units)
c   istkpt   I  pointer into array for first stack in county
c
      character*10 inplnt(MXSTK)
      character*10 stckin(MXSTK)
      character*5  infips(MXSTK)
      character*1  ptflag(MXSTK)
      integer*4    nstack
      integer*4    nstele
      integer*4    istkpt(MXFIP)
      real*4       stdiam(MXSTK)
      real*4       stextp(MXSTK)
      real*4       stexvl(MXSTK)
      real*4       stkht(MXSTK)
      real*4       xloc(MXSTK)
      real*4       yloc(MXSTK)
c
      common /stkchr/ inplnt, infips, ptflag, stckin
c
      common /stkdat/ nstack, nstele, xloc, yloc, 
     &                stkht, stdiam, stextp, stexvl, istkpt
c
c-----------------------------------------------------------------------
c   Variables for emissions by SCC and stack:
c-----------------------------------------------------------------------
c
c   ptscc    C  SCC codes for sources feeding stack
c   ptsic    C  SIC codes for the facility containing the stack
c   nptscc   I  number of SCC codes in arrays
c   iptscc   I  pointer into array of SCC codes feeding the stack
c   emptscc  R  emissions by stack and SCC
c
      character*10 ptscc(MXPTSCC)
      character*4  ptsic(MXPTSCC)
      integer*4    iptscc(MXSTK)
      integer*4    nptscc
      real*4       emptscc(MXPTSCC,MXCRIT)
c
      common /ptscchr/ ptscc, ptsic
      common /ptscdat/ nptscc, emptscc, iptscc
c 
