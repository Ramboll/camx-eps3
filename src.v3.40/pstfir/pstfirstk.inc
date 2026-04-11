C**** PSTFIRSTK.INC 
c
c-----------------------------------------------------------------------
c
c    Include file for the stack data variables for the PSTFIR program
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
      parameter( MXSTK   = 3500 )
      parameter( MXPTSCC = 3500 )
c
c-----------------------------------------------------------------------
c   Stack data variables:
c-----------------------------------------------------------------------
c
c   infips   C  input FIPS county code
c   inplnt   C  NEDS plant code
c   stckin   C  stack identifier
c   nstack   I  number of stacks
c   stdiam   R  stack diam (m)
c   xloc     R  X coordinate of point (UTM E or grid units)
c   yloc     R  Y coordinate of point (UTM N or grid units)
c   stktop   R  plume rise top , varies by hour
c   stkbot   R  plume rise top , varies by hour
c
      character*5  infips(MXSTK)
      character*10 inplnt(MXSTK)
      character*10 stckin(MXSTK)
      integer*4    nstack
      integer*4    npts
      real*4       stdiam(MXSTK)
      real*4       xloc(MXSTK)
      real*4       yloc(MXSTK)
      real*4       stktop(MXSTK,MXHOUR)
      real*4       stkbot(MXSTK,MXHOUR)
c
      common /stkchr/ inplnt, infips, stckin
c
      common /stkdat/ nstack, npts, xloc, yloc, stdiam,
     &                stktop, stkbot
c
