c*** SPLTEM.INC
c
c-----------------------------------------------------------------------
c
c    include file for user inputs for SPLTEM program
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c     08/12/97  -mmj-   original development
c
c-----------------------------------------------------------------------
c   parameter list for SPLTEM routines:
c-----------------------------------------------------------------------
c
c    CVTGRM R    conversion factor of tons to grams
c    DEFTOL R    default value for acceptable weight tolerance
c    MXPRF  I    maximum number of profile codes
c    MXWGT  I    maximum number of weight fractions for a profile
c    MXSRD  I    maximum number of saroad codes to process
c    MXCEL  I    maximum number of totals cells allowed
c
      real*4     CVTGRM
      real*4     DEFTOL
      integer*4  MXPRF
      integer*4  MXWGT
      integer*4  MXSRD
      integer*4  MXCEL
c
      parameter ( CVTGRM = 907184.7 )
      parameter ( DEFTOL = 0.05 )
      parameter ( MXPRF = 10000 )
      parameter ( MXWGT = 500 )
      parameter ( MXSRD = 300 )
      parameter ( MXCEL = 380 )
c
c-----------------------------------------------------------------------
c   user inputs
c-----------------------------------------------------------------------
c
c   epslon    R   value for determining a valid weight profile
c   ijlst     I   array of grid cell locations to extract
c   illxcl    I   lower left corner X cell
c   illycl    I   lower left corner Y cell
c   iurxcl    I   upper right corner X cell
c   iurycl    I   upper right corner Y cell
c   lsplit    L   flag to indicate if criteria pollutant should be split
c   lhcspl    L   flag to indicate if splitting HCs
c   lvocin    L   flag to indicate if input HC is VOC (not TOG)
c   lvocout   L   flag to indicate if output HC is VOC (not TOG)
c   ncells    I   number of cells in region to extract
c   crtspl    C   name of criteria pollutant to split
c   hcout     C   name of output VOC
c
      character*10 crtspl
      character*10 hcout
      integer*4    ijlst(MXCEL)
      integer*4    illxcl
      integer*4    illycl
      integer*4    iurxcl
      integer*4    iurycl
      integer*4    ncells
      logical*4    lsplit(MXCRIT)
      logical*4    lhcspl
      logical*4    lvocin
      logical*4    lvocout
      real*4       epslon
c
      common /usrchr/ crtspl, hcout
      common /usrrgn/ illxcl, illycl, iurxcl, iurycl, ncells, ijlst,
     &                lsplit, epslon, lhcspl, lvocin, lvocout
c
c-----------------------------------------------------------------------
c   output emissions arrays
c-----------------------------------------------------------------------
c
c   emsele    R   elevated sources emission totals
c   emslow    R   lowlevel sources emission totals
c
      real*4 emsele(MXCEL,0:MXHOUR-1,MXSRD)
      real*4 emslow(MXCEL,0:MXHOUR-1,MXSRD)
c
      common /emsdat/ emsele, emslow
