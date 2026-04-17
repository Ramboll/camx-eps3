c*** FACTORS.INC
c
c-----------------------------------------------------------------------
c
c    Include file for SPCEMS input data:
c    speciation factors by profile (GSPRO),
c    conversion factors by profile (GSCNV).
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c     Parameters:
c-----------------------------------------------------------------------
c
c     MXSPL   I   maximum number of entries in speciation factors
c
      integer*4 MXSPL
c
      parameter (MXSPL=1000000)
c
c-----------------------------------------------------------------------
c     Variables for speciation factors (GSPRO):
c-----------------------------------------------------------------------
c
c   nprf     I  number of input speciation profiles records
c   lchkcnv  L  flag indicates if need conversion factor
c   prffac   C  profile codes list 
c   spcfac   R  speciation factors by profile, criteria, speciated poll
c
c
      character*(LENPRF) prffac(MXSPL)
      integer*4          nprf
      logical*4          lchkcnv(MXSPL)
      real*4             spcfac(MXSPL,MXCRIT,MXSPEC)
c
      common /facchr/ prffac
      common /facnum/ nprf, lchkcnv, spcfac

c-----------------------------------------------------------------------
c     Variables for checking EMBR header species list:
c-----------------------------------------------------------------------
c
c   iparnt    I   index of parent criteria pollutant
c   ncntpar   I   number of different parents
c
       integer*4    iparnt(MXSPEC)
       integer*4    ncntpar(MXSPEC)
c
       common /chkdat/ iparnt, ncntpar

c-----------------------------------------------------------------------
c     Variables for pollutant conversion factors data (GSCNV):
c-----------------------------------------------------------------------
c
c   ncnv     I  number of input GSCNV records
c   cnvftr   R  conversion factor
c   lcnvset  L  indicates if a profile has a conversion factor set
c
c
      integer*4          ncnv
      real*4             cnvftr(MXSPL)
      logical*4          lcnvset(MXSPL)
c
      common /cnvnum/ ncnv, cnvftr, lcnvset

