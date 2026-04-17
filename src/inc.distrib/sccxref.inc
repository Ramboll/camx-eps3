c*** SCCXRF.INC 
c
c-----------------------------------------------------------------------
c
c    Include file for ASC/SCC profiles cross reference data used by
c    many of the core EPS modules.
c
c-----------------------------------------------------------------------
c   LOG:
c
c         1. gwilson      Tue Apr 26 10:56:37 PDT 2005
c            Added point ID to lookup table
c         2. mjimenez     Wed Mar 19 13:04:06 PDT 2008
c            Added pollutant code to lookup table; optional for chmspl
c                                                  required for spcems
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Parameters for cross reference data:
c-----------------------------------------------------------------------
c
c   MXREF    I   parameter for array bounds
c
      integer*4 MXREF
c
      parameter( MXREF  = 80000 )
c
c-----------------------------------------------------------------------
c   Cross reference data variables:
c-----------------------------------------------------------------------
c
c   facxrf   C   array of facilities
c   fipxrf   C   array of FIPS codes
c   stkxrf   C   array of stacks
c   pntxrf   C   array of point ID
c   npfxrf   I   number of cross reference records
c   prfxrf   C   speciation profile code
c   sccxrf   C   array of SCC codes
c   crtxrf   C   array of criteria pollutant names
c   lxrfpnt  L   flag to determine if file has point ID's
c   lchkfac  L   flag to determine if file has facility specific entries
c
      character*(LENPRF) prfxrf(MXREF)
      character*10       sccxrf(MXREF)
      character*10       facxrf(MXREF)
      character*10       stkxrf(MXREF)
      character*10       pntxrf(MXREF)
      character*10       crtxrf(MXREF)
      character*5        fipxrf(MXREF)
      integer*4          npfxrf
      logical*4          lxrfpnt
      logical*4          lchkfac
c
      common /xrfchr/ fipxrf, sccxrf, facxrf, prfxrf, stkxrf, pntxrf,
     &                crtxrf
      common /xrfdat/ npfxrf, lxrfpnt, lchkfac
c
c-----------------------------------------------------------------------
c   Default splits data:
c-----------------------------------------------------------------------
c
c   dftlst   C   name of the species in the default splits list
c   dftcrit  C   name of the criteria pollutant for this species
c   dftmlwt  R   default molecular weight for this species
c   dftspl   R   default splits value
c   idfcrt   I   index into the criteria pollutants list
c   idflst   I   index into the species list
c   ndftspl  I   number of default splits
c
      character*10 dftlst(MXSPEC)
      character*10 dftcrit(MXSPEC)
      real*4       dftmlwt(MXSPEC)
      real*4       dftspl(MXSPEC)
      integer*4    idfcrt(MXSPEC)
      integer*4    idflst(MXSPEC)
      integer*4    ndftspl
c
      common /dftchr/ dftlst, dftcrit
      common /dftdat/ dftmlwt, dftspl, idfcrt, idflst, ndftspl
