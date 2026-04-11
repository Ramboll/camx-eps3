c*** USREMCVT.INC 
c
c-----------------------------------------------------------------------
c
c    Include file for species parameters and variables for the EMSCVT
c    program
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    User input variables:
c-----------------------------------------------------------------------
c
c   epslon   R   value for determining a valid weight profile
c                (valid profile must be within epslon of 1.0)
c   lvocin   L   .TRUE. if input inventory will be VOC
c   lvocout  L   .TRUE. if splits will generate VOC emissions
c
      real*4    epslon
      logical*4 lvocin
      logical*4 lvocout
c
      common /usrdat/ epslon, lvocin, lvocout
c
c-----------------------------------------------------------------------
c    Selected species variables:
c-----------------------------------------------------------------------
c
c   defspl   R   default splitting factor for this species
c   molwt    R   molecular weight of species
c   nspecs   I   number of species
c   spclst   C   species for which to compute splits
c   spcrit   C   criteria pollutant producing species
c
      character*10 spclst(MXSPEC)
      character*5  spcrit(MXSPEC)
      integer*4    nspecs
      real*4       molwt(MXSPEC)
      real*4       defspl(MXSPEC)
c
      common /spechr/ spclst, spcrit
      common /spedat/ nspecs, molwt, defspl
c
c-----------------------------------------------------------------------
c    Parameters for control factors data:
c-----------------------------------------------------------------------
c
c   MXCTL   I    maximum number of control factors
c
      integer*4 MXCTL
c
      parameter( MXCTL = 1000)
c
c-----------------------------------------------------------------------
c    Variables for control factors data:
c-----------------------------------------------------------------------
c
c   nctlfac   I   number of control factors
c   ictlcod   I   SAROAD code of compound to control
c   ctlfac    I   control factor to apply to the compound
c
      integer*4 nctlfac
      integer*4 ictlcod(MXCTL)
      real*4    ctlfac(MXCTL)
c
      common /ectldat/ nctlfac, ictlcod, ctlfac
