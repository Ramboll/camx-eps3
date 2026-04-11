c*** CMPDBASE.INC 
c
c-----------------------------------------------------------------------
c
c    Include file for compound data base used by the EMSCVT program.
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Array bound parameters:
c-----------------------------------------------------------------------
c
c   MXCOMP   I   maximum number of compounds
c
      integer*4 MXCOMP
c
      parameter( MXCOMP = 1000 )
c
c-----------------------------------------------------------------------
c    Selected species variables:
c-----------------------------------------------------------------------
c
c   bndfrc   R   bonding fraction for each species
c   idxcmp   I   index of requested species in compound species list
c   idxspl   I   index of compound species in requested species list
c   isarod   I   SARODE code of chemical compound
c   lreact   L   flag for determining if compound is reactive
c                (.TRUE. = reactive -- .FALSE. = non-reactive)
c   ltoxic   L   flag for determing if compound is considered toxic
c                (.TRUE. -- toxic compound  --  .FALSE. = non-toxic)
c   molcmp   R   molecular weight of chemical compound
c   ncmpnd   I   number of compounds in data base
c   nspcmp   I   number of CBIV species in data base
c   spccmp   C   CBIV species contained in the compound data base
c
      character*10 spccmp(MXSPEC)
      integer*4    idxcmp(MXSPEC)
      integer*4    idxspl(MXSPEC)
      integer*4    isarod(MXCOMP)
      integer*4    ncmpnd
      integer*4    nspcmp
      real*4       bndfrc(MXCOMP,MXSPEC)
      real*4       molcmp(MXCOMP)
      logical*4    lreact(MXCOMP)
      logical*4    ltoxic(MXCOMP)
c
      common /cmpchr/ spccmp
      common /cmpdat/ nspcmp, isarod, ncmpnd, bndfrc, molcmp,
     &                lreact, ltoxic, idxspl, idxcmp
