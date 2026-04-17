c*** EMCVTSPL.INC 
c
c-----------------------------------------------------------------------
c
c    Include file for splits data split to be used by the EMSCVT program
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c     Parameters:
c-----------------------------------------------------------------------
c
c     MXSPL   I   maximum number of entries in splits file
c
      integer*4 MXSPL
c
      parameter (MXSPL = 8000)
c
c-----------------------------------------------------------------------
c     Variables:
c-----------------------------------------------------------------------
c
c   nfac     I  number of factor lists
c   nsplit   I  number of species in splits file
c   prfspl   C  profile code for each factor list
c   splcrt   C  criteria pollutant which contributes to species
c               order
c   splmol   R  molecular weight
c   splnam   C  species name
c   spltab   R  table of splitting factors
c
      character*(LENPRF) prfspl(MXSPL)
      character*10       splnam(MXSPEC)
      character*5        splcrt(MXSPEC)
      integer*4          nfac
      integer*4          nsplit
      real*4             splmol(MXSPEC)
      real*4             spltab(MXSPL,MXSPEC)
c
      common /splchr/ splnam, splcrt, prfspl
      common /splnum/ nsplit, nfac, spltab, splmol
