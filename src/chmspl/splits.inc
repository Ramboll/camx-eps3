c*** SPLITS.INC  
c
c-----------------------------------------------------------------------
c
c    Include file for split factors file
c    we group each species name with its molecular weight and
c    the criteria pollutant which contributes to it.  For all
c    of our species, we know that only one criteria pollutant
c    contributes to it.  (However, the same criteria pollutant
c    could contribute to more than one species).
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
c     FACSIZ  I   size of the input record for one set of factors
c
      integer*4 MXSPL
      integer*4 FACSIZ
c
      parameter (MXSPL=26500)
      parameter (FACSIZ=MXSPL*10 + 5)
c
c-----------------------------------------------------------------------
c     Variables:
c-----------------------------------------------------------------------
c
c   idefps   I  index of default splits profile (overall average)
c   indcrt   I  index for the criteria pollutants
c               set up in rdschm
c   indspl   I  index for species in split factors file list
c   nfac     I  number of factor lists
c   nsplit   I  number of species in splits file
c   prfspl   C  profile code for each factor list
c   splcrt   C  criteria pollutant which contributes to species
c   splmol   R  molecular weight
c   splnam   C  species name
c   spltab   R  table of splitting factors
c   lsplcrt  L  .TRUE. if the criteria pollutant needs XREF file
c
c
      character*(LENPRF) prfspl(MXSPL)
      character*10       splnam(MXSPEC)
      character*10       splcrt(MXSPEC)
      integer*4          idefsp
      integer*4          indcrt(MXSPEC)
      integer*4          indspl(MXSPEC)
      integer*4          nfac
      integer*4          nsplit
      real*4             splmol(MXSPEC)
      real*4             spltab(MXSPL,MXSPEC)
      logical*4          lsplcrt(MXCRIT)
c
      common /splchr/ splnam, splcrt, prfspl
      common /splnum/ nsplit, indcrt, indspl, nfac, spltab, 
     &                splmol, idefsp, lsplcrt
