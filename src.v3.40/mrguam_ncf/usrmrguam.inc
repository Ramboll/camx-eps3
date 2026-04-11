c**** RDMRGU.INC 
c
c-----------------------------------------------------------------------
c
c    Include file for the user input data used by the MRGUAM program
c
c-----------------------------------------------------------------------
c
c   lchkhd   L  flag for determing if the headers are checked for 
c               consistency (TRUE = check the headers and abort if they
c               are inconsistent)
c    
c   adjfac   R  adjustment factors applied to each species in each file
c   wtmol    R  molecular weight of species  
c   idxcrt   I  index of criteria pollutant that includes this species
c
      integer*4 idxcrt(MXSPEC)
      real*4    wtmol(MXSPEC)
      real*4    adjfac(MXMRG,MXSPEC)
      logical*4 lchkhd
c
      common /adjdat/ idxcrt, wtmol, adjfac, lchkhd
