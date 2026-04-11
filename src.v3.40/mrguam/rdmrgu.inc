c**** RDMRGU.INC 
c
c-----------------------------------------------------------------------
c
c    Include file for the user input data used by the MRGUAM program
c
c-----------------------------------------------------------------------
c
c   adjfac   R  adjustment factors applied to each species in each file
c   lchkhd   L  flag for determing if the headers are checked for 
c               consistency (TRUE = check the headers and abort if they
c               are inconsistent)
c
      real*4    adjfac(MXMRG,MXSPEC)
      logical*4 lchkhd
c
      common /adjdat/ adjfac, lchkhd
