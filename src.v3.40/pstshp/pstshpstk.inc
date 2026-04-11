C**** PSTSHPSTK.INC 
c
c-----------------------------------------------------------------------
c
c    Include file for the stack data variables for the PSTSHP program
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c   june 2015 mjimenez Updated to support the plume rise
c                      distribution override feature of CAMx
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
      parameter( MXSTK   = 140000 )
      parameter( MXPTSCC =  3500 )
c
c-----------------------------------------------------------------------
c   Stack data variables:
c-----------------------------------------------------------------------
c
c   fipstk   C  input FIPS county code
c   ascstk   C  ASC code
c   vslstk   C  vessel type
c   ijclstk  I  grid cell
c   ptopstk  R  plume top
c   pbotstk  R  plume bottom
c   nstack   I  number of stacks
c
      character*5  fipstk(MXSTK)
      character*10 ascstk(MXSTK)
      character*10 vslstk(MXSTK)
      real*4       ptopstk(MXSTK)
      real*4       pbotstk(MXSTK)
      integer*4    ijclstk(MXSTK)
      integer*4    nstack
c
      common /stkchr/ fipstk, ascstk, vslstk
c
      common /stkdat/ ptopstk, pbotstk, ijclstk, nstack
c
c-----------------------------------------------------------------------
c   Parameters for lookup table for the stack heights:
c-----------------------------------------------------------------------
c
c   MXSTKMAP  I   maximum number of records in the vessel/stack lookup file
c
      integer*4 MXSTKMAP
c
      parameter( MXSTKMAP = 1000 )
c
c-----------------------------------------------------------------------
c   Variables for lookup table for the stack heights:
c-----------------------------------------------------------------------
c
c  ascmap    C   source cagtegory code for stack heigh mapping
c  vslmap    C   vessel type for stack heigh mapping
c  plumebot  R   bottom of the plume (meters above water)
c  plumetop  R   top of the plume (meters above water)
c  plumefrac R   fraction of the plume in surface layer
c  nvslmap   I   number of records in lookup table
c
      character*10 ascmap(MXSTKMAP)
      character*10 vslmap(MXSTKMAP)
      real*4       plumebot(MXSTKMAP)
      real*4       plumetop(MXSTKMAP)
      real*4       plumefrac(MXLAYER,MXSTKMAP)
      integer*4    nvslmap
c
      common /stkmapchr/ ascmap, vslmap
c
      common /stkmapdat/ plumebot, plumetop, plumefrac, nvslmap
