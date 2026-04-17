c*** LAYERS.INC
c
c-----------------------------------------------------------------------
c
c    Include file for user input parameters and variables for the
c    /LAYERS/ packet
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   /LAYERS/ parameters:
c-----------------------------------------------------------------------
c
c   MXLAY   I   maximum number of layers
c
      integer*4  MXLAY

      parameter (MXLAY=36)

c
c-----------------------------------------------------------------------
c    USERIN variable PSTFIR packet:
c-----------------------------------------------------------------------
c
c   lelvall   L  flag to indicate if ALL emissions should be written
c                to the elevated point source file
c
      logical*4    lelvall

c
c-----------------------------------------------------------------------
c    USERIN variables LAYERS packet:
c-----------------------------------------------------------------------
c
c   nlayer   I   number of layers
c   aglht    R   lay AGL ht(m)
c
      real*4    aglht(0:MXLAY)
      integer*4 nlayer 
c
      common /usrdat/ lelvall,nlayer, aglht
c
c-----------------------------------------------------------------------
c    Computed fractions based on layers:
c-----------------------------------------------------------------------
c
c   ctrlayr  R   center point of layer, computed, output as effective plume ht
c   dzlayr   R   the thickness of layer
c
      real*4    ctrlayr(MXLAY)
      real*4    dzlayr(MXLAY)
c
      common /clcdat/ ctrlayr, dzlayr
