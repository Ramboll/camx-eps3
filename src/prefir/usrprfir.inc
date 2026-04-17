c*** USRPRFIR.INC
c
c-----------------------------------------------------------------------
c
c    Include file for user input parameters and variables for PREFIR
c    program
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    USERIN variables PREFIR packet:
c-----------------------------------------------------------------------
c
c   ibindf   I   default fire class
c   dxcomb   R   grid resolution for combining point sources
c   lgeods   L   flag for input coordinates are geodesic
c   lprces   L   flag to determine if missing stack parameters are to be
c                replaced by default values
c   lcomb    L   flag to determine if point sources in a cell should be 
c                combined
c
      real*4    dxcomb
      integer*4 ibindf 
      logical*4 lgeods
      logical*4 lprces
      logical*4 lcomb
c
      common /usrdat/ lgeods, lprces, ibindf , lcomb, dxcomb

