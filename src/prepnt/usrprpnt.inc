c*** USRPRPNT.INC 
c
c-----------------------------------------------------------------------
c
c    Include file for user input parameters and variables for PREPNT
c    program
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    USERIN variables:
c-----------------------------------------------------------------------
c
c   dfdiam   R   default stack diameter (m)
c   dfhite   R   default stack height (m)
c   dftemp   R   default gas exit temperature (k)
c   dfvel    R   default gas ecit velocity (m/s)
c   lgeods   L   flag for input coordinates are geodesic
c   lprces   L   flag to determine if missing stack parameters are to be
c                replaced by default values
c   pcutof   R   plume height cutoff value
c
      real*4    dfdiam
      real*4    dfhite
      real*4    dftemp
      real*4    dfvel
      real*4    pcutof
      logical*4 lgeods
      logical*4 lprces
c
      common /usrdat/ lgeods, lprces, pcutof, dfhite, dfdiam, dftemp,
     &                dfvel
