c*** FIREBIN.INC
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c
c   Variables and common blocks for the /FIRE BINS/ packet which contains
c   the fire class definition data.
c
c-----------------------------------------------------------------------
c   /FIRE BINS/ parameters:
c-----------------------------------------------------------------------
c
c   MXBIN   I   maximum number of fire class defintions
c
      integer*4  MXBIN

      parameter (MXBIN=5)
c
c-----------------------------------------------------------------------
c   /FIRE BINS/ variables:
c-----------------------------------------------------------------------
c
c   nfbin    I   number of fire classes defined
c   lbindef  L   TRUE if using default class defintions
c   sizfbin  R   vitual acreage by fire class
c   befbin   R   buoyant efficiency by class
c   topfbin  R   Ptop max (m) by class
c   botfbin  R   Pbot max (m) by class
c
      real*4      sizfbin(MXBIN)
      real*4      befbin(MXBIN)
      real*4      topfbin(MXBIN)
      real*4      botfbin(MXBIN)
      integer*4   nfbin
      logical*4   lbindef
c
      common /bindat/  lbindef, nfbin, sizfbin, befbin, topfbin, botfbin
c
c-----------------------------------------------------------------------
c   Reporting by fire class:
c-----------------------------------------------------------------------
c
c   crtbin  R  output criteria emissions by fire class
c
      real*4 crtbin(MXCRIT,MXBIN)
c
      common /frptdat/ crtbin

