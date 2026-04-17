c*** USRPSTFIR.INC
c
c-----------------------------------------------------------------------
c
c    Include file for user input parameters and variables for PSTFIR
c    program
c
c-----------------------------------------------------------------------
c    LOG:
c          June 2015 mjimenez Added additional arrays to support
c                    splitting the WRAP paper surface fraction if
c                    layer1 is < 90% of WRAP 38m layer1.
c-----------------------------------------------------------------------
c    PARAMETERS:
c-----------------------------------------------------------------------
c
c   WLAY1 R  layer 1 used in the WRAP PhaseII Fire emission inventory
c
      real*4   WLAY1
c
cgwilson      parameter ( WLAY1 = 38. )
      parameter ( WLAY1 = 1. )
c
c-----------------------------------------------------------------------
c    USERIN variables PSTFIR packet:
c-----------------------------------------------------------------------
c
c   invtzn   I   inventory time zone
c
      integer*4 invtzn 
c
      common /usrdat/ invtzn
c
c-----------------------------------------------------------------------
c    Computed fractions and plumes top/bottoms:
c-----------------------------------------------------------------------
c
c   elvfrac   R   fraction of emissions in elevated layers
c   srffrac   R   fraction of emissions in surface layer
c   plmovrtop R   plume rise distribution override top
c   plmovrbot R   plume rise distribution override bottom
c   aglfrac   R   fraction of WRAP layer1 above user specified layer 1 ht
c   srfbot    R   plume bottom if above WRAP layer1
c   srftop    R   plume top if above WRAP lauer1
c
      real*4    elvfrac(MXBIN,MXHOUR)
      real*4    srffrac(MXBIN,MXHOUR)
      real*4    plmovrtop(MXBIN,MXHOUR)
      real*4    plmovrbot(MXBIN,MXHOUR)
      real*4    aglfrac(MXBIN,MXHOUR)
      real*4    srfbot(MXBIN,MXHOUR)
      real*4    srftop(MXBIN,MXHOUR)
c
      common /clcdat/ elvfrac, srffrac, plmovrtop, plmovrbot,
     &                aglfrac, srfbot, srftop
c
c-----------------------------------------------------------------------
c   Reporting totals:
c-----------------------------------------------------------------------
c
c   crtlay  R  output criteria emissions by vertical layer
c
c     real*4 crtlay(MXCRIT,0:MXLAY)
c
c     common /urptdat/ crtlay

