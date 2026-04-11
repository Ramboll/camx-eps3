c*** UAMREGN.INC 
c
c
c-----------------------------------------------------------------------
c
c   Variables and common blocks for the /UAMREGN/ packet of the USERIN
c   file
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c   jan 2018 gwilson added varaibles for projection definiton
c
c-----------------------------------------------------------------------
c   /UAMREGN/ variables:
c-----------------------------------------------------------------------
c
c   dxcell   R   grid cell size in X direction
c   dycell   R   grid cell size in Y direction
c   htlowr   R   minimum vertical cell height in lower layer
c   htsur    R   height of surface layer
c   htuppr   R   minimum vertical cell height in upper layer
c   izone    I   utm zone
c   nxcell   I   number of grid cells in X direction
c   nycell   I   number of grid cells in Y direction
c   nzcell   I   number of grid cells in vertical direction
c   nzlowr   I   number of vertical cells in lower layer
c   nzuppr   I   number of vertical cells in upper layer
c   utmx     R   utm easting reference origin
c   utmy     R   utm northing reference origin
c   xorig    R   grid origin in X direction
c   yorig    R   grid origin in Y direction
c   polelon  R   longitude of projection pole
c   polelat  R   latitude of projection pole
c   tlat1    R   1st true latitude
c   tlat2    R   2nd true latitude
c   llatlon  L   .TRUE. if projection is lat/lon
c   lutm     L   .TRUE. if projection is UTM
c   lambrt   L   .TRUE. if projection is LCP
c   lrpolar  L   .TRUE. if projection is
c   lpolar   L   .TRUE. if projection is polar stereographic
c   lmerc    L   .TRUE. if projection is
c
      integer*4 izone
      integer*4 nxcell
      integer*4 nycell
      integer*4 nzcell
      integer*4 nzlowr
      integer*4 nzuppr
      real*4    dxcell
      real*4    dycell
      real*4    htlowr
      real*4    htsur
      real*4    htuppr
      real*4    utmx
      real*4    utmy
      real*4    xorig
      real*4    yorig
      real*4    polelon
      real*4    polelat
      real*4    tlat1
      real*4    tlat2
      logical*4 llatlon
      logical*4 lutm
      logical*4 lambrt
      logical*4 lrpolar
      logical*4 lpolar
      logical*4 lmerc
c
      common /regdat/ utmx, utmy, izone, xorig, yorig, dxcell, dycell,
     &                nxcell, nycell, nzcell, nzlowr, nzuppr, htsur,
     &                htlowr, htuppr, polelon, polelat, tlat1, tlat2,
     &                llatlon, lutm, lambrt, lrpolar, lpolar, lmerc
