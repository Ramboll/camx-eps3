c*** PRSHPDAT.INC 
c
c-----------------------------------------------------------------------
c
c    Include file for the stack data variables for the PREPNT program
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Variables for pollutant codes
c-----------------------------------------------------------------------
c
c   ipolcd   I   array of codes for criteria pollutants
c   ncdpol   I   number of pollutant codes in the array
c
      integer*4 ipolcd(MXCRIT)
      integer*4 ncdpol
c
      common /poldat/ ipolcd, ncdpol
c
c-----------------------------------------------------------------------
c   Parameters for array bounds:
c-----------------------------------------------------------------------
c
c   MXSHIP    I   maximum number of records in a facility
c
      integer*4 MXSHIP
c
      parameter( MXSHIP = 60 )
c
c-----------------------------------------------------------------------
c   SHIPS data variables:
c-----------------------------------------------------------------------
c
c   crtpol   R  emissions of specified pollutant (tons/average day)
c   ibegdt   I  beginning date of inventory
c   ienddt   I  ending date of inventory
c   infips   C  input FIPS county code
c   invesl   C  vessel type
c   inpol    I  pollutant code
c   insbrg   C  subregion code
c   inscc    C  SCC code
c   inzone   I  UTM zone of point
c   iperod   C  inventory period
c   xlocbeg  R  X coordinate of begining of channel (m)
c   ylocbeg  R  Y coordinate of begining of channel (m)
c   widebeg  R  width of channel at begining
c   xlocend  R  X coordinate of end of channel (m)
c   ylocend  R  Y coordinate of end of channel (m)
c   wideend  R  width of channel at end
c
      character*10 inscc(0:MXSHIP)
      character*10 invesl(0:MXSHIP)
      character*5  infips(0:1)
      character*5  insbrg(0:1)
      character*2  iperod
      integer*4    ibegdt(0:MXSHIP)
      integer*4    ienddt(0:MXSHIP)
      integer*4    inpol
      integer*4    inzone(0:1)
      real*4       crtpol(MXCRIT,0:MXSHIP)
      real*4       xlocbeg(0:1)
      real*4       ylocbeg(0:1)
      real*4       widebeg(0:1)
      real*4       xlocend(0:1)
      real*4       ylocend(0:1)
      real*4       wideend(0:1)
c
      common /stkchr/ inscc, invesl, infips, insbrg, iperod
c
      common /stkdat/ ibegdt, ienddt, inzone, inpol, xlocbeg, ylocbeg, 
     &                widebeg, xlocend, ylocend, wideend, crtpol
c
c-----------------------------------------------------------------------
c   Gridded data variables:
c-----------------------------------------------------------------------
c
c   shiparea  R  total area of the shipping channel
c   gridarea  R  area in each cell intersecting with the shipping channel
c   iclxbeg   I first X grid cell that intersects the shipping channel
c   jclybeg   I first Y grid cell that intersects the shipping channel
c   iclxend   I last X grid cell that intersects the shipping channel
c   jclyend   I last Y grid cell that intersects the shipping channel
c
      real*4 shiparea
      real*4 gridarea(MXCELX,MXCELY)
      integer*4 iclxbeg
      integer*4 jclybeg
      integer*4 iclxend
      integer*4 jclyend
c
      common /grddat/ shiparea, gridarea, iclxbeg, jclybeg, 
     &                iclxend, jclyend
