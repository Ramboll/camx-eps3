c*** PRPNTSTK.INC 
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
c   MXFAC    I   maximum number of records in a facility
c
      integer*4 MXFAC
c
      parameter( MXFAC = 6000 )
c
c-----------------------------------------------------------------------
c   AFS data variables:
c-----------------------------------------------------------------------
c
c   crtpol   R  emissions of specified pollutant (tons/average day)
c   ibegdt   I  beginning date of inventory
c   ienddt   I  ending date of inventory
c   infips   C  input FIPS county code
c   inplnt   C  NEDS plant code
c   instck   C  stack number
c   inpnt    C  emissions point identifier
c   inpol    I  pollutant code
c   insbrg   C  subregion code
c   inscc    C  SCC code
c   inseg    I  segment number
c   insic    C  SIC code
c   inzone   I  UTM zone of point
c   iperod   C  inventory period
c   irefyr   I  inventory reference year
c   plmhit   R  effective plume height of source (m)
c   stdiam   R  stack diam (m)
c   stextp   R  stack gas temperature (k)
c   stexvl   R  stack gas exit velocity (m/s)
c   stkht    R  stack height (m)
c   xloc     R  X coordinate of point (UTM E or lattitude)
c   yloc     R  Y coordinate of point (UTM N or longitude)
c
      character*10 inscc(0:MXFAC)
      character*10 inplnt(0:1)
      character*10 inpnt(0:MXFAC)
      character*10 instck(0:MXFAC)
      character*5  infips(0:1)
      character*5  insbrg(0:1)
      character*4  insic(0:MXFAC)
      character*3  inseg(0:MXFAC)
      character*2  iperod
      integer*4    ibegdt(0:MXFAC)
      integer*4    ienddt(0:MXFAC)
      integer*4    inpol
      integer*4    inzone(0:1)
      integer*4    irefyr
      real*4       crtpol(MXCRIT,0:MXFAC)
      real*4       plmhit(0:MXFAC)
      real*4       stdiam(0:MXFAC)
      real*4       stextp(0:MXFAC)
      real*4       stexvl(0:MXFAC)
      real*4       stkht(0:MXFAC)
      real*4       xloc(0:MXFAC)
      real*4       yloc(0:MXFAC)
c
      common /stkchr/ inscc, inplnt, infips, insbrg, insic, instck,
     &                inpnt, inseg, iperod
c
      common /stkdat/ irefyr, ibegdt, ienddt, inzone, inpol, xloc, yloc, 
     &                stkht, stdiam, stextp, stexvl, plmhit, crtpol
