c*** PRFIRPNT.INC 
c
c-----------------------------------------------------------------------
c
c    Include file for emissions data variables for the PREFIR program.
c    If points are combined by grid cell then the 'combined' (cmb)
c    arrays are written after all input records are processed.  If
c    points are not combined then the 'current' data is written as
c    single EMBR record.
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Parameters for array bounds:
c-----------------------------------------------------------------------
c
c   MXPTS    I   maximum number of points to save
c
      integer*4 MXPTS
c
      parameter( MXPTS = 25000 )
c
c-----------------------------------------------------------------------
c   PREFIR data variables for 'current' record:
c-----------------------------------------------------------------------
c
c   crtpol   R  emissions of specified pollutant (tons/average day)
c   ibegdt   I  beginning date of inventory
c   ienddt   I  ending date of inventory
c   infips   C  input FIPS county code
c   inplnt   C  NEDS plant code
c   instck   C  stack number
c   insbrg   C  subregion code
c   inscc    C  SCC code
c   iperod   C  inventory period
c   irefyr   I  inventory reference year
c   stdiam   R  stack diam (m)
c   xloc     R  X coordinate of point (UTM E or lattitude)
c   yloc     R  Y coordinate of point (UTM N or longitude)
c
      character*10 curscc
      character*10 curplnt
      character*10 curstck
      character*5  curfips
      character*2  fperiod
      integer*4    irefyr
      integer*4    curbegdt
      integer*4    curenddt
      integer*4    ijcur
      integer*4    curbin
      integer*4    izoncur
      real*4       xloccur
      real*4       yloccur
      real*4       curems(MXCRIT)
c
      common /curchr/ curscc, curplnt, curfips, curstck,
     &                fperiod
c
      common /curdat/ irefyr, curbegdt, curenddt, izoncur, xloccur, 
     &                yloccur, ijcur, curbin, curems
c
c-----------------------------------------------------------------------
c   PREFIR data variables for 'combined' points:
c-----------------------------------------------------------------------
c
      character*10 cmbscc(MXPTS)
      character*5  cmbfips(MXPTS)
      integer*4    ncmb
      integer*4    ibegcmb(MXPTS)
      integer*4    iendcmb(MXPTS)
      integer*4    ibincmb(MXPTS)
      integer*4    ijcmb(MXPTS)
      integer*4    pntscmb(MXPTS)
      real*4       emiscmb(MXCRIT,MXPTS)
c
      common /pntchr/ cmbscc, cmbfips
c
      common /pntdat/ ncmb, ibegcmb, iendcmb, ijcmb,
     &                ibincmb, pntscmb, emiscmb
