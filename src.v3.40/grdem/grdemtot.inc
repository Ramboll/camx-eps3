c*** GRDEMTOT.INC 
c
c-----------------------------------------------------------------------
c
c    Include file for emissions totals for GRDEM program
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Parameters for index in totals array:
c-----------------------------------------------------------------------
c
c   IDXELE   I   index of elevated emissions
c   IDXEMR   I   index of total emissions written to EMAR file
c   IDXIN    I   index of total input emissions
c   IDXOUT   I   index of total output emissions
c   IDXSEC   I   index of total emissions mapped to secondary surrogate
c   IDXTRT   I   index of total emissions mapped to tertiary surrogate
c   IDXSKP   I   index of total emissions skipped
c
      integer*4 IDXELE
      integer*4 IDXEMR
      integer*4 IDXIN
      integer*4 IDXOUT
      integer*4 IDXSEC
      integer*4 IDXTRT
      integer*4 IDXSKP
c
      parameter( IDXIN  = 1 )
      parameter( IDXOUT = 2 )
      parameter( IDXELE = 3 )
      parameter( IDXSEC = 4 )
      parameter( IDXTRT = 5 )
      parameter( IDXEMR = 6 )
      parameter( IDXSKP = 7 )
c
c-----------------------------------------------------------------------
c   Variables for totals of emissions:
c-----------------------------------------------------------------------
c
c   totems   R   array for emissions of criteria pollutants (gm)
c   totspc   R   array for speciated emissions (moles)
c
      real*8 totems(IDXSKP,MXCRIT)
      real*8 totspc(IDXSKP,MXSPEC)
c
      common /ttl/ totems, totspc
c
c-----------------------------------------------------------------------
c   Record processing totals:
c-----------------------------------------------------------------------
c
c   nrec     I   number of output records written
c
      integer*4   nrec(IDXSKP)
c
      common /trec/ nrec
c
c-----------------------------------------------------------------------
c   Gridded array:
c-----------------------------------------------------------------------
c
c   emsgrd  R  large array for storing gridded emissions
c   cellon  R  array for gridded longitude values
c   cellat  R  array for gridded latitude values
c   deltax  R  array of cell width for each row
c   deltay       R  array of cell width for all columns
c
      real*4 emsgrd(MXHOUR,MXSPEC,MXCELY,MXCELX)
      real*4 cellon(MXCELX,MXCELY)
      real*4 cellat(MXCELX,MXCELY)
      real*4 deltax(MXCELY)
      real*4 deltay
c
      common /emsdat/ emsgrd, cellon, cellat, deltax, deltay
c
c-----------------------------------------------------------------------
c   Variables for the last record:
c-----------------------------------------------------------------------
c
c   fiplast  C  FIPS code of the last record
c   asclast  C  Source category code of the last record
c   lnglast  C  "long" (county and subregion) FIPS code from the last record
c
      character*10 asclast
      character*10 lnglast
      character*5  fiplast
c
      common /lastchr/ asclast, lnglast, fiplast
