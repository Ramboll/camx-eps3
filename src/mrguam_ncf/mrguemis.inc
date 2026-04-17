c***** RDEMIS.INC 
c
c-----------------------------------------------------------------------
c     Include file for emissions data used by the MRGUAM program
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Parameters for index in totals array:
c-----------------------------------------------------------------------
c
c   IDXIN    I   index of total input emissions
c   IDXOUT   I   index of total output emissions
c
      integer*4 IDXIN
      integer*4 IDXOUT
c
      parameter( IDXIN  = 1 )
      parameter( IDXOUT = 2 )
c
c-----------------------------------------------------------------------
c   Variables for totals of emissions:
c-----------------------------------------------------------------------
c
c   totems  R  total of processed emissions for criteria pollutants
c   totspc  R  total of processed emissions for speciate pollutants
c   emisout R  array for merged emissions
c
      real*8 totems(IDXOUT,MXCRIT)
      real*8 totspc(IDXOUT,MXSPEC)
      real*4 emisout(MXCELX,MXCELY,MXSPEC)
c
      common /emidat/ totems, totspc, emisout
c
c-----------------------------------------------------------------------
c   Grid definition array:
c-----------------------------------------------------------------------
c
c   cellon  R  array for gridded longitude values
c   cellat  R  array for gridded latitude values
c   deltax  R  array of cell width for each row
c   deltay       R  array of cell width for all columns
c
      real*4 cellon(MXCELX,MXCELY)
      real*4 cellat(MXCELX,MXCELY)
      real*4 deltax(MXCELY)
      real*4 deltay
c
      common /mrggrddat/ cellon, cellat, deltax, deltay
