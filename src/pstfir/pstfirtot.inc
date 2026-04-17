c*** PSTFIRTOT.INC 
c
c-----------------------------------------------------------------------
c
c    Include file for emissions totals for PSTFIR program
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Parameters for index in totals array:
c-----------------------------------------------------------------------
c
c   IDXERR   I   index of total emissions written to EMAR file
c   IDXELV   I   index of total computed output elevated emissions
c   IDXIN    I   index of total input emissions
c   IDXOUT   I   index of total output emissions
c   IDXREG   I   index of total emissions outside region
c   IDXSKP   I   index of total emissions skipped
c   IDXSRF   I   index of total computed output surface emissions
c
      integer*4 IDXERR
      integer*4 IDXELV
      integer*4 IDXIN
      integer*4 IDXOUT
      integer*4 IDXREG
      integer*4 IDXSKP
      integer*4 IDXSRF
c
      parameter( IDXIN  = 1 )
      parameter( IDXOUT = 2 )
      parameter( IDXELV = 3 )
      parameter( IDXSRF = 4 )
      parameter( IDXREG = 5 )
      parameter( IDXERR = 6 )
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
      common /firtl/ totems, totspc
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
c   Gridded array (surface emissions):
c-----------------------------------------------------------------------
c
c   emssrf  R  large array for storing surface gridded emissions
c
      real*4 emssrf(MXHOUR,MXSPEC,MXCELY,MXCELX)
c
      common /emsdat/ emssrf
c
c-----------------------------------------------------------------------
c   Gridded array (surface emissions):
c-----------------------------------------------------------------------
c
c   MX3DCELLS I  maxumum number of 3D cells that contain emissions
c
      integer*4 MX3DCELLS
c
      parameter( MX3DCELLS = 100000 )
c
c   ijcell3d     I  array of indexes into array of gridded emissions values
c   num_ijcell3d I  number of cells in gridded emissions array
c   emiss3d      R  large array for storing 3D gridded emissions
c   cellon       R  array for gridded longitude values
c   cellat       R  array for gridded latitude values
c   deltax       R  array of cell width for each row
c   deltay       R  array of cell width for all columns
c   lusehr       L  .TRUE. if the hour is used anywhere
c
      integer*4 ijcell3d(MXCELX,MXCELY,MXLAYER)
      integer*4 num_ijcell3d
c
      real*4    emiss3d(MXHOUR,MXSPEC,MX3DCELLS)
      real*4    cellon(MXCELX,MXCELY)
      real*4    cellat(MXCELX,MXCELY)
      real*4    deltax(MXCELY)
      real*4    deltay
      logical*4 lusehr(MXHOUR)
c
      common /emsdat/ ijcell3d, num_ijcell3d, emiss3d, 
     &                cellon, cellat, deltax, deltay, lusehr
