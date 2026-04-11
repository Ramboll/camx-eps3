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
c   spcnam_pkt  I   names of species in /SPECIES LIST/ packet
c   nspecs_pkt  I   number of species in /SPECIES LIST/ packet
c   totspc      R   array for total input emissions (English tons)
c   molwt_out   R   array of molecular weight for output species
c   lgas_out    L   .TRUE. if species is a gas species
c   lspec_pkt   L   .TRUE. if /SPECIES LIST/ packet found
c   npts_out    I   number of point sources addes to output cellls 
c
      character*10 spcnam_pkt(MXSPEC)
      integer*4    nspecs_pkt
      integer*4    npts_out
      real*8       totspc(IDXOUT,MXSPEC)
      real*4       molwt_out(MXSPEC)
      logical*4    lgas_out(MXSPEC)
      logical*4    lspec_pkt
c
      common /ttl_chr/ spcnam_pkt
      common /ttl_dat/ nspecs_pkt, totspc, molwt_out, 
     &                 lgas_out, lspec_pkt, npts_out
c
c-----------------------------------------------------------------------
c   Gridded arrays for metadata:
c-----------------------------------------------------------------------
c
c   cellon  R  array for gridded longitude values
c   cellat  R  array for gridded latitude values
c   deltax  R  array of cell width for each row
c   deltay  R  array of cell width for all columns
c
      real*4  cellon(MXCELX,MXCELY)
      real*4  cellat(MXCELX,MXCELY)
      real*4  deltax(MXCELY)
      real*4  deltay
c
      common /grddat/cellon, cellat, deltax, deltay
c
c-----------------------------------------------------------------------
c   Parameters for point source arrays:
c-----------------------------------------------------------------------
c
c   MXPTSRCE I  maximum number of point sources
c
      integer*4 MXPTSRCE
c
      parameter( MXPTSRCE = 2000000 )
c 
c-----------------------------------------------------------------------
c   Varaibles for point source arrays:
c-----------------------------------------------------------------------
c
c   nptsrce I  number of point sources in input file
c   xlocpt  R  location of point source in X direction
c   ylocpt  R  location of point source in Y direction
c
      integer*4 nptsrce 
      real*4    xlocpt(MXPTSRCE)
      real*4    ylocpt(MXPTSRCE)
c
      common /ptslocdat/ nptsrce, xlocpt, ylocpt 
