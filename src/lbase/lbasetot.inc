c*** LBASETOT.INC 
c
c-----------------------------------------------------------------------
c
c    Include file for emissions totals for LBASE program
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Parameter for array bounds:
c-----------------------------------------------------------------------
c
c   MXASCL  I  maximum number of ASC codes in inventory
c   MXLCEL  I  maximum number of grid cells in any given county
c   MXFIPCL I  maxumum number of counties with output cells
c
      integer*4 MXASCL
      integer*4 MXLCEL
      integer*4 MXFIPCL
c
      parameter (MXASCL  =   30 )
      parameter (MXLCEL  =   20 )
      parameter (MXFIPCL =    5 )
c
c-----------------------------------------------------------------------
c   Parameters for index in totals array:
c-----------------------------------------------------------------------
c
c   IDXSKP   I   index of total emissions skipped
c   IDXIN    I   index of total input emissions
c   IDXOUT   I   index of total output emissions
c
      integer*4 IDXSKP
      integer*4 IDXIN
      integer*4 IDXOUT
c
      parameter( IDXIN  = 1 )
      parameter( IDXOUT = 2 )
      parameter( IDXSKP = 3 )
c
c-----------------------------------------------------------------------
c   Variables for totals of emissions:
c-----------------------------------------------------------------------
c
c   totems   R   array for emissions of criteria pollutants
c
      real*8 totems(IDXSKP,MXCRIT)
c
      common /lbstot/ totems
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
c   Variables for keeping the last record's data:
c-----------------------------------------------------------------------
c
c   fiplast   C  last FIPS code 
c   xbeglast  R  beginning X-coordinate of last link segment
c   ybeglast  R  beginning Y-coordinate of last link segment
c   xendlast  R  ending X-coordinate of last link segment
c   yendlast  R  ending Y-coordinate of last link segment
c   idxclast  I  indexes into global array for the last link processed 
c   ibegdlast I  last beginning date/time stamp
c   ienddlast I  last ending date/time stamp
c
      character*5 fiplast
      integer*4   idxclast(MXLCEL)
      real*4      xbeglast
      real*4      ybeglast
      real*4      xendlast
      real*4      yendlast
      integer*4   ibeglast
      integer*4   iendlast
c
      common /lstchr/ fiplast
      common /lstdat/ xbeglast, ybeglast, xendlast, yendlast, idxclast,
     &                ibeglast, iendlast
