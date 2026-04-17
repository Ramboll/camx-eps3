c*** PIGEMSTOT.INC version 2.0 09/01/2007
c
c-----------------------------------------------------------------------
c
c    Include file for the totals that will be output to the PIGEMS
c    message file.
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Parameters for totals arrays:
c-----------------------------------------------------------------------
c
c   ICDSTK    I  index for total number of stacks
c   ICDPIG    I  index for number of stacks that were pigged
c   ICDCOL    I  index for number of stacks that were colocated
c   ICDCOM    I  index for number of stacks that were combined
c   ICDMSK    I  index for number of stacks that fell under mask file
c
      integer*4 ICDSTK
      integer*4 ICDPIG
      integer*4 ICDCOL
      integer*4 ICDCOM
      integer*4 ICDMSK
c
      parameter( ICDSTK = 1 )
      parameter( ICDPIG = 2 )
      parameter( ICDCOL = 3 )
      parameter( ICDCOM = 4 )
      parameter( ICDMSK = 5 )
c
c-----------------------------------------------------------------------
c    Variables for storing stack count totals:
c-----------------------------------------------------------------------
c
c   ntotstk  I   stack counts by file
c
      integer*4 ntotstk(MXSTKFL,ICDMSK)
c
      common /stkcnt/ ntotstk
c
c-----------------------------------------------------------------------
c    Variables for storing stack count totals:
c-----------------------------------------------------------------------
c
c   emstot   R   emissions totals by file
c   cellon  R  array for gridded longitude values
c   cellat  R  array for gridded latitude values
c   deltax  R  array of cell width for each row
c   deltay       R  array of cell width for all columns
c
      real*8 emstot(MXEMSFL+1,MXCRIT)
      real*4 cellon(MXCELX,MXCELY)
      real*4 cellat(MXCELX,MXCELY)
      real*4 deltax(MXCELY)
      real*4 deltay
c
      common /stkcnt/ emstot, cellon, cellat, deltax, deltay
