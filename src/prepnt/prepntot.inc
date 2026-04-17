c*** PREPNTOT.INC 
c
c-----------------------------------------------------------------------
c
c    Include file for emissions totals for PREPNT program
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Parameters for index in totals array:
c-----------------------------------------------------------------------
c
c   IDXELE   I   index of total elevated output emissions
c   IDXERR   I   index of total emissions written to EMAR file
c   IDXIN    I   index of total input emissions
c   IDXLOW   I   index of total elevated output emissions
c   IDXOUT   I   index of total output emissions
c   IDXREG   I   index of total emissions outside region
c   IDXSKP   I   index of total emissions skipped
c
      integer*4 IDXELE
      integer*4 IDXERR
      integer*4 IDXIN
      integer*4 IDXLOW
      integer*4 IDXOUT
      integer*4 IDXREG
      integer*4 IDXSKP
c
      parameter( IDXIN  = 1 )
      parameter( IDXOUT = 2 )
      parameter( IDXELE = 3 )
      parameter( IDXLOW = 4 )
      parameter( IDXREG = 5 )
      parameter( IDXERR = 6 )
      parameter( IDXSKP = 7 )
c
c-----------------------------------------------------------------------
c   Variables for totals of emissions:
c-----------------------------------------------------------------------
c
c   totems   R   array for emissions of criteria pollutants (gm)
c
      real*8 totems(IDXSKP,MXCRIT)
c
      common /prptl/ totems
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
