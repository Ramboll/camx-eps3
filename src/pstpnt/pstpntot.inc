c*** PSTPNTOT.INC 
c
c-----------------------------------------------------------------------
c
c    Include file for emissions totals for PSTPNT program
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
c   IDXLOW   I   index of total low level emissions
c   IDXEMR   I   index of total emissions written to EMAR file
c   IDXSKP   I   index of total emissions skipped
c
      integer*4 IDXIN
      integer*4 IDXOUT
      integer*4 IDXLOW
      integer*4 IDXEMR
      integer*4 IDXSKP
c
      parameter( IDXIN  = 1 )
      parameter( IDXOUT = 2 )
      parameter( IDXLOW = 3 )
      parameter( IDXEMR = 4 )
      parameter( IDXSKP = 5 )
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
