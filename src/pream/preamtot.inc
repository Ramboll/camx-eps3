c*** PREAMTOT.INC 
c
c-----------------------------------------------------------------------
c
c    Include file for emissions totals for PREAM program
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
c   IDXIN    I   index of total input emissions
c   IDXOUT   I   index of total output emissions
c   IDXSKP   I   index of skipped emissions
c
      integer*4 IDXERR
      integer*4 IDXIN
      integer*4 IDXOUT
      integer*4 IDXSKP
c
      parameter( IDXIN  = 1 )
      parameter( IDXOUT = 2 )
      parameter( IDXSKP = 3 )
      parameter( IDXERR = 4 )
c
c-----------------------------------------------------------------------
c   Variables for totals of emissions:
c-----------------------------------------------------------------------
c
c   totems   R   array for emissions of criteria pollutants (gm)
c
      real*8    totems(IDXERR,MXCRIT)
c
      common /preatl/ totems
c
c-----------------------------------------------------------------------
c   Record processing totals:
c-----------------------------------------------------------------------
c
c   nrec     I   number of output records written
c
      integer*4   nrec(IDXERR)
c
      common /trec/ nrec
