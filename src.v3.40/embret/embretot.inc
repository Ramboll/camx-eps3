c*** EMRETOT.INC 
c
c-----------------------------------------------------------------------
c
c    Include file for emissions totals for EMBRET program
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
c   totems   R   array for emissions of criteria pollutants (gm)
c   totspc   R   array for speciated emissions (moles)
c
      real*8 totems(IDXOUT,MXCRIT)
      real*8 totspc(IDXOUT,MXSPEC)
c
      common /ttl/ totems, totspc
c
c-----------------------------------------------------------------------
c   Record processing totals:
c-----------------------------------------------------------------------
c
c   nrec     I   number of output records written
c
      integer*4   nrec(IDXOUT)
c
      common /trec/ nrec
