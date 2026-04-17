c*** TOTCHM.INC 
c
c-----------------------------------------------------------------------
c
c    Include file for totals for the CHMSPL program
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Parameters for index in totals array:
c-----------------------------------------------------------------------
c
c   IDXIN    i   index of total input emissions
c   IDXERR   i   index of total emissions written to EMAR file
c   IDXOUT   i   index of total output emissions
c   IDXSPC   i   index of total output criteria-speciated emissions
c   IDXSKP   i   index of total emissions skipped
c
      integer*4 IDXIN
      integer*4 IDXERR
      integer*4 IDXOUT
      integer*4 IDXSPC
      integer*4 IDXSKP
c
      parameter( IDXIN  = 1 )
      parameter( IDXOUT = 2 )
      parameter( IDXSPC = 3 )
      parameter( IDXSKP = 4 )
      parameter( IDXERR = 5 )
c
c-----------------------------------------------------------------------
c   Arrays:
c-----------------------------------------------------------------------
c
c   totems   R   array for emissions of criteria pollutants
c   totpfc   R   array of emissions totals by profile code for criteria
c   totpfs   R   array of emissions totals by profile code for cbiv
c
      real*8 totems(IDXERR,MXCRIT)
      real*4 totpfc(IDXOUT,MXSPL,MXCRIT)
      real*4 totpfs(MXSPL,MXSPEC)
c
      common /totdat/ totems, totpfc, totpfs
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
