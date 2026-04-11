c*** SPLTEMTOT.INC
c
c-----------------------------------------------------------------------
c
c    include file for emissions totals for SPLTEM program
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c    08/13/97   -mmj-   original development
c
c-----------------------------------------------------------------------
c   parameters for index in totals array:
c-----------------------------------------------------------------------
c
c   IDXDAT   i   index of total lowlevel emissions
c   IDXELE   i   index of elevated emissions
c   IDXIN    i   index of total input emissions
c   IDXOUT   i   index of total output emissions
c   IDXRNG   i   index of total emissions outside region of interest
c   IDXSKP   i   index of total emissions skipped
c
      integer*4 IDXELE
      integer*4 IDXDAT
      integer*4 IDXIN
      integer*4 IDXOUT
      integer*4 IDXSKP
      integer*4 IDXRNG
c
      parameter( IDXIN  = 1 )
      parameter( IDXOUT = 2 )
      parameter( IDXDAT = 3 )
      parameter( IDXELE = 4 )
      parameter( IDXSKP = 5 )
      parameter( IDXRNG = 6 )
c
c-----------------------------------------------------------------------
c   variables for totals of emissions:
c-----------------------------------------------------------------------
c
c   totems   r   array for emissions of criteria pollutants (gm)
c   totprf   r   array for emissions by profile for split criteria
c
      real*4 totems(IDXRNG,MXCRIT)
      real*4 totprf(MXPRF)
c
      common /ttl/ totems, totprf
c
c-----------------------------------------------------------------------
c   record processing totals:
c-----------------------------------------------------------------------
c
c   nrec     i   number of output records written
c
      integer*4   nrec(IDXRNG)
c
      common /trec/ nrec
c
