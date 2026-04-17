c*** TMPRLPRF.INC 
c
c-----------------------------------------------------------------------
c
c    Include file for temporal profile data used by the TMPRL program
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Parameters for cross reference data:
c-----------------------------------------------------------------------
c
c   MXREF    I   parameter for array bounds
c
      integer*4 MXREF
c
      parameter( MXREF = 120000 )
c
c-----------------------------------------------------------------------
c   Cross reference data variables:
c-----------------------------------------------------------------------
c
c   facxrf   C   array of NEDS plant identification code
c   fipxrf   C   array of FIPS codes
c   stkxrf   C   array of stack identification code
c   pntxrf   C   array of point identification code
c   idyxrf   I   array of diurnal temporal profile codes
c   imnxrf   I   array of monthly temporal profile codes
c   iwkxrf   I   array of weekly temporal profile codes
c   npfxrf   I   number of cross reference records
c   sccxrf   C   array of ASC-SCC codes
c
      character*10 sccxrf(MXREF)
      character*10 fipxrf(MXREF)
      character*10 facxrf(MXREF)
      character*10 stkxrf(MXREF)
      character*10 pntxrf(MXREF)
      integer*4    idyxrf(MXREF,MXCRIT)
      integer*4    imnxrf(MXREF,MXCRIT)
      integer*4    iwkxrf(MXREF,MXCRIT)
      integer*4    npfxrf
c
      common /txfchr/ fipxrf, facxrf, sccxrf, stkxrf, pntxrf
      common /txfdat/ npfxrf, imnxrf, iwkxrf, idyxrf
c
c-----------------------------------------------------------------------
c    Variables for hash table arrays:
c-----------------------------------------------------------------------
c
c   idxhsh_tpl_first_pair I index of first record for this hash value
c   idxhsh_tpl_next_pair  I index of next record in the hash table
c
       integer*4 idxhsh_tpl_first_pair(MXHASH_PAIR)
       integer*4 idxhsh_tpl_next_pair(MXREF)
c
      common /datxrfhash/ idxhsh_tpl_first_pair, idxhsh_tpl_next_pair
c
c-----------------------------------------------------------------------
c   Parameters for temporal profiles data:
c-----------------------------------------------------------------------
c
c   IDXEND   I   parameter for index of weekend profiles in duurnal
c                profile arrays
c   IDXMID   I   parameter for index of mid-week profiles in duurnal
c                profile arrays
c   MXDAY    I   parameter for maximum number of hours of day
c   MXMON    I   parameter for maximum number of months of year
c   MXPRF    I   parameter for maximum number of profile codes
c   MXWEK    I   parameter for maximum number of days of week
c
      integer*4 IDXEND
      integer*4 IDXMID
      integer*4 MXDAY
      integer*4 MXMON
      integer*4 MXPRF
      integer*4 MXWEK
c
      parameter( IDXEND =   2 )
      parameter( IDXMID =   1 )
      parameter( MXDAY  =  24 )
      parameter( MXMON  =  12 )
      parameter( MXPRF  =16700)
      parameter( MXWEK  =   7 )
c
c-----------------------------------------------------------------------
c   Temporal profiles data variables:
c-----------------------------------------------------------------------
c
c   idycd    I   array of temporal codes for hour of day profile
c                (index MXDAY+1 is total of all weights)
c   idyfac   I   array for temporal weights for diurnal profile codes
c   imncd    I   array of monthly profile codes
c                (index MXMON+1 is total of all weights)
c   imnfac   I   array for temporal weights for monthly profile
c   iwkcd    I   array of day of week profile code
c                (index MXWEK+1 is total of all weights)
c   iwkfac   I   array for temproal weights for weekly profile codes
c   ndycd    I   actual number of diurnal profile codes
c   nmncd    I   actual number of monthly profile codes
c   nwkcd    I   actual number of weekly profile codes
c
      integer*4 idycd(2,MXPRF)
      integer*4 idyfac(2,MXPRF,MXDAY+1)
      integer*4 imncd(MXPRF)
      integer*4 imnfac(MXPRF,MXMON+1)
      integer*4 iwkcd(MXPRF)
      integer*4 iwkfac(MXPRF,MXWEK+1)
      integer*4 ndycd(2)
      integer*4 nmncd
      integer*4 nwkcd
c
      common /prfdat/ imncd, nmncd, imnfac, iwkcd, nwkcd, iwkfac,
     &                idycd, ndycd, idyfac
