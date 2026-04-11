c*** MSGTOT.INC 
c
c-----------------------------------------------------------------------
c
c   Variables and common blocks for the tables of processed emissions.
c
c-----------------------------------------------------------------------
c   Log:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Paarameters for indexing global arrays:
c-----------------------------------------------------------------------
c
c   ICDIN  I  code that indicates values to be stored in input arrays
c   ICDOUT I  code that indicates values to be stored in output arrays
c   MXASC  I  maximum number of source category codes (ASC or SCC)
c   MXFCD  I  maximum number of FIPS/source category pairs
c   MXPID  I  maximum number of point source IDs
c
      integer*4 ICDIN
      integer*4 ICDOUT
      integer*4 MXASC
      integer*4 MXFCD
      integer*4 MXPID
c
       parameter( ICDIN  =      1 )
       parameter( ICDOUT =      2 )
       parameter( MXASC  =    100 )
       parameter( MXFCD  =   1000 )
       parameter( MXPID  =   1000 )
c
c-----------------------------------------------------------------------
c   Global arrays:
c-----------------------------------------------------------------------
c
c   crticty  R  total input criteria by county
c   crtocty  R  total output criteria by county
c
c   crtiasc  R  total input criteria by source category
c   crtoasc  R  total output criteria by source category
c
c   crtihrs  R  total input criteria by hour
c   crtohrs  R  total output criteria by hour
c
c   crtishr  R  total input criteria by source category/hour
c   crtoshr  R  total output criteria by source category/hour
c
c   crtifcd  R  total input criteria by FIPS/soure cat pair
c   crtofcd  R  total output criteria by FIPS/soure cat pair
c
c   spcicty  R  total input speciated by county
c   spcocty  R  total output speciated by county
c
c   spciasc  R  total input speciated by source category
c   spcoasc  R  total output speciated by source category
c
c   spcihrs  R  total input speciated by hour
c   spcohrs  R  total output speciated by hour
c
c   spcishr  R  total input speciated by source category/hour
c   spcoshr  R  total output criteria by source category/hour
c
c   spcifcd  R  total input speciated by FIPS/soure cat pair
c   spcofcd  R  total output speciated by FIPS/soure cat pair
c
      real*4 crticty(MXFIP+1,MXCRIT)
      real*4 crtocty(MXFIP+1,MXCRIT)
c
      real*4 crtiasc(MXASC,MXCRIT)
      real*4 crtoasc(MXASC,MXCRIT)
c
      real*4 crtihrs(MXHOUR,MXCRIT)
      real*4 crtohrs(MXHOUR,MXCRIT)
c
      real*4 crtihcd(MXASC,MXHOUR,MXCRIT)
      real*4 crtohcd(MXASC,MXHOUR,MXCRIT)
c
      real*4 crtifcd(MXFCD,MXCRIT)
      real*4 crtofcd(MXFCD,MXCRIT)
c
      real*4 crtipid(MXPID,MXCRIT)
      real*4 crtopid(MXPID,MXCRIT)
c
      real*4 spcicty(MXFIP+1,MXSPEC)
      real*4 spcocty(MXFIP+1,MXSPEC)
c
      real*4 spciasc(MXASC,MXSPEC)
      real*4 spcoasc(MXASC,MXSPEC)
c
      real*4 spcihrs(MXHOUR,MXSPEC)
      real*4 spcohrs(MXHOUR,MXSPEC)
c
      real*4 spcihcd(MXASC,MXHOUR,MXSPEC)
      real*4 spcohcd(MXASC,MXHOUR,MXSPEC)
c
      real*4 spcifcd(MXFCD,MXSPEC)
      real*4 spcofcd(MXFCD,MXSPEC)
c
      real*4 spcipid(MXPID,MXSPEC)
      real*4 spcopid(MXPID,MXSPEC)
c
      common /totvara/ crticty
      common /totvarb/ crtocty
      common /totvarc/ crtiasc
      common /totvard/ crtoasc
      common /totvare/ crtihrs
      common /totvarf/ crtohrs
      common /totvarh/ crtihcd
      common /totvarj/ crtohcd
      common /totvari/ crtifcd
      common /totvark/ crtofcd
      common /totvarl/ crtipid
      common /totvarm/ crtopid
      common /totvarn/ spcicty
      common /totvaro/ spcocty
      common /totvarp/ spciasc
      common /totvarq/ spcoasc
      common /totvarr/ spcihrs
      common /totvars/ spcohrs
      common /totvart/ spcihcd
      common /totvaru/ spcohcd
      common /totvarv/ spcifcd
      common /totvarw/ spcopid
      common /totvarx/ spcipid
      common /totvara/ spcofcd
c
c-----------------------------------------------------------------------
c   Arrays for ASC codes:
c-----------------------------------------------------------------------
c
c   msgasc      A   array of source category codes
c   last_msgasc A   source category code from the last record
c   idx_lastasc I   index of source category code from the last record
c   msgfcd      A   array of FIPS/source category pairs
c   last_msgfcd A   FIPS/source category pair from the last record
c   idx_lastfcd I   index of FIPS/source category pair from the last record
c   msgpid      A   array of point source ids (FIPS/Facility/Stack/Point)
c   nmsgasc     I   number of ASC codes in the array
c   nmsgfcd     I   number of FIPS/source cat pairs in the array
c   nmsgpid     I   number of point source ids in the array
c
      character*35 msgpid(MXPID)
      character*16 msgfcd(MXFCD)
      character*16 last_msgfcd
      character*10 msgasc(MXASC)
      character*10 last_msgasc
      integer*4    idx_lastasc
      integer*4    idx_lastfcd
      integer*4    nmsgasc
      integer*4    nmsgfcd
      integer*4    nmsgpid
c
      common /ascchr/ msgasc, msgfcd, msgpid, last_msgasc, last_msgfcd
      common /ascnum/ nmsgasc, nmsgfcd, nmsgpid, idx_lastasc, 
     &                idx_lastfcd
c
c-----------------------------------------------------------------------
c   Variables for index of FIPS/ASC pair:
c-----------------------------------------------------------------------
c
c   idxfcd   I   index into totals list for each county/ASC pair
c
      integer*4 idxfcd(MXFIP+1,MXASC)
c
      common /codidx/ idxfcd
c
c-----------------------------------------------------------------------
c   Flags to determine which output tables are written:
c-----------------------------------------------------------------------
c
c   NTABLS   I  parameter that specifies number of tables options
c   IDXFMT   I  index into array for formatted tables
c   IDXCSV   I  index into array for CSV tables
c
      integer*4 NTABLS
      integer*4 IDXFMT
      integer*4 IDXCSV
c
      parameter( NTABLS = 8 )
      parameter( IDXFMT = 1 )
      parameter( IDXCSV = 2 )
c
c If .TRUE. the flags will result in the following tables being written:
c
c   lcrtcty  L  table by county for criteria
c   lcrtasc  L  table by source category for criteria
c   lcrtfcd  L  table by county/source category for criteria
c   lcrthr   L  table by hour for criteria
c   lcrthcd  L  table by source category/hour for criteria
c   lcrtfac  L  table by county/facililty for criteria
c   lcrtstk  L  table by county/facility/stack for criteria
c   lcrtpnt  L  table by county/facility/stack/point for criteria
c   lcrtpid  L  any of the tables by point source id
c
c   lspccty  L  table by county for criteria
c   lspcasc  L  table by source category for criteria
c   lspcfcd  L  table by county/source category for criteria
c   lspchr   L  table by hour for criteria
c   lspchcd  L  table by source category/hour for criteria
c   lspcfac  L  table by county/facililty for criteria
c   lspcstk  L  table by county/facility/stack for criteria
c   lspcpnt  L  table by county/facility/stack/point for criteria
c   lspcpid  L  any of the tables by point source id
c
      logical*4 lcrtcty(IDXCSV)
      logical*4 lcrtasc(IDXCSV)
      logical*4 lcrtfcd(IDXCSV)
      logical*4 lcrthr(IDXCSV)
      logical*4 lcrthcd(IDXCSV)
      logical*4 lcrtfac(IDXCSV)
      logical*4 lcrtstk(IDXCSV)
      logical*4 lcrtpnt(IDXCSV)
      logical*4 lcrtpid
c
      logical*4 lspccty(IDXCSV)
      logical*4 lspcasc(IDXCSV)
      logical*4 lspcfcd(IDXCSV)
      logical*4 lspchr(IDXCSV)
      logical*4 lspchcd(IDXCSV)
      logical*4 lspcfac(IDXCSV)
      logical*4 lspcstk(IDXCSV)
      logical*4 lspcpnt(IDXCSV)
      logical*4 lspcpid
c
      common /flgtbl/ lcrtcty, lcrtasc, lcrtfcd, lcrthr, lcrthcd,
     &                lcrtfac, lcrtstk, lcrtpnt, lcrtpid,
     &                lspccty, lspcasc, lspcfcd, lspchr, lspchcd, 
     &                lspcfac, lspcstk, lspcpnt, lspcpid
c
c-----------------------------------------------------------------------
c   Arrays for hash table for various lookup for totals:
c----------------------------------------------------------------------
c
c   idxhsh_msgtot_next_asc I index of next record in the hash table
c   idxhsh_msgtot_next_pair I index of next record in the hash table
c
       integer*4 idxhsh_msgtot_next_asc(MXASC)
       integer*4 idxhsh_msgtot_next_pair(MXFCD)
c
       common /msghshdat/ idxhsh_msgtot_next_asc, 
     &                    idxhsh_msgtot_next_pair
c
c-----------------------------------------------------------------------
c   Variables for table headers:
c-----------------------------------------------------------------------
c
c   header  C  headers for tables (can vary by application)
c
      character*10 header(2,MXCRIT+MXSPEC)
c
      common /thdchr/ header
