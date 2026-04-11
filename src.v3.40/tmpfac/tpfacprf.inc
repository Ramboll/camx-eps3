c*** TPFACPRF.INC 
c
c-----------------------------------------------------------------------
c
c    Include file for temporal profile data used by the TMPFAC program
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Parameters for cross reference data:
c-----------------------------------------------------------------------
c
c   MXPOL    I   parameter for number of pollutants (not same as MXCRIT)
c   MXREF    I   parameter for array bounds
c   BFUZZ    R   fuzz factor for comparing real numbers read from
c
      integer*4 MXPOL
      integer*4 MXREF
      real*4 BFUZZ
c
      parameter( MXPOL = 10 )
      parameter( MXREF = 135000 )
      parameter( BFUZZ = 0.00005 )
c
c-----------------------------------------------------------------------
c   Pollutant code data:
c-----------------------------------------------------------------------
c
c   ipolcd   I   array of pollutant codes.
c   ncdpol   I   number of pollutant codes
c
      integer*4 ipolcd(MXPOL)
      integer*4 ncdpol
c
      common /poldat/ ipolcd, ncdpol
c
c-----------------------------------------------------------------------
c   Cross reference data variables:
c-----------------------------------------------------------------------
c
c   facxrf   C   array NEDS plant identification code
c   fipxrf   C   array of FIPS codes
c   stkxrf   C   array stack identification code
c   idyxrf   I   array of diurnal temporal profile codes
c   imnxrf   I   array of monthly temporal profile codes
c   iwkxrf   I   array of weekly temporal profile codes
c   npfxrf   I   number of cross refernce records
c   pntxrf   C   array of point identification codes
c   sccxrf   C   array of ASC-SCC codes
c   lglob    L   flag that tells if record is global pollutant
c
      character*10 sccxrf(MXREF)
      character*10 fipxrf(MXREF)
      character*10 facxrf(MXREF)
      character*10 pntxrf(MXREF)
      character*10 stkxrf(MXREF)
      integer*4    idyxrf(MXREF,MXPOL)
      integer*4    imnxrf(MXREF,MXPOL)
      integer*4    iwkxrf(MXREF,MXPOL)
      integer*4    npfxrf
      logical*4    lglob(MXREF,MXPOL)
c
      common /fxfchr/ fipxrf, facxrf, stkxrf, sccxrf, pntxrf
      common /frfdat/ npfxrf, imnxrf, iwkxrf, idyxrf, lglob
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
      parameter( MXPRF  = 8000)
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
c   imxday   I   largest of the diurnal profile codes
c   imxmon   I   largest of the monthly profile codes
c   imxwek   I   largest of the weekly profile codes
c   iwkcd    I   array of day of week profile code
c                (index MXWEK+1 is total of all weights)
c   iwkfac   I   array for temproal weights for weekly profile codes
c   ndycd    I   actual number of diurnal profile codes
c   nmncd    I   actual number of monthly profile codes
c   nwkcd    I   actual number of weekly profile codes
c   pctday   R   array of weight percent for diurnal profiles
c   pctmon   R   array of weight percent for monthly profiles
c   pctwek   R   array of weight percent for weekly profiles
c
      integer*4 idycd(2,MXPRF)
      integer*4 idyfac(2,MXPRF,MXDAY+1)
      integer*4 imncd(MXPRF)
      integer*4 imnfac(MXPRF,MXMON+1)
      integer*4 imxday(2)
      integer*4 imxmon
      integer*4 imxwek
      integer*4 iwkcd(MXPRF)
      integer*4 iwkfac(MXPRF,MXWEK+1)
      integer*4 ndycd(2)
      integer*4 nmncd
      integer*4 nwkcd
      real*4    pctday(2,MXPRF,MXDAY)
      real*4    pctmon(MXPRF,MXMON)
      real*4    pctwek(MXPRF,MXWEK)
c
      common /prfdat/ imncd, nmncd, imxmon, imnfac, iwkcd, nwkcd,
     &                iwkfac, imxwek, idycd, ndycd, imxday, idyfac,
     &                pctmon, pctwek, pctday
