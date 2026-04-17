c*** PARAM.INC 
c
c-----------------------------------------------------------------------
c
c    Include file for parameters used by the EPS modules.
c
c-----------------------------------------------------------------------
c   Log:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Standard I/O unit number parameters:
c-----------------------------------------------------------------------
c
c   IORSTD   I   unit number for standard input
c   IOWSTD   I   unit number for standard output
c
      integer*4 IORSTD
      integer*4 IOWSTD
c
      parameter ( IORSTD =  5 )
      parameter ( IOWSTD =  6 )
c
c-----------------------------------------------------------------------
c    Error flag parameters:
c-----------------------------------------------------------------------
c
c   IEOD     I   end of data occured on read
c   IEOF     I   end of file encountered on read
c   IFAIL    I   error encountered, leads to termination
c   IRDERR   I   error occurred on formatted read
c   ISKIP    I   data insuffucient for processing, skip and continue
c   ISUCES   I   no error occurred
c   IWRERR   I   error occurred on formatted write

      integer*4 IEOD
      integer*4 IEOF
      integer*4 IFAIL
      integer*4 IRDERR
      integer*4 ISKIP
      integer*4 ISUCES
      integer*4 IWRERR
c
      parameter ( ISKIP  =  1 )
      parameter ( ISUCES =  0 )
      parameter ( IFAIL  = -1 )
      parameter ( IEOD   = -2 )
      parameter ( IEOF   = -3 )
      parameter ( IRDERR = -4 )
      parameter ( IWRERR = -5 )
c
c-----------------------------------------------------------------------
c   Array bounds parameters:
c-----------------------------------------------------------------------
c
c   MXCELX   I   maximum number of grid cell in X direction
c   MXCELY   I   maximum number of grid cell in Y direction
c   MXLAYER  I   maximum number of layers
c   MXCRIT   I   maximum number of criteria pollutants
c   MXDSG    I   maximum number of records in /DISAG/ packet
c   MXFIP    I   maximum number of counties in region
c   MXHOUR   I   maximum number of hours
c   MXLKCL   I   maxumum number of cells containing a partial link
c   MXSPEC   I   maximum number of species
c   MXSTR    I   maximum length of character string for filenames
c   LENREC   I   length of header/trailer records in RMBR file
c   LENPRF   I   parameter for lenght of profile code
c   MXVERT   I   maximum number of vertices for SHIPPING lane polygons
c
      integer*4 MXCELX
      integer*4 MXCELY
      integer*4 MXLAYER
      integer*4 MXCRIT
      integer*4 MXDSG
      integer*4 MXFIP
      integer*4 MXHOUR
      integer*4 MXLKCL
      integer*4 MXSPEC
      integer*4 MXSTR
      integer*4 LENREC
      integer*4 LENPRF
      integer*4 MXVERT
c
      parameter ( MXCELX  =  200 )
      parameter ( MXCELY  =  200 )
      parameter ( MXLAYER =    1 )
      parameter ( MXCRIT  =   10 )
      parameter ( MXDSG   =   10 )
      parameter ( MXFIP   =  100 )
      parameter ( MXHOUR  =   24 )
      parameter ( MXLKCL  =    1 )
      parameter ( MXSPEC  =   40 )
      parameter ( MXSTR   = 1000 )
      parameter ( LENREC  = 1000 )
      parameter ( LENPRF  =   14 )
      parameter ( MXVERT  =    4 )
c
c-----------------------------------------------------------------------
c   Parameters for USERIN file identifiers:
c-----------------------------------------------------------------------
c
c   KEYEND   C   character string for packet terminator
c   KEYLEN   I   length of paccket keyword identifier strings
c
      integer*4    KEYLEN
c
      parameter ( KEYLEN = 20 )
c
      character*(KEYLEN) KEYEND
c
      parameter ( KEYEND = '/END/' )
c
c-----------------------------------------------------------------------
c   Parameters for missing value:
c-----------------------------------------------------------------------
c
c   IMISS    I   missing value for integers
c   RMISS    R   missing value for reals
c
       integer*4 IMISS
       real*4    RMISS
c
       parameter ( IMISS =   -9 )
       parameter ( RMISS = -9.0 )
c
c-----------------------------------------------------------------------
c   Parameters for decision making:
c-----------------------------------------------------------------------
c
c   NO       C   character string for checking user response
c   YES      C   character string for checking user response
c
      character*3 NO
      character*3 YES
c
      parameter ( NO  = 'NO ' )
      parameter ( YES = 'YES' )
c
c-----------------------------------------------------------------------
c    Parameter for conversion of grams to tons:
c-----------------------------------------------------------------------
c
c   TONLBL  C     label for type of tons used
c   CVTTON  R     conversion factor for grams to tons (English)
c
      character*12 TONLBL
      real*4       CVTTON
c
      parameter( TONLBL = 'English Tons' )
      parameter( CVTTON = 1/907184.74 )
c
c-----------------------------------------------------------------------
c   Parameters for period code:
c-----------------------------------------------------------------------
c
c      ANTYP  C   annual
c      PCTYP  C   peak CO typical day
c      POTYP  C   peak ozone typical day
c      ADTYP  C   average day
c      SPTYP  C   specified interval
c
      character*2 ANTYP
      character*2 PCTYP
      character*2 POTYP
      character*2 ADTYP
      character*2 SPTYP
c
      parameter (ANTYP = '  ')
      parameter (PCTYP = 'PC')
      parameter (POTYP = 'PO')
      parameter (ADTYP = 'AD')
      parameter (SPTYP = 'S ')
c
c-----------------------------------------------------------------------
c   Parameters indexing the timezones
c-----------------------------------------------------------------------
c
c   IDXPST  I  Pacific Standard Time
c   IDXMST  I  Mountain Standard Time
c   IDXCST  I  Central Standard Time
c   IDXEST  I  Eastern Standard Time
c   IDXPDT  I  Pacific Daylight Time
c   IDXMDT  I  Mountain Daylight Time
c   IDXCDT  I  Central Daylight Time
c   IDXEDT  I  Eastern Daylight Time
c
      integer*4 IDXPST
      integer*4 IDXMST
      integer*4 IDXCST
      integer*4 IDXEST
      integer*4 IDXPDT
      integer*4 IDXMDT
      integer*4 IDXCDT
      integer*4 IDXEDT
c
      parameter( IDXPST = -8 )
      parameter( IDXMST = -7 )
      parameter( IDXCST = -6 )
      parameter( IDXEST = -5 )
      parameter( IDXPDT = -7 )
      parameter( IDXMDT = -6 )
      parameter( IDXCDT = -5 )
      parameter( IDXEDT = -4 )
c
c   PST     C     character string for PST
c   MST     C     character string for MST
c   CST     C     character string for CST
c   EST     C     character string for EST
c   PDT     C     character string for PST
c   MDT     C     character string for MST
c   CDT     C     character string for CST
c   EDT     C     character string for EST
c
      character*3 PST
      character*3 MST
      character*3 CST
      character*3 EST
      character*3 PDT
      character*3 MDT
      character*3 CDT
      character*3 EDT
c
      parameter( PST = 'PST' )
      parameter( MST = 'MST' )
      parameter( CST = 'CST' )
      parameter( EST = 'EST' )
      parameter( PDT = 'PDT' )
      parameter( MDT = 'MDT' )
      parameter( CDT = 'CDT' )
      parameter( EDT = 'EDT' )
c
c-----------------------------------------------------------------------
c   I/O unit and warning count variables:
c-----------------------------------------------------------------------
c
c   ioerr    I   unit number of the file to write errors
c    nwarn   I   number of warnings written during execution
c
      integer*4 ioerr
      integer*4 nwarn
c
      common /curdat/ ioerr, nwarn
c-----------------------------------------------------------------------
c   Parameters for default values for projection:
c-----------------------------------------------------------------------
c
c   DEF_POLELON  R  default value for longitude projection pole
c   DEF_POLELAT  R  default value for latitude projection pole
c   DEF_TLAT1    R  default value for 1st true latitude
c   DEF_TLAT2    R  default value for 2nd true latitude
c
      real*4 DEF_POLELON
      real*4 DEF_POLELAT
      real*4 DEF_TLAT1
      real*4 DEF_TLAT2
c
      parameter( DEF_POLELON = -97.0 )
      parameter( DEF_POLELAT =  40.0 )
      parameter( DEF_TLAT1   =  45.0 )
      parameter( DEF_TLAT2   =  33.0 )
