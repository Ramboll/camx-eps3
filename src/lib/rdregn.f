C**** RDREGN.F
c    
      subroutine rdregn( ierr,
     &                         iounit)
c
c-----------------------------------------------------------------------
c
c    Reads the user input file and stores the data in the /UAMREGN/
c    packet in common variables
c
c    Argument declaration.
c     Outputs:
c       ierr   I error flag
c     Inputs:
c       iounit I logical unit number of file to read
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c    jan 2018 gwilson added variables for projection definition
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'uamregn.inc'
      include 'embrdat.inc'
c
c-----------------------------------------------------------------------
c    Argument declerations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
      integer*4 iounit
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
c   strmin   I   returns the actual length of a string
c
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
c   keywrd  C   keyword for packet as variable
c   string  C   character string for temporary storage
c   jerr    I   error flag for subroutine calls
c
      character*(KEYLEN) keywrd, keyin
      character*(MXSTR)  string
      integer*4          irec, jerr
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
      data keywrd /'/REGION/'/
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      irec = 0
c
c   --- call routine to get to find packet identifier ---
c
      call fndkey( jerr,
     &                   iounit, keywrd )
      if( jerr .EQ. IRDERR ) goto 9999
      if( jerr .NE. ISUCES ) goto 7000
c
c   --- read origin data (reading through unused data) ---
c
      irec = irec + 1
      read( iounit,'(A)',ERR=7001,END=7003 ) string
      read( string,'(20X,2F10.0)',ERR=7004,END=7003 ) xorig, yorig
      irec = irec + 1
      read( iounit,'(A)',ERR=7001,END=7003 ) string
      read( string,'(20X,I10)',ERR=7004,END=7003 ) izone
c
c   --- read grid cell data (checking for array overflow) ---
c
      irec = irec + 1
      read( iounit,'(A)',ERR=7001,END=7003 ) string
      read( string,'(20X,2F10.0)',ERR=7004,END=7003 ) dxcell, dycell
      irec = irec + 1
      read( iounit,'(A)',ERR=7001,END=7003 ) string
      read( string,'(20X,2I10)',ERR=7004,END=7003 ) nxcell, nycell
      if( nxcell .LE. 0 .OR. nxcell .GT. MXCELX .OR. nycell .LE. 0 .OR.
     &                                    nycell .GT. MXCELY ) goto 7002
c
c  --- set projection paramaters to defaults ---
c
      lambrt  = .TRUE.
      llatlon = .FALSE.
      lutm    = .FALSE.
      lrpolar = .FALSE.
      lpolar  = .FALSE.
      lmerc   = .FALSE.
      polelon = DEF_POLELON
      polelat = DEF_POLELAT
      tlat1   = DEF_TLAT1
      tlat2   = DEF_TLAT2
c
c   --- see if next record contains the projection type ---
c
      read( iounit,'(A)',ERR=7001,END=7003 ) string
      keyin = string(21:21+KEYLEN)
      call low2up( keyin )
      call lftjst( keyin )
      if( keyin .EQ. KEYEND ) goto 222
      lambrt = .FALSE.
      if( keyin(:3) .EQ. 'UTM' ) then
         lutm = .TRUE.
      else if( keyin(:6) .EQ. 'LATLON' ) then
         llatlon = .TRUE.
      else if( keyin(:6) .EQ. 'RPOLAR' ) then
         lrpolar = .TRUE.
      else if( keyin(:7) .EQ. 'LAMBERT' ) then
         lambrt = .TRUE.
      else if( keyin(:5) .EQ. 'POLAR' ) then
         lpolar = .TRUE.
      else if( keyin(:8) .EQ. 'MERCATOR' ) then
         lmerc = .TRUE.
      else
         goto 7005
      endif
c
c  ---- read the parameters for this type ---
c
      if( lrpolar .OR. lambrt .OR. lpolar .OR. lmerc ) then
         read( iounit,'(20X,F10.0)',ERR=7001,END=7003 ) polelon
         read( iounit,'(20X,F10.0)',ERR=7001,END=7003 ) polelat
      endif
      if( lambrt .OR. lpolar .OR. lmerc ) then
         read( iounit,'(20X,F10.0)',ERR=7001,END=7003 ) tlat1
         read( iounit,'(20X,F10.0)',ERR=7001,END=7003 ) tlat2
      endif
c
c  --- initialize the unused variables ---
c
 222  continue
      nzcell = 0
      nzlowr = 0
      nzuppr = 0
      htlowr = 0.
      htsur = 0.
      htuppr = 0.
      utmx = 0.
      utmy = 0.
c
c   --- set error flag to success ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,A)',ERR=9999) 'ERROR: Reading USERIN file.'
      write(ioerr,'(3A,//)',ERR=9999) 
     &         'This program requires the ',keywrd(:strmin(keywrd)),
     &                                             ' packet of USERIN.'
      goto 9999
c
 7001 continue
      write(ioerr,'(/,3A,I8,//)',ERR=9999) 
     &                    'ERROR: Reading ',keywrd(:strmin(keywrd)),
     &                          ' packet of USERIN file at line: ',irec
      goto 9999
c
 7002 continue
      write(ioerr,'(/,3A,I8)',ERR=9999) 'ERROR: Reading ',
     & keywrd(:strmin(keywrd)),' packet of USERIN file at line: ',irec
      write(ioerr,'(A)',ERR=9999) 'Number of grid cells exceeds max.'
      write(ioerr,9000,ERR=9999) 'Values in USERIN: ',nxcell, nycell
      write(ioerr,9000,ERR=9999) 'Maximum allowed : ',MXCELX, MXCELY
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameters MXCELX ',
     &                                     'and MXCELY and recompile.'
      goto 9999
c
 7003 continue
      write(ioerr,'(/,3A,I8)',ERR=9999) 'ERROR: Reading ',
     & keywrd(:strmin(keywrd)),' packet of USERIN file at line: ',irec
      write(ioerr,'(A,//)',ERR=9999) 'Premature end-of-file reached.'
      goto 9999
c
 7004 continue
      write(ioerr,'(/,3A,I8)',ERR=9999) 'ERROR: Reading ',
     & keywrd(:strmin(keywrd)),' packet of USERIN file at line: ',irec
      write(ioerr,'(A)',ERR=9999) 'Line read:'
      write(ioerr,'(A,//)',ERR=9999) string(:strmin(string))
      goto 9999
c
 7005 continue
      write(ioerr,'(//,2A)') 'ERROR: Invalid projection type entered: ',
     &                                          keyin(:strmin(keyin))
      write(ioerr,'(A)') 'Accceptable options are: '
      write(ioerr,'(10X,A)') 'UTM'
      write(ioerr,'(10X,A)') 'LATLON'
      write(ioerr,'(10X,A)') 'RPOLAR'
      write(ioerr,'(10X,A)') 'LAMBERT'
      write(ioerr,'(10X,A)') 'POLAR'
      write(ioerr,'(10X,A)') 'MERCATOR'
      goto 9999
c
c-----------------------------------------------------------------------
c   Format statements:
c-----------------------------------------------------------------------
c
 9000 format(A,'(',I5,',',I5,')')
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
