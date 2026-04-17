C**** RDOTBL.F
c    
      subroutine rdotbl( ierr, 
     &                         iounit, module, lspec)
c
c-----------------------------------------------------------------------
c
c    Reads the user input file and parses the data in the
c    /CRITERIA TABLES/ packet and the /SPECIES TABLES/ packet.
c    It sets the global flags for all of the tables that are
c    requested for this module.
c
c    Argument declaration.
c     Outputs:
c       ierr    I error flag
c     Inputs:
c       iounit  I logical unit number of file to read
c       module  C current module (name of this program)
c       lspec   L if .TRUE. then speciated packet is parsed
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'embrdat.inc'
      include 'msgtot.inc'
c
c-----------------------------------------------------------------------
c    Argument declerations:
c-----------------------------------------------------------------------
c
      character*10 module
      integer*4    ierr
      integer*4    iounit
      logical*4    lspec
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
      character*(KEYLEN) keywrd, keyin
      character*(MXSTR)  string, tmpstr, respon
      character*10       name, modcsv
      integer*4          irec, jerr, iplace, i
      logical*4          lflags(NTABLS,IDXCSV)
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      modcsv = module
      call lftjst( modcsv )
      iplace = strmin( modcsv )
      modcsv(iplace+1:iplace+1) = ","
      irec = 0
c
c   --- assume no tables are requested ---
c
      do i=1,IDXFMT,IDXCSV
        lcrtcty(i) = .FALSE.
        lcrtasc(i) = .FALSE.
        lcrtfcd(i) = .FALSE.
        lcrthr(i)  = .FALSE.
        lcrthcd(i) = .FALSE.
        lcrtfac(i) = .FALSE.
        lcrtstk(i) = .FALSE.
        lcrtpnt(i) = .FALSE.
c
        lspccty(i) = .FALSE.
        lspcasc(i) = .FALSE.
        lspcfcd(i) = .FALSE.
        lspchr(i)  = .FALSE.
        lspchcd(i) = .FALSE.
        lspcfac(i) = .FALSE.
        lspcstk(i) = .FALSE.
        lspcpnt(i) = .FALSE.
      enddo
c
c   --- look first for the /CRITRIA TABLES/ packet ---
c
      keywrd = '/CRITERIA TABLES/'
      call fndkey( jerr,
     &                   iounit, keywrd )
      if( jerr .EQ. IRDERR ) goto 9999
      if( jerr .NE. ISUCES ) goto 6000
c
c   --- loop through all possible table options ---
c
      do i=1,NTABLS
         lflags(i,IDXFMT) = .FALSE.
         lflags(i,IDXCSV) = .FALSE.
         irec = irec + 1
         read( iounit,'(A)',ERR=7000,END=7001 ) string
         keyin = string(:KEYLEN)
         call lftjst( keyin )
         call low2up( keyin )
         if( keyin .EQ. KEYEND ) goto 7002
         tmpstr = string(21:)
         call lftjst( tmpstr )
         call low2up( tmpstr )
c
c   --- call routine to get the words and compare against this module ---
c
 111     continue
         call token( jerr, name, tmpstr )
         if( name .EQ. module ) lflags(i,IDXFMT) = .TRUE.
         if( name .EQ. modcsv ) then
              lflags(i,IDXFMT) = .TRUE.
              lflags(i,IDXCSV) = .TRUE.
         endif
         if( jerr .NE. IEOD ) goto 111
      enddo
c
c   --- load the flags into global variables, this is where
c       the order of the records in the packet get sorted out ---
c
      lcrtcty(IDXFMT) = lflags(1,IDXFMT)
      lcrtasc(IDXFMT) = lflags(2,IDXFMT)
      lcrtfcd(IDXFMT) = lflags(3,IDXFMT)
      lcrthr(IDXFMT)  = lflags(4,IDXFMT)
      lcrthcd(IDXFMT) = lflags(5,IDXFMT)
      lcrtfac(IDXFMT) = lflags(6,IDXFMT)
      lcrtstk(IDXFMT) = lflags(7,IDXFMT)
      lcrtpnt(IDXFMT) = lflags(8,IDXFMT)
c
      lcrtcty(IDXCSV) = lflags(1,IDXCSV)
      lcrtasc(IDXCSV) = lflags(2,IDXCSV)
      lcrtfcd(IDXCSV) = lflags(3,IDXCSV)
      lcrthr(IDXCSV)  = lflags(4,IDXCSV)
      lcrthcd(IDXCSV) = lflags(5,IDXCSV)
      lcrtfac(IDXCSV) = lflags(6,IDXCSV)
      lcrtstk(IDXCSV) = lflags(7,IDXCSV)
      lcrtpnt(IDXCSV) = lflags(8,IDXCSV)
c
c   --- set flag that keeps track if any of the point source
c       ID tables was requested ----
c
      lcrtpid = .FALSE.
      if( lcrtfac(IDXFMT) ) lcrtpid = .TRUE.
      if( lcrtstk(IDXFMT) ) lcrtpid = .TRUE.
      if( lcrtpnt(IDXFMT) ) lcrtpid = .TRUE.
      if( lcrtfac(IDXCSV) ) lcrtpid = .TRUE.
      if( lcrtstk(IDXCSV) ) lcrtpid = .TRUE.
      if( lcrtpnt(IDXCSV) ) lcrtpid = .TRUE.
c
c   --- if flag is not set, don't look for species packet ---
c
  222 continue
      if( .NOT. lspec ) goto 444
c
c   --- now look for the /SPECIES TABLES/ packet ---
c
      keywrd = '/SPECIES TABLES/'
      call fndkey( jerr,
     &                   iounit, keywrd )
      if( jerr .EQ. IRDERR ) goto 9999
      if( jerr .NE. ISUCES ) goto 6001
c
c   --- loop through all possible table options ---
c
      do i=1,NTABLS
         lflags(i,IDXFMT) = .FALSE.
         lflags(i,IDXCSV) = .FALSE.
         irec = irec + 1
         read( iounit,'(A)',ERR=7000,END=7001 ) string
         keyin = string(:KEYLEN)
         call lftjst( keyin )
         call low2up( keyin )
         if( keyin .EQ. KEYEND ) goto 7002
         tmpstr = string(21:)
         call lftjst( tmpstr )
         call low2up( tmpstr )
c
c   --- call routine to get the words and compare against this module ---
c
 333     continue
         call token( jerr, name, tmpstr )
         if( name .EQ. module ) lflags(i,IDXFMT) = .TRUE.
         if( name .EQ. modcsv ) then
             lflags(i,IDXFMT) = .TRUE.
             lflags(i,IDXCSV) = .TRUE.
         endif
         if( jerr .NE. IEOD ) goto 333
      enddo
c
c   --- load the flags into global variables, this is where
c       the order of the records in the packet get sorted out ---
c
      lspccty(IDXFMT) = lflags(1,IDXFMT)
      lspcasc(IDXFMT) = lflags(2,IDXFMT)
      lspcfcd(IDXFMT) = lflags(3,IDXFMT)
      lspchr(IDXFMT)  = lflags(4,IDXFMT)
      lspchcd(IDXFMT) = lflags(5,IDXFMT)
      lspcfac(IDXFMT) = lflags(6,IDXFMT)
      lspcstk(IDXFMT) = lflags(7,IDXFMT)
      lspcpnt(IDXFMT) = lflags(8,IDXFMT)
c
      lspccty(IDXCSV) = lflags(1,IDXCSV)
      lspcasc(IDXCSV) = lflags(2,IDXCSV)
      lspcfcd(IDXCSV) = lflags(3,IDXCSV)
      lspchr(IDXCSV)  = lflags(4,IDXCSV)
      lspchcd(IDXCSV) = lflags(5,IDXCSV)
      lspcfac(IDXCSV) = lflags(6,IDXCSV)
      lspcstk(IDXCSV) = lflags(7,IDXCSV)
      lspcpnt(IDXCSV) = lflags(8,IDXCSV)
c
c   --- set flag that keeps track if any of the point source
c       ID tables was requested ----
c
      lspcpid = .FALSE.
      if( lspcfac(IDXFMT) ) lspcpid = .TRUE.
      if( lspcstk(IDXFMT) ) lspcpid = .TRUE.
      if( lspcpnt(IDXFMT) ) lspcpid = .TRUE.
      if( lspcfac(IDXCSV) ) lspcpid = .TRUE.
      if( lspcstk(IDXCSV) ) lspcpid = .TRUE.
      if( lspcpnt(IDXCSV) ) lspcpid = .TRUE.
c
c   --- set error flag to success ---
c
 444  continue
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Warning messages:
c-----------------------------------------------------------------------
c
 6000 continue
      write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
      write(ioerr,'(3A)',ERR=9999) 'WARNING: Cannot find the ',
     &                keywrd(:strmin(keywrd)),' packet of USERIN file.'
      write(ioerr,'(2A)',ERR=9999) 'Only default tables for ',
     &        'criteria pollutants will be written to the message file.'
      write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
      nwarn = nwarn + 1
      goto 222
c
 6001 continue
      write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
      write(ioerr,'(3A)',ERR=9999) 'WARNING: Cannot find the ',
     &                keywrd(:strmin(keywrd)),' packet of USERIN file.'
      write(ioerr,'(2A)',ERR=9999) 'Only default tables for ',
     &       'speciated pollutants will be written to the message file.'
      write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
      nwarn = nwarn + 1
      goto 444
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,3A,I8,//)',ERR=9999) 'ERROR: Reading ',
     & keywrd(:strmin(keywrd)),' packet of USERIN file at line: ',irec
      goto 9999
c
 7001 continue
      write(ioerr,'(/,3A,I8)',ERR=9999) 'ERROR: Reading ',
     & keywrd(:strmin(keywrd)),' packet of USERIN file at line: ',irec
      write(ioerr,'(A,//)',ERR=9999) 'Premature end-of-file reached.'
      goto 9999
c
 7002 continue
      write(ioerr,'(/,3A,I8)',ERR=9999) 'ERROR: Reading ',
     & keywrd(:strmin(keywrd)),' packet of USERIN file at line: ',irec
      write(ioerr,'(A)',ERR=9999) 'Premature end-of-packet reached.'
      write(ioerr,'(2A,//)',ERR=9999) 'Make sure there is a line ',
     &                               'for each of the table options.'
      goto 9999
c
c-----------------------------------------------------------------------
c   Format statements:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
