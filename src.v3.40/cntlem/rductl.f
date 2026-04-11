C**** RDUCTL.F 
c
      subroutine rductl( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the user input file and stores the data in the /CNTLEM/
c    packet in common variables to be used by the pream program.
c
c    Argument Declaration.
c     Outputs:
c       ierr  I error flag
c     Inputs:
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Include Files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'iocntlem.inc'
      include 'cntylst.inc'
      include 'cntlem.inc'
c
c-----------------------------------------------------------------------
c    Argument Declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
c
c-----------------------------------------------------------------------
c    External Functions:
c-----------------------------------------------------------------------
c
c   strmin  I   returns the actual length of a string
c
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c   keywrd  C   temporary storage for seaching of keyword
c   respon  C   character string for temporary storage
c   jerr    I   error flag for subroutine calls
c
      character*(MXSTR)  string
      character*(KEYLEN) keywrd, module
      character*(MXSTR)  respon
      character*40       cdate
      integer*4          jerr, irec
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
      data keywrd /'/CNTLEM/'/
      data module /'CNTLEM'/
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
      ierr = IFAIL
      irec = 0
c
c   --- call routine to read county list ---
c
      call rdcnty( jerr,
     &                   IORINP )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- call routine to get the output tables to generate ---
c
      call rdotbl( jerr,
     &                   IORINP, module, .TRUE. )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- echo the data to message file ---
c   --- VERSON is defined in iopream ---
c
      call getime( cdate )
      write(IOWMSG,'(/,T20,A,A40,/)',ERR=7000) VERSON, cdate
      write(IOWMSG,'(T10,A,I5)',ERR=7000) 'Number of counties: ',ncdfip
c
c   --- call routine to find /CNTLEM/ packet ---
c
      call fndkey( jerr,
     &                   IORINP, keywrd )
      if( jerr .EQ. IRDERR ) goto 9999
      if( jerr .NE. ISUCES ) goto 7001
c
c   --- read the write county tables option ---
c
      irec = irec + 1
      read(IORINP,'(A)',ERR=7002,END=7003) string
      respon = string(21:) 
      call lftjst( respon )
      call low2up( respon )
      if( respon(1:3) .EQ. YES ) then
          lprntc = .TRUE.
      else if( respon(1:3) .EQ. NO ) then
          lprntc = .FALSE.
      else
          goto 7004
      endif
c
c  --- check to see if there is another flag ---
c
      lexact = .FALSE.
      irec = irec + 1
      read(IORINP,'(A)',ERR=7002,END=7003) string
      respon = string
      call low2up( respon )
      if( respon(:KEYLEN) .EQ. KEYEND ) goto 111
c
c  --- we have another switch, must be the exact match switch ---
c
      respon = string(21:) 
      call lftjst( respon )
      call low2up( respon )
      if( respon(1:3) .EQ. YES ) then
          lexact = .TRUE.
      else if( respon(1:3) .EQ. NO ) then
          lexact = .FALSE.
      else
          goto 7005
      endif
c
c   --- write option to message file
c
 111  continue
      if ( .NOT. lprntc ) then
        write(IOWMSG,'(T10,A,/)',ERR=7000)
     &              'Total control tables only will be created.'
      else
        write(IOWMSG,'(T10,A,/)',ERR=7000)
     &              'County and total control tables will be created.'
      endif
      if ( lexact ) then
        write(IOWMSG,'(T10,A,/)',ERR=7000)
     &        'Only EXACT MATCHES will be allowed for control factors.'
      else
        write(IOWMSG,'(T10,A,/)',ERR=7000)
     &           'Best match criteria for control factors is in place.'
      endif
c
c   ---- set error flag to success ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      ioerr = IOWSTD
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Writing message file.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',msgfl(:strmin(msgfl))
      goto 9999
c
 7001 continue
      write(ioerr,'(/,A)',ERR=9999) 'ERROR: Reading the USERIN file.'
      write(ioerr,'(3A,//)',ERR=9999) 'This program requires the ',
     &                     keywrd(:strmin(keywrd)),' packet of USERIN.'
      goto 9999
c
 7002 continue
      write(ioerr,'(/,3A,I5,//)',ERR=9999) 'ERROR: Reading ',
     & keywrd(:strmin(keywrd)),' packet of USERIN file at line: ',irec
      goto 9999
c
 7003 continue
      write(ioerr,'(/,3A,I5)',ERR=9999) 'ERROR: Reading ',
     & keywrd(:strmin(keywrd)),' packet of USERIN file at line: ',irec
      write(ioerr,'(A,//)',ERR=9999) 'Premature end-of-file reached.'
      goto 9999
c
 7004 continue
      write(ioerr,'(/,3A)',ERR=9999) 'ERROR: Reading ',
     &      keywrd(:strmin(keywrd)),' packet of USERIN file.'
      write(ioerr,'(2A)',ERR=9999) 
     &        'Invalid response to county tables option: ',
     &                                         string(21:strmin(string))
      write(ioerr,'(A)',ERR=9999) 'Valid responses are:'
      write(ioerr,'(10X,A)',ERR=9999) YES
      write(ioerr,'(10X,A,//)',ERR=9999) NO
      goto 9999
c
c
 7005 continue
      write(ioerr,'(/,3A)',ERR=9999) 'ERROR: Reading ',
     &      keywrd(:strmin(keywrd)),' packet of USERIN file.'
      write(ioerr,'(2A)',ERR=9999)
     &        'Invalid response to exact match option: ',
     &                                         string(21:strmin(string))
      write(ioerr,'(A)',ERR=9999) 'Valid responses are:'
      write(ioerr,'(10X,A)',ERR=9999) YES
      write(ioerr,'(10X,A,//)',ERR=9999) NO
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
