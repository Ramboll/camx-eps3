C**** RDUCHM.F 
c
      subroutine rduchm( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the user input file, looking for the /CHMSPL/ packet
c    and the /SPECIES LIST/ packet
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
      include 'uamcntl.inc'
      include 'iochmspl.inc'
      include 'chmspl.inc'
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
c   strmin  I   returns the actual length of a string (min of 1)
c
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c   jerr    I   error flag for subroutine calls
c   keywrd  C   temporary storage for seaching fo keyword
c   respon  C   character string for temporary storage
c
      character*(5*MXSTR) string
      character*(KEYLEN)  keywrd
      character*(MXSTR)   respon
      character*40        cdate
      character*10        module
      integer*4           jerr, irec
c
c-----------------------------------------------------------------------
c   data statements:
c-----------------------------------------------------------------------
c
      data keywrd /'/CHMSPL/'/
      data module /'CHMSPL'/
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- assume success ---
c
      ierr = IFAIL
c
c   --- call routine to read date packet ---
c
      call rddate( jerr,
     &                   IORINP )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- call routine to read control packet ---
c
      call rdspec( jerr,
     &                   IORINP, .TRUE. )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- call routine to read county list ---
c
      call rdcnty( jerr,
     &                   IORINP )
      if( jerr .NE. ISUCES ) goto 9999
c
c  --- call routine to read the /DEFAULT SPLITS/ packet ---
c
      call rddfspl( jerr,
     &                    IORINP )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- call routine to get the output tables requested ---
c
      call rdotbl( jerr, 
     &                   IORINP, module, .TRUE. )
      if( jerr .NE. ISUCES ) goto 9999
c
c  --- echo data to message file ---
c
      call getime( cdate )
      write(IOWMSG,'(/,T20,A,A40,/)',ERR=7000) VERSON, cdate
      write(IOWMSG,9000,ERR=7000) 'File note',fnote(:strmin(fnote))
      write(IOWMSG,9001,ERR=7000) 'Episode date (Calander)',ibgcal
      write(IOWMSG,9002,ERR=7000) 'Episode date (Julian)',ibgdat
      write(IOWMSG,9002,ERR=7000) 'Beginning hour',INT(begtim/100.)
      write(IOWMSG,9002,ERR=7000) 'Ending hour',INT(endtim/100.)
c
c   --- call routine to find /CHMSPL/ packet ---
c
      call fndkey( jerr,
     &                   IORINP, keywrd )
      if( jerr .EQ. IRDERR ) goto 9999
      if( jerr .NE. ISUCES ) goto 7001
c
c   --- read the records ---
c
      irec = 1
      read(IORINP,'(A)',ERR=7002,END=7003) string
      respon = string(21:MXSTR)
      call low2up( respon )
      call lftjst( respon )
      if( respon(1:3) .EQ. YES ) then
          lovrid = .TRUE.
          write(IOWMSG,9000,ERR=7000)
     &                   'Use inconsistent species lists',YES
      else if( respon(1:3) .EQ. NO ) then
          lovrid = .FALSE.
          write(IOWMSG,9000,ERR=7000)
     &                   'Use inconsistent species lists',NO
      else
          goto 7004
      endif
c
      irec = irec + 1
      read(IORINP,'(A)',ERR=7002,END=7003) string
      respon = string(21:MXSTR)
      call low2up( respon )
      call lftjst( respon )
      if( respon(1:3) .EQ. YES ) then
          lprtpf = .TRUE.
          write(IOWMSG,9000,ERR=7000) 'Write totals by profile code',YES
      else if( respon(1:3) .EQ. NO ) then
          lprtpf = .FALSE.
          write(IOWMSG,9000,ERR=7000) 'Write totals by profile code',NO
      else
          goto 7005
      endif
c
      irec = irec + 1
      read(IORINP,'(A)',ERR=7002,END=7003) string
      respon = string(21:MXSTR)
      call low2up( respon )
      call lftjst( respon )
      if( respon(1:3) .EQ. YES ) then
          lreqpf = .TRUE.
          write(IOWMSG,9000,ERR=7000)
     &                   'Force profile match',YES
      else if( respon(1:3) .EQ. NO ) then
          lreqpf = .FALSE.
          write(IOWMSG,9000,ERR=7000)
     &                   'Force profile match',NO
      else
          goto 7006
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
      ierr = IFAIL
      goto 9999
c
 7002 continue
      write(ioerr,'(/,3A,I5,//)',ERR=9999) 'Reading ',
     & keywrd(:strmin(keywrd)),' packet of USERIN file at line: ',irec
      goto 9999
c
 7003 continue
      write(ioerr,'(/,3A,I5)',ERR=9999) 'Reading ',
     & keywrd(:strmin(keywrd)),' packet of USERIN file at line: ',irec
      write(ioerr,'(A,//)',ERR=9999) 'Premature end-of-file reached.'
      goto 9999
c
 7004 continue
      write(ioerr,'(/,3A)',ERR=9999) 'ERROR: Reading ',
     &      keywrd(:strmin(keywrd)),' packet of USERIN file at line.'
      write(ioerr,'(2A)',ERR=9999)
     &          'Invalid response to use consistent species flag: ',
     &                                       string(21:strmin(string))
      write(ioerr,'(A)',ERR=9999) 'Valid responses are:'
      write(ioerr,'(10X,A)',ERR=9999) YES
      write(ioerr,'(10X,A)',ERR=9999) NO
      goto 9999
c
 7005 continue
      write(ioerr,'(/,3A)',ERR=9999) 'ERROR: Reading ',
     &      keywrd(:strmin(keywrd)),' packet of USERIN file at line.'
      write(ioerr,'(2A)',ERR=9999)
     &          'Invalid response to write totals by profile flag: ',
     &                                       string(21:strmin(string))
      write(ioerr,'(A)',ERR=9999) 'Valid responses are:'
      write(ioerr,'(10X,A)',ERR=9999) YES
      write(ioerr,'(10X,A)',ERR=9999) NO
      goto 9999
c
 7006 continue
      write(ioerr,'(/,3A)',ERR=9999) 'ERROR: Reading ',
     &      keywrd(:strmin(keywrd)),' packet of USERIN file at line.'
      write(ioerr,'(2A)',ERR=9999)
     &            'Invalid response to force profile match flag: ',
     &                                       string(21:strmin(string))
      write(ioerr,'(A)',ERR=9999) 'Valid responses are:'
      write(ioerr,'(10X,A)',ERR=9999) YES
      write(ioerr,'(10X,A)',ERR=9999) NO
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
 9000 format(T10,A,T40,:,':',A)
 9001 format(T10,A,T40,:,':',I6)
 9002 format(T10,A,T40,:,':',I5,:,I5,:,I5)
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
