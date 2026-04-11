C**** RDUTMP.F 
c
      subroutine rdutmp( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the user input file and stores the data in the /TMPRL/
c    packet in common variables to be used by the TMPRL program.
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
      include 'iotmprl.inc'
      include 'usrtmprl.inc'
      include 'uamcntl.inc'
      include 'cntylst.inc'
c
c-----------------------------------------------------------------------
c    Argument Declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
c
c-----------------------------------------------------------------------
c    Subroutines Called:
c-----------------------------------------------------------------------
c
c   fndkey  --  searches a file for a keyword (library)
c   lftjst  --  left justifies a character string (library)
c   low2up  --  converts a character string to upper case (library)
c   rddate  --  reads the /DATE/ packet (library)
c   rdcnty  --  reads the /COUNTY/ packet (library)
c   rdotbl  --  reads the packets for output tables (library)
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
c   itztmp  I   temporary storage for timezone
c   keywrd  C   temporary storage for seaching fo keyword
c   uselst  C   character string for temporary storage
c   tzntmp  C   character string to read in the timezone
c   wekday  C   character string for day of week response
c
      character*(5*MXSTR) string
      character*(KEYLEN)  keywrd, keytmp
      character*(MXSTR)   uselst, wekday, tzntmp
      character*40        cdate
      character*10        module
      integer*4           irec, jerr, itztmp
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
      data keywrd /'/TMPRL/'/
      data module /'TMPRL'/
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      irec = 0
c
c   --- call routine to read date packet ---
c
      call rddate( jerr,
     &                   IORINP )
      if( jerr .NE. ISUCES ) goto 9999
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
c
      call getime( cdate )
      write(IOWMSG,9000,ERR=7000) VERSON, cdate
      write(IOWMSG,9001,ERR=7000) 'File note',fnote(1:strmin(fnote))
      write(IOWMSG,9003,ERR=7000) 'Episode date (Calander)',ibgcal
      write(IOWMSG,9002,ERR=7000) 'Episode date (Julian)',ibgdat
      write(IOWMSG,9002,ERR=7000) 'Beginning hour',INT(begtim/100.)
      write(IOWMSG,9002,ERR=7000) 'Ending hour',INT(endtim/100.)
      write(IOWMSG,9002,ERR=7000) 'Number of counties',ncdfip
c
c   --- call routine to find /TMPRL/ packet ---
c
      call fndkey( jerr,
     &                   IORINP, keywrd )
      if( jerr .EQ. IRDERR ) goto 9999
      if( jerr .NE. ISUCES ) goto 7001
c
      irec = irec + 1
      read(IORINP,'(A)',ERR=7002,END=7003) string
      wekday = string(21:MXSTR)
      call low2up( wekday )
      call lftjst( wekday )
      if( wekday(1:3) .EQ. MON ) then
          idxday = IDXMON
      else if( wekday(1:3) .EQ. TUE ) then
          idxday = IDXTUE
      else if( wekday(1:3) .EQ. WED ) then
          idxday = IDXWED
      else if( wekday(1:3) .EQ. THU ) then
          idxday = IDXTHU
      else if( wekday(1:3) .EQ. FRI ) then
          idxday = IDXFRI
      else if( wekday(1:3) .EQ. SAT ) then
          idxday = IDXSAT
      else if( wekday(1:3) .EQ. SUN ) then
          idxday = IDXSUN
      else
          goto 7005
      endif
      write(IOWMSG,9001,ERR=7000) 'Day of week',wekday(1:3)
c
c --- read inventory timezone flag  ----
c
      irec = irec + 1
      read(IORINP,'(A)',ERR=7002,END=7003) string
      keytmp = string(:20)
      call low2up( keytmp )
      call lftjst( keytmp )
      if( keytmp .EQ. KEYEND ) goto 7006
      tzntmp = string(21:MXSTR)
      call low2up( tzntmp )
      call lftjst( tzntmp )
      if( tzntmp(:KEYLEN) .EQ. KEYEND ) goto 7006
c
      if( tzntmp(1:3) .EQ. PST ) then
         invtzn = IDXPST
      else if( tzntmp(1:3) .EQ. MST ) then
         invtzn = IDXMST
      else if( tzntmp(1:3) .EQ. CST ) then
         invtzn = IDXCST
      else if( tzntmp(1:3) .EQ. EST ) then
         invtzn = IDXEST
      else if( tzntmp(1:3) .EQ. PDT ) then
         invtzn = IDXPDT
      else if( tzntmp(1:3) .EQ. MDT ) then
         invtzn = IDXMDT
      else if( tzntmp(1:3) .EQ. CDT ) then
         invtzn = IDXCDT
      else if( tzntmp(1:3) .EQ. EDT ) then
         invtzn = IDXEDT
      else if ( strmin(tzntmp) .GT. 3 ) then  
	 read (tzntmp(4:),*) itztmp
	 if ( itztmp .GT. 12 .OR. itztmp .LT. -12 ) goto 7007
	 invtzn = itztmp
      else if ( tzntmp(1:3) .EQ. 'GMT' ) then
         invtzn = 0
      else
         goto 7007
      endif
      write(IOWMSG,9001,ERR=7000) 'Inventory Timezone : ',tzntmp(1:6)
c
c  --- set error flag to sucess ---
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
      write(ioerr,'(/,3A,I8,//)',ERR=9999) 'ERROR: Reading ',
     & keywrd(:strmin(keywrd)),' packet of USERIN file at line: ',irec
      goto 9999
c
 7003 continue
      write(ioerr,'(/,3A,I8)',ERR=9999) 'ERROR: Reading ',
     & keywrd(:strmin(keywrd)),' packet of USERIN file at line: ',irec
      write(ioerr,'(A,//)',ERR=9999) 'Premature end-of-file reached.'
      goto 9999
c
 7004 continue
      write(ioerr,'(/,3A)',ERR=9999) 'ERROR: Reading ',
     &      keywrd(:strmin(keywrd)),' packet of USERIN file at line.'
      write(ioerr,'(2A)',ERR=9999) 'Invalid response to LST flag: ',
     &                                         string(21:strmin(string))
      write(ioerr,'(A)',ERR=9999) 'Valid responses are:'
      write(ioerr,'(10X,A)',ERR=9999) YES
      write(ioerr,'(10X,A,//)',ERR=9999) NO
      goto 9999
c
 7005 continue
      write(ioerr,'(/,3A)',ERR=9999) 'ERROR: Reading ',
     &      keywrd(:strmin(keywrd)),' packet of USERIN file at line.'
      write(ioerr,'(2A)',ERR=9999)
     &                  'Invalid response to day of week flag: ',
     &                                       string(21:strmin(string))
      write(ioerr,'(A)',ERR=9999) 'Valid responses are:'
      write(ioerr,'(10X,A)',ERR=9999) MON
      write(ioerr,'(10X,A)',ERR=9999) TUE
      write(ioerr,'(10X,A)',ERR=9999) WED
      write(ioerr,'(10X,A)',ERR=9999) THU
      write(ioerr,'(10X,A)',ERR=9999) FRI
      write(ioerr,'(10X,A)',ERR=9999) SAT
      write(ioerr,'(10X,A)',ERR=9999) SUN
      write(ioerr,'(2A)',ERR=9999) '  This version no longer supports ',
     &                            'the MET DATA TIME MATCH flag.'
      write(ioerr,'(3A,//)',ERR=9999) '  If your ',
     &                                  keywrd(:strmin(keywrd)),
     &                ' packet includes this record, please remove it.'
      goto 9999
c
 7006 continue
      write(ioerr,'(/,3A)',ERR=9999) 'ERROR: Reading ',
     &      keywrd(:strmin(keywrd)),' packet of USERIN file at line.'
      write(ioerr,'(A)',ERR=9999)
     &             'Found /END/ keyword when reading the timezone flag.'
      write(ioerr,'(2A)',ERR=9999) 'This version requires that you ',
     &          'specify the domain time zone in the /TMPPRL/ packet.'
      write(ioerr,'(2A)',ERR=9999) 'Please add a record identifying ',
     &                                'the time zone of the inventory.'
      write(ioerr,'(A)',ERR=9999) 'Valid responses are:'
      write(ioerr,'(10X,A)',ERR=9999) PST
      write(ioerr,'(10X,A)',ERR=9999) MST
      write(ioerr,'(10X,A)',ERR=9999) CST
      write(ioerr,'(10X,A)',ERR=9999) EST
      write(ioerr,'(10X,A)',ERR=9999) PDT
      write(ioerr,'(10X,A)',ERR=9999) MDT
      write(ioerr,'(10X,A)',ERR=9999) CDT
      write(ioerr,'(10X,A)',ERR=9999) EDT
      write(ioerr,'(10X,A)',ERR=9999) 'GMT'
      write(ioerr,'(10X,A)',ERR=9999) 'GMT +/- 1'
      write(ioerr,'(10X,A)',ERR=9999) 'GMT +/- 2'
      write(ioerr,'(10X,A)',ERR=9999) 'GMT +/- 3'
      write(ioerr,'(10X,A)',ERR=9999) 'GMT +/- 4'
      write(ioerr,'(10X,A)',ERR=9999) 'GMT +/- 5'
      write(ioerr,'(10X,A)',ERR=9999) 'GMT +/- 6'
      write(ioerr,'(10X,A)',ERR=9999) 'GMT +/- 7'
      write(ioerr,'(10X,A)',ERR=9999) 'GMT +/- 8'
      write(ioerr,'(10X,A)',ERR=9999) 'GMT +/- 9'
      write(ioerr,'(10X,A)',ERR=9999) 'GMT +/- 10'
      write(ioerr,'(10X,A)',ERR=9999) 'GMT +/- 11'
      write(ioerr,'(10X,A,//)',ERR=9999) 'GMT +/- 12'
      goto 9999
c
 7007 continue
      write(ioerr,'(/,3A)',ERR=9999) 'ERROR: Reading ',
     &      keywrd(:strmin(keywrd)),' packet of USERIN file at line.'
      write(ioerr,'(2A)',ERR=9999)
     &                  'Invalid response to timezone flag: ',
     &                                       string(21:strmin(string))
      write(ioerr,'(2A)',ERR=9999) 'This version requires that you ',
     &                                  'specify the domain time zone.'
      write(ioerr,'(A)',ERR=9999) 'Valid responses are:'
      write(ioerr,'(10X,A)',ERR=9999) PST
      write(ioerr,'(10X,A)',ERR=9999) MST
      write(ioerr,'(10X,A)',ERR=9999) CST
      write(ioerr,'(10X,A)',ERR=9999) EST
      write(ioerr,'(10X,A)',ERR=9999) PDT
      write(ioerr,'(10X,A)',ERR=9999) MDT
      write(ioerr,'(10X,A)',ERR=9999) CDT
      write(ioerr,'(10X,A)',ERR=9999) EDT
      write(ioerr,'(10X,A)',ERR=9999) 'GMT'
      write(ioerr,'(10X,A)',ERR=9999) 'GMT +/- 1'
      write(ioerr,'(10X,A)',ERR=9999) 'GMT +/- 2'
      write(ioerr,'(10X,A)',ERR=9999) 'GMT +/- 3'
      write(ioerr,'(10X,A)',ERR=9999) 'GMT +/- 4'
      write(ioerr,'(10X,A)',ERR=9999) 'GMT +/- 5'
      write(ioerr,'(10X,A)',ERR=9999) 'GMT +/- 6'
      write(ioerr,'(10X,A)',ERR=9999) 'GMT +/- 7'
      write(ioerr,'(10X,A)',ERR=9999) 'GMT +/- 8'
      write(ioerr,'(10X,A)',ERR=9999) 'GMT +/- 9'
      write(ioerr,'(10X,A)',ERR=9999) 'GMT +/- 10'
      write(ioerr,'(10X,A)',ERR=9999) 'GMT +/- 11'
      write(ioerr,'(10X,A,//)',ERR=9999) 'GMT +/- 12'
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
 9000 format(/,T20,A,A40,/)
 9001 format(T10,A,T40,:,':',A)
 9002 format(T10,A,T40,:,':',I5)
 9003 format(T10,A,T40,:,':',I6)
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
