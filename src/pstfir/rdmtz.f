c**** RDMTZ.F
c
      subroutine rdmtz( ierr, invtzn )
c
c-----------------------------------------------------------------------
c
c    Reads the modeling time zone from the /TMPRL/ packet
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
       include 'iopstfir.inc'
c
c-----------------------------------------------------------------------
c    Argument Declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
      integer*4 invtzn
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
c   keywrd  C   temporary storage for seaching for keyword
c   tzntmp  C   character string to read in the timezone
c
      character*(MXSTR)   string, tzntmp
      character*(KEYLEN)  keywrd, keytmp
      integer*4           jerr, itztmp, irec
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
      data keywrd /'/TMPRL/'/
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- initialize error code ---
c
      ierr = IFAIL
      irec = 1
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
c   ---- set error flag to sucess ---
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
      ierr = IFAIL
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
 9001 format(/,T10,A,T40,:,':',A6,/)
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
