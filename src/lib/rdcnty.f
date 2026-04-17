C**** RDCNTY.F 
c
      subroutine rdcnty( ierr,
     &                         iounit )
c
c-----------------------------------------------------------------------
c
c    Reads the user input file and stores the data in the /COUNTY/
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
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'hash.inc'
      include 'cntylst.inc'
      include 'embrdat.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
      integer*4 iounit
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
c   strmin      I     returns actual length of string
c   mkhash_fips I     returns the hash code for the county
c
      integer*4 strmin
      integer*4 mkhash_fips
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
c   keywrd  C   keyword for packet as variable
c   keytmp  C   character string for temporary storage
c   fiptmp  C   character string for reading FIPS codes
c   tzntmp  C   character string for timezone of each county 
c   itztmp  I   integer for temp storage of timezone of each county 
c   jerr    I   error flag for subroutine calls
c
      character*(5*MXSTR) string
      character*(KEYLEN)  keywrd, keytmp
      character*(MXSTR)   strtmp, tzntmp
      character*5         fiptmp
      integer*4           idxfrst, idxhash, idxnext, idxprev
      integer*4           jerr, i, itztmp
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
      data keywrd /'/COUNTY/'/
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   ---  set error flag ---
c
      ierr = IFAIL
c
c   ---- initialize the hash table entries ---
c
      do i=1,MXHASH_FIPS
         idxhsh_first_fips(i) = IMISS
      enddo
      do i=1,MXFIP+1
         idxhsh_next_fips(i) = IMISS
      enddo
c
c   --- call routine to get to find packet identifier ---
c
      ncdfip = 0
      call fndkey( jerr,
     &                   iounit, keywrd )
      if( jerr .EQ. IRDERR ) goto 9999
      if( jerr .NE. ISUCES ) goto 7000
c
c   --- read counties (incramenting the array counter) ---
c
  111 continue
      read( iounit,'(A)',ERR=7001,END=7003) string
      read( string,'(A19,1X,A5,1X,A)',ERR=7004 ) keytmp, 
     &                                              fiptmp, strtmp
c
      call low2up( keytmp )
      call lftjst( keytmp )
      if( keytmp .EQ. KEYEND ) goto 222
      ncdfip = ncdfip + 1
      if( ncdfip .GT. MXFIP ) goto 7002
      fipcd(ncdfip) = fiptmp
c
c   --- get the hash value for this code ---
c
      idxfrst = mkhash_fips( fiptmp )
c
c   --- if this is the first hash entry for this value, 
c       initialize the hash index arrays ---
c    
      if( idxhsh_first_fips(idxfrst) .EQ. IMISS ) then
         idxhsh_first_fips(idxfrst) = ncdfip
         goto 444
c
c   --- get the next available position for this hash value ---
c
      else
         idxhash = idxhsh_first_fips(idxfrst)
         idxprev = idxhash
      endif
  333 continue
      if( idxhash .EQ. IMISS ) then
         idxhsh_next_fips(idxprev) = ncdfip
      else
         idxprev = idxhash
         idxnext = idxhsh_next_fips(idxhash)
         idxhash = idxnext
         goto 333
      endif
c
c   --- get the time zone for this county ---
c
  444 continue
      if( strtmp .EQ. ' ' ) goto 7005
      tzntmp = strtmp
      call low2up( tzntmp )
      call lftjst( tzntmp )
      if( tzntmp(1:3) .EQ. PST ) then
         itmzon(ncdfip) = IDXPST
      else if( tzntmp(1:3) .EQ. MST ) then
         itmzon(ncdfip) = IDXMST
      else if( tzntmp(1:3) .EQ. CST ) then
         itmzon(ncdfip) = IDXCST
      else if( tzntmp(1:3) .EQ. EST ) then
         itmzon(ncdfip) = IDXEST
      else if( tzntmp(1:3) .EQ. PDT ) then
         itmzon(ncdfip) = IDXPDT
      else if( tzntmp(1:3) .EQ. MDT ) then
         itmzon(ncdfip) = IDXMDT
      else if( tzntmp(1:3) .EQ. CDT ) then
         itmzon(ncdfip) = IDXCDT
      else if( tzntmp(1:3) .EQ. EDT ) then
         itmzon(ncdfip) = IDXEDT
      else if ( strmin(tzntmp) .GT. 3 ) then  
	 read (tzntmp(4:),* ) itztmp
	 if ( itztmp .GT. 12 .OR. itztmp .LT. -12 ) goto 7006
	 itmzon(ncdfip) = itztmp
      else if ( tzntmp(1:3) .EQ. 'GMT' ) then
         itmzon(ncdfip) = 0
      else
         goto 7006
      endif
c
c  --- get the next county ---

      goto 111
c
c   --- set error flag to success ---
c
  222 continue
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
     &                 'ERROR: Reading ',keywrd(:strmin(keywrd)),
     &                     ' packet of USERIN file at line: ',ncdfip+1
      goto 9999
c
 7002 continue
      write(ioerr,'(/,3A,I5)',ERR=9999) 'ERROR: Reading ',
     &                keywrd(:strmin(keywrd)),' packet of USERIN file.'
      write(ioerr,'(A)',ERR=9999) 'Number of counties exceeds max.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maximum allowed: ',MXFIP
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXFIP ',
     &                                                 'and recompile.'
      goto 9999
c
 7003 continue
      write(ioerr,'(/,3A,I8)',ERR=9999) 'ERROR: Reading ',
     &     keywrd(:strmin(keywrd)),' packet of USERIN file at line: ',
     &                                                          ncdfip+1
      write(ioerr,'(A,//)',ERR=9999) 'Premature end-of-file reached.'
      goto 9999
c
 7004 continue
      write(ioerr,'(/,3A,I8)',ERR=9999)
     &                     'ERROR: Reading ',keywrd(:strmin(keywrd)),
     &                     ' packet of USERIN file at line: ',ncdfip+1
      write(ioerr,'(A)',ERR=9999)  'Line read:'
      write(ioerr,'(A)',ERR=9999)  string(:strmin(string))
      write(ioerr,'(2A)',ERR=9999) 'This version requires that you ',
     &                         'specify the time zone of each county.'
      write(ioerr,'(2A,//)',ERR=9999) 'Please add a time zone field ',
     &                                         'to the county packet.'
      goto 9999
c
 7005 continue
      write(ioerr,'(/,3A,I8)',ERR=9999)
     &                     'ERROR: Reading ',keywrd(:strmin(keywrd)),
     &                     ' packet of USERIN file at line: ',ncdfip
      write(ioerr,'(A)',ERR=9999)  'Line read:'
      write(ioerr,'(A)',ERR=9999)  string(:strmin(string))
      write(ioerr,'(2A)',ERR=9999) 'This version requires that you ',
     &                         'specify the time zone of each county.'
      write(ioerr,'(2A,//)',ERR=9999) 'Please add a time zone field ',
     &                                         'to the county packet.'
      goto 9999
c
 7006 continue
      write(ioerr,'(/,3A,I8)',ERR=9999)
     &                     'ERROR: Reading ',keywrd(:strmin(keywrd)),
     &                     ' packet of USERIN file at line: ',ncdfip
      write(ioerr,'(A)',ERR=9999)  'Line read:'
      write(ioerr,'(A)',ERR=9999)  string(:strmin(string))
      write(ioerr,'(2A)',ERR=9999)
     &                  'Invalid response to timezone flag: ',
     &                                       strtmp(:strmin(strtmp))
      write(ioerr,'(2A)',ERR=9999) 'This version requires that you ',
     &                         'specify the time zone of each county.'
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
