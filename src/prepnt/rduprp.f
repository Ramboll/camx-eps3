c**** RDUPRP.F 
c
      subroutine rduprp( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the user input file and stores the data in the /PREPNT/
c    packet in common variables to be used by the PREPNT program.
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
      include 'embrdat.inc'
      include 'uamregn.inc'
      include 'cntylst.inc'
      include 'ioprepnt.inc'
      include 'usrprpnt.inc'
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
      integer*4           irec, jerr
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
      data keywrd /'/PREPNT/'/
      data module /'PREPNT'/
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- assume success ---
c
      ierr = IFAIL
      irec = 0
c
c   --- call routine to read criteria pollutants packet ---
c
      call rdcrit( jerr,
     &                   IORINP )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- call routine to read date packet ---
c
      call rddate( jerr,
     &                   IORINP )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- call routine to read region definition ---
c
      call rdregn( jerr,
     &                   IORINP )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- call routine to read county list ---
c
      call rdcnty( jerr,
     &                   IORINP )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- call routine to find /PREPNT/ packet ---
c
      call fndkey( jerr,
     &                   IORINP, keywrd )
      if( jerr .EQ. IRDERR ) goto 9999
      if( jerr .NE. ISUCES ) goto 7001
c
c   --- read the packet variables ---
c
      irec = irec + 1
      read(IORINP,'(A)',ERR=7002,END=7003) string
      respon = string(21:MXSTR)
      call low2up( respon )
      call lftjst( respon )
      if( respon(1:3) .EQ. YES ) then
          lgeods = .TRUE.
      else if( respon(1:3) .EQ. NO ) then 
          lgeods = .FALSE.
      else
          goto 7004
      endif
c
c   --- call routine to get the output tables requested ---
c
      call rdotbl( jerr,
     &                   IORINP, module, .FALSE. )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- echo the data to message file ---
c
      call getime( cdate )
      write(IOWMSG,9001,ERR=7000) VERSON, cdate
      write(IOWMSG,9002,ERR=7000) 'File note',fnote(:strmin(fnote))
      write(IOWMSG,9008,ERR=7000) 'Episode date (Calander)',ibgcal
      write(IOWMSG,9003,ERR=7000) 'Episode date (Julian)',ibgdat
      write(IOWMSG,9003,ERR=7000) 'Beginning hour',INT(begtim/100.)
      write(IOWMSG,9003,ERR=7000) 'Ending hour',INT(endtim/100.)
      write(IOWMSG,9005,ERR=7000) 'Grid origin (km)',xorig,yorig
      write(IOWMSG,9003,ERR=7000) 'UTM zone',izone
      write(IOWMSG,9005,ERR=7000) 'Grid cell width (km)',dxcell,dycell
      write(IOWMSG,9006,ERR=7000) 'Number of cells',nxcell,nycell
      write(IOWMSG,9003,ERR=7000) 'Number of counties',ncdfip
c
c   --- echo the option ----
c
      if( lgeods ) then
          write(IOWMSG,9002,ERR=7000) 'Geodesic coordinates',YES
      else 
          write(IOWMSG,9002,ERR=7000) 'Geodesic coordinates',NO
      endif
c
c
c   --- call routine to find /PREPNT/ packet ---
c
      call fndkey( jerr,
     &                   IORINP, keywrd )
      if( jerr .EQ. IRDERR ) goto 9999
      if( jerr .NE. ISUCES ) goto 7001
c
c   --- read the packet variables ---
c
      irec = irec + 1
      read(IORINP,'(A)',ERR=7002,END=7003) string
      irec = irec + 1
      read(IORINP,'(A)',ERR=7002,END=7003) string
      respon = string(21:MXSTR)
      call low2up( respon )
      call lftjst( respon )
      if( respon(1:3) .EQ. YES ) then
          lprces = .TRUE.
          write(IOWMSG,9002,ERR=7000) 'Process missing stack data',YES
      else if( respon(1:3) .EQ. NO ) then
          lprces = .FALSE.
          write(IOWMSG,9002,ERR=7000) 'Process missing stack data',NO
      else
          goto 7005
      endif
c
c   --- read plume height cutoff and default stack parameters ---
c
      read(IORINP,'(A)',ERR=7002,END=7003) string
      read(string,'(20X,F10.0)',ERR=7006) pcutof
      read(IORINP,'(A)',ERR=7002,END=7003) string
      read(string,'(20X,F10.0)',ERR=7006) dfhite
      read(IORINP,'(A)',ERR=7002,END=7003) string
      read(string,'(20X,F10.0)',ERR=7006) dfdiam
      read(IORINP,'(A)',ERR=7002,END=7003) string
      read(string,'(20X,F10.0)',ERR=7006) dftemp
      read(IORINP,'(A)',ERR=7002,END=7003) string
      read(string,'(20X,F10.0)',ERR=7006) dfvel
c
c   ---- echo data to message file ----
c
      write(IOWMSG,9004,ERR=7000) 'Plume height cutoff (m)',pcutof
      write(IOWMSG,9004,ERR=7000) 'Default stack height (m)',dfhite
      write(IOWMSG,9004,ERR=7000) 'Default stack diameter (m)',dfdiam
      write(IOWMSG,9004,ERR=7000) 'Default gas temperature (k)',dftemp
      write(IOWMSG,9004,ERR=7000) 'Default exit velocity (m/s)',dfvel
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
 7004 continue
      write(ioerr,'(/,3A)',ERR=9999) 'ERROR: Reading ',
     &             keywrd(:strmin(keywrd)),' packet of USERIN file.'
      write(ioerr,'(2A)',ERR=9999)
     &             'Invalid response to geodesic coordinates flag: ',
     &                                      string(21:strmin(string))
      write(ioerr,'(A)',ERR=9999) 'Valid responses are:'
      write(ioerr,'(10X,A)',ERR=9999) YES
      write(ioerr,'(10X,A)',ERR=9999) NO
      goto 9999
c
 7005 continue
      write(ioerr,'(/,3A)',ERR=9999) 'ERROR: Reading ',
     &      keywrd(:strmin(keywrd)),' packet of USERIN file.'
      write(ioerr,'(2A)',ERR=9999)
     &         'Invalid response to process missing stack flag: ',
     &                                       string(21:strmin(string))
      write(ioerr,'(A)',ERR=9999) 'Valid responses are:'
      write(ioerr,'(10X,A)',ERR=9999) YES
      write(ioerr,'(10X,A,//)',ERR=9999) NO
      goto 9999
c
 7006 continue
      write(ioerr,'(/,3A,I5)',ERR=9999) 'ERROR: Reading ',
     & keywrd(:strmin(keywrd)),' packet of USERIN file at line: ',irec
      write(ioerr,'(A)',ERR=9999) 'Line read:'
      write(ioerr,'(A,//)',ERR=9999) string(:strmin(string))
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
 9001 format(/,T20,A,A40,/)
 9002 format(T10,A,T40,:,':',A)
 9003 format(T10,A,T40,:,':',I5,:,I5,:,I5)
 9004 format(T10,A,T40,:,':',F7.2)
 9005 format(T10,A,T40,:,': (',F10.3,',',F10.3,')')
 9006 format(T10,A,T40,:,': (',I5,',',I5,')')
 9007 format(T10,A,T40,:,':',F10.3,:,1X,F10.3,:,1X,F10.3)
 9008 format(T10,A,T40,:,':',I6)
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
