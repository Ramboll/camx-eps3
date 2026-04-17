c**** RDPRFP.F
c
      subroutine rdprfp( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the USERIN input file for the /PREFIR/ packet and stores
c    in common variables to be used by the PREFIR program.
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
      include 'ioprefir.inc'
      include 'usrprfir.inc'
      include 'firebin.inc'
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
      integer*4           irec, jerr
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
      data keywrd /'/PREFIR/'/
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c     --- initialize error flag ---
c
      ierr = IFAIL
      irec = 0
c
c     --- call routine to find /PREFIR/ packet ---
c
      call fndkey( jerr,
     &                   IORINP, keywrd )
      if( jerr .EQ. IRDERR ) goto 9999
      if( jerr .NE. ISUCES ) goto 7001
c
c     --- read the packet variables ---
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
c     --- echo the option ----
c
      if( lgeods ) then
          write(IOWMSG,9002,ERR=7000) 'Geodesic coordinates',YES
      else 
          write(IOWMSG,9002,ERR=7000) 'Geodesic coordinates',NO
      endif
c
c     --- read the packet variables ---
c
      irec = irec + 1
      read(IORINP,'(A)',ERR=7002,END=7003) string
      respon = string(21:MXSTR)
      call low2up( respon )
      call lftjst( respon )
      if( respon(1:3) .EQ. YES ) then
          lprces = .TRUE.
          write(IOWMSG,9002,ERR=7000) 'Process missing fire acreage',YES
      else if( respon(1:3) .EQ. NO ) then
          lprces = .FALSE.
          write(IOWMSG,9002,ERR=7000) 'Process missing fire acreage',NO
      else
          goto 7005
      endif
c
c     --- read default fire class parameter ---
c
      read(IORINP,'(A)',ERR=7002,END=7003) string
      if ( lprces )  then
         read(string,'(20X,I10)',ERR=7006) ibindf
         write(IOWMSG,9008,ERR=7000) 'Default fire class ',ibindf
         if ( ibindf .GT. nfbin .OR. ibindf .LE. 0 ) goto 7008
      endif
c
c     --- read Do you want to combine points within grid cell? ---
c
      read(IORINP,'(A)',ERR=7002,END=7003) string
      respon = string(21:MXSTR)
      call low2up( respon )
      call lftjst( respon )
      if( respon(1:3) .EQ. YES ) then
          lcomb = .TRUE.
          write(IOWMSG,9002,ERR=7000) 'Combine point sources', YES
      else if( respon(1:3) .EQ. NO ) then
          lcomb = .FALSE.
          write(IOWMSG,9002,ERR=7000) 'Combine point sources', NO
      else
          goto 7007
      endif
c
c     --- read grid resolution for combining points ---
c
      if ( lcomb ) then
         read(IORINP,'(A)',ERR=7002,END=7003) string
         read(string,'(20X,F10.0)',ERR=7006) dxcomb
         write(IOWMSG,9004,ERR=7000) 'Combining resolution (km)', dxcomb
      endif
c
c     ---- set error flag to sucess ---
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
     &         'Invalid response to process fire acreage flag: ',
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
 7007 continue
      write(ioerr,'(/,3A)',ERR=9999) 'ERROR: Reading ',
     &      keywrd(:strmin(keywrd)),' packet of USERIN file.'
      write(ioerr,'(2A)',ERR=9999)
     &         'Invalid response to combine point sources: ',
     &                                       string(21:strmin(string))
      write(ioerr,'(A)',ERR=9999) 'Valid responses are:'
      write(ioerr,'(10X,A)',ERR=9999) YES
      write(ioerr,'(10X,A,//)',ERR=9999) NO
      goto 9999
c
 7008 continue
      write(ioerr,'(/,3A)',ERR=9999) 'ERROR: Reading ',
     &      keywrd(:strmin(keywrd)),' packet of USERIN file.'
      write(ioerr,'(2A)',ERR=9999)
     &         'Invalid default fire class specifed: ',
     &                                       string(21:strmin(string))
      write(ioerr,'(A,I2,//)',ERR=9999) 'Valid range is: 1 - ', nfbin
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
 9001 format(/,T20,A,A40,/)
 9002 format(T10,A,T40,:,':',A)
 9003 format(T10,A,T40,:,':',I5,:,I5,:,I5)
 9004 format(T10,A,T40,:,':',F7.3)
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
