C***** RDULBS.F 
c
      subroutine rdulbs( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the user input file and stores the data in the /LBASE/
c    packet in common variables to be used by the LBASE program.
c
c    Argument Declaration.
c     outputs:
c       ierr  I error flag
c     inputs:
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
      include 'cntylst.inc'
      include 'uamregn.inc'
      include 'iolbase.inc'
      include 'usrlbase.inc'
      include 'lbasetot.inc'
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
c    Local Parameters:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c   jerr    I   error flag for subroutine calls
c   keywrd  C   temporary storage for seaching fo keyword
c   respon  C   response to input emissions flag
c
      character*(KEYLEN) keywrd, module, keyin
      character*(MXSTR)  respon
      character*40       cdate
      integer*4          irec, jerr
c
c-----------------------------------------------------------------------
c   data statements:
c-----------------------------------------------------------------------
c
      data keywrd /'/LBASE/'/
      data module /'LBASE'/
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
c   --- call routine to read control packet ---
c
      call rddate( jerr,
     &                   IORINP )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- call routine to read region packet ---
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
c   --- check to make sure the number of counties does not
c       exceed the smaller limitations of LBASE arrays ---
c
      if( ncdfip .GT. MXFIPCL ) goto 7000
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
      write(IOWMSG,9000,ERR=7001) VERSON, cdate
      write(IOWMSG,9001,ERR=7001) 'File note',fnote(:strmin(fnote))
      write(IOWMSG,9003,ERR=7001) 'Episode date (Calander)',ibgcal
      write(IOWMSG,9002,ERR=7001) 'Episode date (Julian)',ibgdat
      write(IOWMSG,9002,ERR=7001) 'Beginning hour',INT(begtim/100.)
      write(IOWMSG,9002,ERR=7001) 'Ending hour',INT(endtim/100.)
      write(IOWMSG,9004,ERR=7001) 'Grid origin (km)',xorig,yorig
      write(IOWMSG,9002,ERR=7001) 'UTM zone',izone
      write(IOWMSG,9004,ERR=7001) 'Grid cell width (km)',dxcell,dycell
      write(IOWMSG,9005,ERR=7001) 'Number of cells',nxcell,nycell
      write(IOWMSG,9002,ERR=7001) 'Number of counties',ncdfip
c
c   --- call routine to find /LBASE/ packet ---
c
      call fndkey( jerr,
     &                   IORINP, keywrd )
      if( jerr .EQ. IRDERR ) goto 9999
      if( jerr .NE. ISUCES ) goto 7002
c
c   --- read the packet variables ---
c
      irec = irec + 1
      read(IORINP,'(A)',ERR=7003,END=7004) respon
      keyin = respon(21:21+KEYLEN-1) 
      call lftjst( keyin )
      call low2up( keyin )
      if( keyin .EQ. GRMSTR ) then
          cvtunit = GRMCVT
      else if( keyin .EQ. TONSTR ) then
          cvtunit = TONCVT
      else
          goto 7005
      endif
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
c
 7000 continue
      write(ioerr,'(/,A)',ERR=9999) 'ERROR: Reading the USERIN file.'
      write(ioerr,'(2A)',ERR=9999) 'Number of counties exceeds ',
     &                               'the maximum for this program.'
      write(ioerr,'(2A)',ERR=9999) 'The number of counties allowed ',
     &          'in LBASE is considerably less than for other modules.'
      write(ioerr,'(A,I5)',ERR=9999) 'Counties in USERIN: ',ncdfip
      write(ioerr,'(A,I5)',ERR=9999) 'Maximum allowed   : ',MXFIPCL
      write(ioerr,'(2A,I5,//)',ERR=9999) '  Either strip down your ',
     &                 '/COUNTY/ packet or increase parameter: MXFIPCL'
      goto 9999
c
 7001 continue
      ioerr = IOWSTD
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Writing message file.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',msgfl(:strmin(msgfl))
      ierr = IFAIL
      goto 9999
c
 7002 continue
      write(ioerr,'(/,A)',ERR=9999) 'ERROR: Reading the USERIN file.'
      write(ioerr,'(3A,//)',ERR=9999) 'This program requires the ',
     &                     keywrd(:strmin(keywrd)),' packet of USERIN.'
      ierr = IFAIL
      goto 9999
c
 7003 continue
      write(ioerr,'(/,3A,I8,//)',ERR=9999) 'ERROR: Reading ',
     & keywrd(:strmin(keywrd)),' packet of USERIN file at line: ',irec
      goto 9999
c
 7004 continue
      write(ioerr,'(/,3A,I8)',ERR=9999) 'ERROR: Reading ',
     & keywrd(:strmin(keywrd)),' packet of USERIN file at line: ',irec
      write(ioerr,'(A,//)',ERR=9999) 'Premature end-of-file reached.'
      goto 9999
c
 7005 continue
      write(ioerr,'(/,3A,I8)',ERR=9999) 'ERROR: Reading ',
     &               keywrd(:strmin(keywrd)),' packet of USERIN file.'
      write(ioerr,'(2A)',ERR=9999)
     &         'Invalid response to input emissions units flag: ',
     &                                       respon(21:strmin(respon))
      write(ioerr,'(A)',ERR=9999) '  Acceptable respones are: '
      write(ioerr,'(12X,A)',ERR=9999) GRMSTR
      write(ioerr,'(12X,A,//)',ERR=9999) TONSTR
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
 9000 format(/,T20,A,A40,/)
 9001 format(T10,A,T40,:,':',A)
 9002 format(T10,A,T40,:,':',I5,:,I5)
 9003 format(T10,A,T40,:,':',I6)
 9004 format(T10,A,T40,:,': (',F10.3,',',F10.3,')')
 9005 format(T10,A,T40,:,': (',I5,',',I5,')')
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
