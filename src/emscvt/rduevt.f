C**** RDUEVT.F 
c 
      subroutine rduevt( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the user input file and stores the data in common variables
c    to be used by the EMSCVT program.
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
      include 'ioemscvt.inc'
      include 'usremcvt.inc'
      include 'emcvtspl.inc'
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
c   TOGNAM  C   parameter for matching input emissions flag
c   VOCNAM  C   parameter for matching input emissions flag
c
      character*3 TOGNAM
      character*3 VOCNAM
c
      parameter( TOGNAM = 'TOG')
      parameter( VOCNAM = 'VOC')
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c   critmp  C   temporary string for criteria polluatant names
c   deftmp  R   temporary variable for splitting factor
c   moltmp  R   temporary variable for molecular weight
c   spctmp  C   temporary string for species names
c
      character*(2*MXSTR) string, respon
      character*40        cdate
      character*10        spctmp
      character*5         critmp
      integer*4           irec, i
      real*4              moltmp, deftmp
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
c   --- read range of valid weights ---
c
      irec = irec + 1
      read(IORUIN,'(A)',ERR=7000,END=7001) string 
      read(string,'(20X,F10.0)',ERR=7002) epslon
      epslon = ABS(epslon)
c
c   --- read input emissions flag ---
c
      irec = irec + 1
      read(IORUIN,'(20X,A)',ERR=7000) string
      call low2up( string )
      call lftjst( string )
      if( string(1:3) .EQ. VOCNAM ) then
          lvocin = .TRUE.
      else if( string(1:3) .EQ. TOGNAM ) then
          lvocin = .FALSE.
      else
          goto 7003
      endif
c
c   --- read output emissions flag ---
c
      irec = irec + 1
      read(IORUIN,'(20X,A)',ERR=7000) string
      call low2up( string )
      call lftjst( string )
      if( string(1:3) .EQ. VOCNAM ) then
          lvocout = .TRUE.
      else if( string(1:3) .EQ. TOGNAM ) then
          lvocout = .FALSE.
      else
          goto 7004
      endif
c
c   --- read species data ---
c
  111 continue
      irec = irec + 1
      read(IORUIN,'(A)',ERR=7000,END=222) string 
      read(string(21:),8000,ERR=7000) spctmp, moltmp, critmp, deftmp
      nspecs = nspecs + 1
      if( nspecs .GT. MXSPEC ) goto 7005
      call low2up( spctmp )
      call lftjst( spctmp )
      spclst(nspecs) = spctmp
      call low2up( critmp )
      call lftjst( critmp )
      spcrit(nspecs) = critmp
      molwt(nspecs) = moltmp
      defspl(nspecs) = deftmp
      goto 111
c
c   --- echo data to message file ---
c
  222 continue
      call getime( cdate )
      write(IOWMSG,'(/,T20,A,A40,/)',ERR=7006) VERSON, cdate
      write(IOWMSG,9000,ERR=7006) 'Range for valid profile',1.0-epslon,
     &                                                 1.0+epslon
      if( lvocin ) then
         write(IOWMSG,9003,ERR=7001) 'Input emissions should be ',VOCNAM
      else
         write(IOWMSG,9003,ERR=7001) 'Input emissions should be ',TOGNAM
      endif
      if( lvocout ) then
         write(IOWMSG,9003,ERR=7001) 'Output emissions will be ',VOCNAM
      else
         write(IOWMSG,9003,ERR=7001) 'Output emissions will be ',TOGNAM
      endif
      write(IOWMSG,9002,ERR=7006) 'Species name ','Molecular wt.',
     &                                   '   Criteria  ','Default split'
      write(IOWMSG,9003,ERR=7006) ('-',i=1,18*4)
      do i=1,nspecs
         write(IOWMSG,9004,ERR=7006) spclst(i),molwt(i),spcrit(i),
     &                                                       defspl(i)
      enddo
      write(IOWMSG,9003,ERR=7006) ('-',i=1,18*4)
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
      write(ioerr,'(/,2A,I8)',ERR=9999) 'ERROR: Reading user input',
     &                                           ' file at line: ',irec
      write(ioerr,'(2A,//)',ERR=9999)  'File: ',uinfl(:strmin(uinfl))
      goto 9999
c
 7001 continue
      write(ioerr,'(/,2A,I8)',ERR=9999) 'ERROR: Reading user input',
     &                                           ' file at line: ',irec
      write(ioerr,'(2A)',ERR=9999)  'File: ',uinfl(:strmin(uinfl))
      write(ioerr,'(A,//)',ERR=9999) 'Premature end-of-file reached.'
      goto 9999
c
 7002 continue
      write(ioerr,'(/,2A,I8)',ERR=9999) 'ERROR: Reading user input',
     &                                           ' file at line: ',irec
      write(ioerr,'(2A)',ERR=9999)  'File: ',uinfl(:strmin(uinfl))
      write(ioerr,'(A)',ERR=9999) 'Line read:'
      write(ioerr,'(A,//)',ERR=9999) string(:strmin(string))
      goto 9999
c
 7003 continue
      write(ioerr,'(/,A)',ERR=9999) 'ERROR: Reading user input file.'
      write(ioerr,'(2A)',ERR=9999)
     &             'Invalid response to input emissions flag: ',
     &                                      string(21:strmin(string))
      write(ioerr,'(A)',ERR=9999) 'Valid responses are:'
      write(ioerr,'(10X,A)',ERR=9999) VOCNAM
      write(ioerr,'(10X,A)',ERR=9999) TOGNAM
      goto 9999
c
 7004 continue
      write(ioerr,'(/,A)',ERR=9999) 'ERROR: Reading user input file.'
      write(ioerr,'(2A)',ERR=9999)
     &             'Invalid response to output emissions flag: ',
     &                                      string(21:strmin(string))
      write(ioerr,'(A)',ERR=9999) 'Valid responses are:'
      write(ioerr,'(10X,A)',ERR=9999) VOCNAM
      write(ioerr,'(10X,A)',ERR=9999) TOGNAM
      goto 9999
c
 7005 continue
      write(ioerr,'(/,A)',ERR=9999) 'ERROR: Reading user input file.'
      write(ioerr,'(A)',ERR=9999) 'Number of species exceeds max.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maxumium allowed : ',MXSPEC
      write(ioerr,'(3A,//)',ERR=9999) 'Increase the parameter MXSPEC ',
     &                                                  'and recompile.'
      goto 9999
c
 7006 continue
      ioerr = IOWSTD
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Writing message file.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',msgfl(:strmin(msgfl))
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
 8000 format(A10,1X,F5.0,1X,A5,1X,F10.0)
 8001 format(A)
 9000 format(T10,A,T40,:,':','/',F8.4,',',F8.4,'/')
 9001 format(T10,A,T40,:,':',A)
 9002 format(//,T10,4(A,5X))
 9003 format(T10,250(:,A))
 9004 format(T10,1X,A10,5X,F10.2,14X,A5,11X,E10.4)
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
