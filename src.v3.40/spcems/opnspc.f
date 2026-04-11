C**** OPNSPC.F 
c   
      subroutine opnspc( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the filenames to be used in the SPCEMS program and opens all
c    files
c
c    Argument Declaration.
c     Inputs:
c     Outputs:
c       ierr  I error flag
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
      include 'iospcems.inc'
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
c   strlen  I   returns the actual length of a string
c   strmin  I   returns the actual length of a string (min of 1)
c
      integer*4 strlen
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c   fname   C   character string for temporary storage of filenames
c   lcheck  L   logical to check for existence of files
c
      character*(MXSTR) fname, action
      logical           lcheck
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      ioerr = IOWSTD
      lcnvfl = .TRUE.
c
c   --- read all filenames from standard input ---
c
      action = 'Reading USERIN file.'
      write(IOWSTD,'(/,2A)',ERR=9999) 'Enter USERIN filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7005) uinpfl
      call lftjst( uinpfl )
      write(IOWSTD,'(2A)',ERR=9999) 'Read: ',uinpfl(:strmin(uinpfl))
c
      action = 'Reading input EMBR file.'
      write(IOWSTD,'(/,2A)',ERR=9999) 'Enter input EMBR filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7005) infl
      call lftjst( infl )
      write(IOWSTD,'(2A)',ERR=9999) 'Read: ',infl(:strmin(infl))
c
      action = 'Reading speciation profile file (RSPRO).'
      write(IOWSTD,'(/,2A)',ERR=9999) 
     &                  'Enter speciation profile filename (RSPRO):'
      read(IORSTD,'(20X,A)',ERR=7000,END=7005) spltfl
      call lftjst( spltfl )
      write(IOWSTD,'(2A)',ERR=9999) 'Read: ',spltfl(:strmin(spltfl))
c
      action = 'Reading cross reference file.'
      write(IOWSTD,'(/,2A)',ERR=9999)
     &      'Enter cross reference filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7005) xrffl
      call lftjst( xrffl )
      write(IOWSTD,'(2A)',ERR=9999) 'Read: ',xrffl(:strmin(xrffl))
c
      action = 'Reading pollutant conversion file.'
      write(IOWSTD,'(/,2A)',ERR=9999)
     &      'Enter pollutant conversion filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7005) cnvfl
      call lftjst( cnvfl )
      if( strlen( cnvfl ) .EQ. 0 ) then
         lcnvfl = .FALSE.
         write(IOWSTD,'(A)',ERR=9999) 'No file supplied.'
      else
         write(IOWSTD,'(2A)',ERR=9999) 'Read: ',cnvfl(:strmin(cnvfl))
      endif
c
      action = 'Reading message filename.'
      write(IOWSTD,'(/,2A)',ERR=9999) 'Enter message filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7005) msgfl
      call lftjst( msgfl )
      write(IOWSTD,'(2A)',ERR=9999) 'Read: ',msgfl(:strmin(msgfl))
c
      action = 'Reading output EMBR filename.'
      write(IOWSTD,'(/,2A)',ERR=9999) 'Enter output EMBR filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7005) outfl
      call lftjst( outfl )
      write(IOWSTD,'(2A)',ERR=9999) 'Read: ',outfl(:strmin(outfl))
c
      action = 'Reading error filename.'
      write(IOWSTD,'(/,2A)',ERR=9999) 'Enter error EMAR filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7005) errfl
      call lftjst( errfl )
      write(IOWSTD,'(2A)',ERR=9999) 'Read: ',errfl(:strmin(errfl))
c
c   --- check for existence of input files ---
c
      fname = uinpfl
      action = 'Reading USERIN filename.'
      inquire(file=uinpfl,exist=lcheck)
      if( .NOT. lcheck ) goto 7001
c
      fname = infl
      action = 'Reading input EMBR filename.'
      inquire(file=infl,exist=lcheck)
      if( .NOT. lcheck ) goto 7001
c
      fname = spltfl
      action = 'Reading speciation profile factors filename.'
      inquire(file=spltfl,exist=lcheck)
      if( .NOT. lcheck ) goto 7001
c
      fname = xrffl
      action = 'Reading cross reference filename.'
      inquire(file=xrffl,exist=lcheck)
      if( .NOT. lcheck ) goto 7001
c
      if( lcnvfl ) then
         fname = cnvfl
         action = 'Reading pollutant conversion filename.'
         inquire(file=cnvfl,exist=lcheck)
         if( .NOT. lcheck ) goto 7001
      endif
c
c   --- check for output files that already exist ---
c
      fname = msgfl
      action = 'Reading message filename.'
      inquire(file=msgfl,exist=lcheck)
      if( lcheck ) goto 7002
c
      fname = outfl
      action = 'Reading output filename filename.'
      inquire(file=outfl,exist=lcheck)
      if( lcheck ) goto 7002
c
      fname = errfl
      action = 'Reading error filename.'
      inquire(file=errfl,exist=lcheck)
      if( lcheck ) goto 7002
c
c   --- open files ---
c
      fname = uinpfl
      action = 'Opening error file.'
      open(unit=IORINP,file=uinpfl,status='OLD',ERR=7003)
c
      fname = infl
      action = 'Opening input EMBR file.'
      open(unit=IOREMB,file=infl,form='UNFORMATTED',
     &                                        status='OLD',ERR=7003)
c
      fname = spltfl
      action = 'Opening speciation factors file.'
      open(unit=IORSPL,file=spltfl,status='OLD',ERR=7003)
c
      fname = xrffl
      action = 'Opening cross reference file.'
      open(unit=IORXRF,file=xrffl,status='OLD',ERR=7003)
c
      if( lcnvfl ) then
         fname = cnvfl
         action = 'Opening pollutant conversion file.'
         open(unit=IORCNV,file=cnvfl,status='OLD',ERR=7003)
      endif
c
      fname = msgfl
      action = 'Opening message file.'
      open(unit=IOWMSG,file=msgfl,status='UNKNOWN',ERR=7003)
c
      fname = outfl
      action = 'Opening output EMBR file.'
      open(unit=IOWEMB,file=outfl,form='UNFORMATTED',
     &                                        status='UNKNOWN',ERR=7003)
c
      fname = errfl
      action = 'Opening error EMBR file.'
      open(unit=IOWERR,file=errfl,status='UNKNOWN',ERR=7003)
c
c   --- echo program version and filenames to message file ---
c
      ioerr = IOWMSG
      write(IOWMSG,'(/,T20,A,/)',ERR=7004) VERSON
      write(IOWMSG,9000,ERR=7004) '    Input Files'
c
      write(IOWMSG,9000,ERR=7004) 'USERIN file',uinpfl(:strmin(uinpfl))
c
      write(IOWMSG,9000,ERR=7004) 'Input EMBR file',infl(:strmin(infl))
c
      write(IOWMSG,9000,ERR=7004) 'Speciation factors file',
     &                                       spltfl(:strmin(spltfl))
c
      write(IOWMSG,9000,ERR=7004) 'Cross reference file',
     &                                            xrffl(:strmin(xrffl))
c
      if( lcnvfl ) then
         write(IOWMSG,9000,ERR=7004) 
     &                             'Conversion factors file',
     &                              cnvfl(:strmin(cnvfl))
      else
          write(IOWMSG,9000,ERR=7004) 
     &                     'No conversion data provided',
     &                     'No conversions will be applied.'
      endif
c
      write(IOWMSG,9000,ERR=7004)
      write(IOWMSG,9000,ERR=7004) '    Output Files'
      write(IOWMSG,9000,ERR=7004) 
     &                        'Output EMBR file',outfl(:strmin(outfl))
c
      write(IOWMSG,9000,ERR=7004) 
     &                        'EMAR error file',errfl(:strmin(errfl))
      write(IOWMSG,'(A)',ERR=7004) 
c
c  --- set error flag to success ----
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,A)',ERR=9999) 'ERROR: Reading filenames.'
      write(ioerr,'(A,//)',ERR=9999) action(:strmin(action))
      goto 9999
c
 7001 continue
      write(ioerr,'(/,2A)',ERR=9999)
     &            'ERROR: Input file not found: ',fname(:strmin(fname))
      write(ioerr,'(A,//)',ERR=9999) action(:strmin(action))
      goto 9999
c
 7002 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Output file exists: ',
     &                                           fname(:strmin(fname))
      write(ioerr,'(A)',ERR=9999) action(:strmin(action))
      write(ioerr,'(A,//)',ERR=9999)
     &                          'Please remove the file and try again.'
      goto 9999
c
 7003 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Opening file: ',
     &                                           fname(:strmin(fname))
      write(ioerr,'(A,//)',ERR=9999) action(:strmin(action))
      goto 9999
c
 7004 continue
      ioerr = IOWSTD
      write(IOWSTD,'(/,A)',ERR=9999) 'ERROR: Writing to message file.'
      write(IOWSTD,'(2A,//)',ERR=9999) 'File: ',msgfl(:strmin(msgfl))
      goto 9999
c
 7005 continue
      write(ioerr,'(/,A)',ERR=9999) 'ERROR reading filenames.'
      write(ioerr,'(A)',ERR=9999) action(:strmin(action))
      write(ioerr,'(A,//)',ERR=9999) 'Not enough filenames supplied.' 
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
 9000 format(T10,A,T40,:,':',A)
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
