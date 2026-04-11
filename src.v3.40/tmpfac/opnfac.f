C**** OPNFAC.F 
c
      subroutine opnfac( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the filenames to be used in the TMPFAC program and opens all
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
      include 'iotmpfac.inc'
c
c-----------------------------------------------------------------------
c    Argument Declerations:
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
      character*40      cdate
      logical*4         lcheck
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c   --- assume using all files ---
c
      lxref = .TRUE.
      lprof = .TRUE.
      lafs = .TRUE.
      ioerr = IOWSTD
c
c   --- read all filenames from standard input ---
c
      action = 'Reading input cross reference filename.'
      write(IOWSTD,'(/,A)',ERR=9999) 
     &                      'Enter input cross reference filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7001) inxrfl
      call lftjst( inxrfl )
      if( strlen(inxrfl) .EQ. 0 ) then
          lxref = .FALSE.
          write(IOWSTD,'(A)',ERR=9999) 'No file supplied.'
      else
          write(IOWSTD,'(2A)') 'Read: ',inxrfl(:strmin(inxrfl))
      endif
c
      action = 'Reading input temporal profiles filename.'
      write(IOWSTD,'(/,A)',ERR=9999)
     &                     'Enter input temporal profiles filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7001) inprfl
      call lftjst( inprfl )
      if( strlen(inprfl) .EQ. 0 ) then
          lprof = .FALSE.
          write(IOWSTD,'(A)',ERR=9999) 'No file supplied.'
      else
          write(IOWSTD,'(2A)') 'Read: ',inprfl(:strmin(inprfl))
      endif
c
      action = 'Reading input AFS filename.'
      write(IOWSTD,'(/,A)',ERR=9999) 'Enter input AFS filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7001) afsfl
      call lftjst( afsfl )
      if( strlen(afsfl) .EQ. 0 ) then
          lafs = .FALSE.
          write(IOWSTD,'(A)',ERR=9999) 'No file supplied.'
      else
          write(IOWSTD,'(2A)') 'Read: ',afsfl(:strmin(afsfl))
      endif
c
      action = 'Reading output message filename.'
      write(IOWSTD,'(/,A)',ERR=9999) 'Enter message filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7001) msgfl
      call lftjst( msgfl )
      write(IOWSTD,'(2A)') 'Read: ',msgfl(:strmin(msgfl))
c
      action = 'Reading output cross reference filename.'
      write(IOWSTD,'(/,A)',ERR=9999) 'Enter output XREF filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7001) oxrfl
      call lftjst( oxrfl )
      write(IOWSTD,'(2A)') 'Read: ',oxrfl(:strmin(oxrfl))
c
      action = 'Reading output temporal profiles filename.'
      write(IOWSTD,'(/,A)',ERR=9999)
     &                      'Enter output temporal profiles filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7001) oprfl
      call lftjst( oprfl )
      write(IOWSTD,'(2A)') 'Read: ',oprfl(:strmin(oprfl))
c
c   --- check for existence of input files ---
c
      if( lxref ) then
          action = 'Reading input cross reference filename.'
          fname = inxrfl
          inquire(file=inxrfl,exist=lcheck)
          if( .NOT. lcheck ) goto 7002
      endif
c
      if( lprof ) then
          action = 'Reading input temporal profiles filename.'
          fname = inprfl
          inquire(file=inprfl,exist=lcheck)
          if( .NOT. lcheck ) goto 7002
      endif
c
      if( lafs ) then
          action = 'Reading input AFS filename.'
          fname = afsfl
          inquire(file=afsfl,exist=lcheck)
          if( .NOT. lcheck ) goto 7002
      endif
c
c   --- check for output files that already exist ---
c
      action = 'Reading output message filename.'
      fname = msgfl
      inquire(file=msgfl,exist=lcheck)
      if( lcheck ) goto 7003
c
      action = 'Reading output cross reference filename.'
      fname = oxrfl
      inquire(file=oxrfl,exist=lcheck)
      if( lcheck ) goto 7003
c
      action = 'Reading output temporal profiles filename.'
      fname = oprfl
      inquire(file=oprfl,exist=lcheck)
      if( lcheck ) goto 7003
c
c   --- open files ---
c
      if( lxref ) then
         action = 'Opening input cross reference file.'
         fname = inxrfl
         open(unit=IORXRF,file=inxrfl,ERR=7004,status='UNKNOWN')
      endif
c
      if( lprof ) then
         action = 'Opening input temporal profiles file.'
         fname = inprfl
         open(unit=IORPRF,file=inprfl,ERR=7004,status='UNKNOWN')
      endif
c
      if( lafs ) then
         action = 'Opening input AFS file.'
         fname = afsfl
         open(unit=IORAFS,file=afsfl,ERR=7004,status='UNKNOWN')
      endif
c
      action = 'Opening output message file.'
      fname = msgfl
      open(unit=IOWMSG,file=msgfl,ERR=7004,status='UNKNOWN')
c
      action = 'Opening output cross reference file.'
      fname = oxrfl
      open(unit=IOWXRF,file=oxrfl,ERR=7004,status='UNKNOWN')
c
      action = 'Opening output temporal profiles file.'
      fname = oprfl
      open(unit=IOWPRF,file=oprfl,ERR=7004,status='UNKNOWN')
c
c   --- echo filenames ---
c
      ioerr = IOWMSG
      call getime( cdate )
      write(IOWMSG,'(/,T20,A,A40,/)',ERR=7005) VERSON, cdate
      write(IOWMSG,9000,ERR=7005) '    Input Files'
c
      if( lxref ) then
         write(IOWMSG,9000,ERR=7005) 'Input XREF file',
     &                                           inxrfl(:strmin(inxrfl))
      else
         write(IOWMSG,9000,ERR=7005) 'No input XREF file.'
      endif
c
      if( lprof ) then
         write(IOWMSG,9000,ERR=7005)'Input profiles file',
     &                                           inprfl(:strmin(inprfl))
      else
         write(IOWMSG,9000,ERR=7005) 'No input profiles file.'
      endif
c
      if( lafs ) then
         write(IOWMSG,9000,ERR=7005) 'Input AFS file',
     &                                           afsfl(:strmin(afsfl))
      else
         write(IOWMSG,9000,ERR=7005) 'No input AFS file.'
      endif
c
      write(IOWMSG,'(A)',ERR=7005)
      write(IOWMSG,9000,ERR=7005) '    Output Files'
      write(IOWMSG,9000,ERR=7005) 'Output XREF file',
     &                                           oxrfl(:strmin(oxrfl))
c
      write(IOWMSG,9000,ERR=7005) 'Output profiles file',
     &                                           oprfl(:strmin(oprfl))
      write(IOWMSG,'(A)',ERR=7005)
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
      write(ioerr,'(/,A)',ERR=9999) 'ERROR reading filenames.'
      write(ioerr,'(A,//)',ERR=9999) action(:strmin(action))
      goto 9999
c
 7001 continue
      write(ioerr,'(/,A)',ERR=9999) 'ERROR reading filenames.'
      write(ioerr,'(A)',ERR=9999) action(:strmin(action))
      write(ioerr,'(A,//)',ERR=9999) 'Not enough filenames supplied.'
      goto 9999
c
 7002 continue
      write(ioerr,'(/,2A)',ERR=9999)
     &            'ERROR: Input file not found: ',fname(:strmin(fname))
      write(ioerr,'(A,//)',ERR=9999) action(:strmin(action))
      goto 9999
c
 7003 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Output file exists: ',
     &                                           fname(:strmin(fname))
      write(ioerr,'(A)',ERR=9999) action(:strmin(action))
      write(ioerr,'(A,//)',ERR=9999)
     &                          'Please remove the file and try again.'
      goto 9999
c
 7004 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Opening file: ',
     &                                           fname(:strmin(fname))
      write(ioerr,'(A,//)',ERR=9999) action(:strmin(action))
      goto 9999
c
 7005 continue
      ioerr = IOWSTD
      write(IOWSTD,'(/,A)',ERR=9999) 'ERROR: Writing to message file.'
      write(IOWSTD,'(2A,//)',ERR=9999) 'File: ',msgfl(:strmin(msgfl))
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
