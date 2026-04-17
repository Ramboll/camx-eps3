C**** OPNRET.F 
c
      subroutine opnret( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the filenames to be used in the EMBRET program and opens all
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
      include 'ioembret.inc'
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
      logical           lcheck
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c   --- assume using a user input file ---
c
      lusrin = .TRUE.
      ioerr = IOWSTD
c
c   --- read all filenames from standard input ---
c
      action = 'Reading user input filename.'
      write(IOWSTD,'(/,A)',ERR=9999) 'Enter user input filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7001) uinpfl
      call lftjst( uinpfl )
      if( strlen(uinpfl) .EQ. 0 ) then
          lusrin = .FALSE.
          write(IOWSTD,'(A)',ERR=9999) 'No file supplied.'
      else
          write(IOWSTD,'(2A)'),'Read: ',uinpfl(:strmin(uinpfl))
      endif
c
      action = 'Reading input EMBR filename.'
      write(IOWSTD,'(/,A)',ERR=9999) 'Enter input EMBR filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7001) embrfl
      call lftjst( embrfl )
      write(IOWSTD,'(2A)'),'Read: ',embrfl(:strmin(embrfl))
c
      action = 'Reading output message filename.'
      write(IOWSTD,'(/,A)',ERR=9999) 'Enter message filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7001) msgfl
      call lftjst( msgfl )
      write(IOWSTD,'(2A)'),'Read: ',msgfl(:strmin(msgfl))
c
      action = 'Reading output EMAR filename.'
      write(IOWSTD,'(/,A)',ERR=9999) 'Enter EMAR formatted filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7001) emarfl
      call lftjst(  emarfl )
      write(IOWSTD,'(2A)'),'Read: ',emarfl(:strmin(emarfl))
c
c   --- check for existence of input files ---
c
      if( lusrin ) then
          action = 'Reading user input filename.'
          call lftjst( uinpfl )
          fname = uinpfl
          inquire(file=uinpfl,exist=lcheck)
          if( .NOT. lcheck ) goto 7002
      endif
c
      action = 'Reading input EMBR filename.'
      fname = embrfl
      inquire(file=embrfl,exist=lcheck)
      if( .NOT. lcheck ) goto 7002
c
c   --- check for output files that already exist ---
c
      action = 'Reading output message filename.'
      fname = msgfl
      inquire(file=msgfl,exist=lcheck)
      if( lcheck ) goto 7003
c
      action = 'Reading output EMAR filename.'
      fname = emarfl
      inquire(file=emarfl,exist=lcheck)
      if( lcheck ) goto 7003
c
c   --- open files ---
c
      if( lusrin ) then
         action = 'Opening user input file.'
         fname = uinpfl
         open(unit=IORINP,file=uinpfl,ERR=7004,status='OLD')
      endif
c
      action = 'Opening input EMBR file.'
      fname = embrfl
      open(unit=IORMBR,file=embrfl,form='UNFORMATTED',
     &                                          ERR=7004,status='OLD')
c
      action = 'Opening output message file.'
      fname = msgfl
      open(unit=IOWMSG,file=msgfl,ERR=7004,status='UNKNOWN')
c
      action = 'Opening output EMAR file.'
      fname = emarfl
      open(unit=IOWEMR,file=emarfl,ERR=7004,status='UNKNOWN')
c
c   --- echo filenames ---
c
      call getime( cdate )
      ioerr = IOWMSG
      write(IOWMSG,'(/,T20,A,A40,/)',ERR=7005) VERSON, cdate
      write(IOWMSG,9000,ERR=7005) '    Input Files'
c
      if( lusrin ) then
         write(IOWMSG,9000,ERR=7005) 'User input file',
     &                                          uinpfl(:strmin(uinpfl))
      else
         write(IOWMSG,9000,ERR=7005) 'No user input file.',
     &                         '  Entire input file will be retrieved.'
      endif
c
      write(IOWMSG,9000,ERR=7005) 'Input EMBR file',
     &                                          embrfl(:strmin(embrfl))
c
      write(IOWMSG,9000,ERR=7005)
      write(IOWMSG,9000,ERR=7005) '    Output Files'
      write(IOWMSG,9000,ERR=7005) 'Output EMAR file',
     &                                          emarfl(:strmin(emarfl))
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
