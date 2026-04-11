C**** OPNPST.F 
c
      subroutine opnpst( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the filenames to be used in the PSTPNT program and opens all
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
      include 'iopstpnt.inc'
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
      lstksfl = .TRUE.
      ioerr = IOWSTD
c
c   --- read all filenames from standard input ---
c
      action = 'Reading USERIN filename.'
      write(IOWSTD,'(/,2A)',ERR=9999) 'Enter USERIN filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7006) uinpfl
      call lftjst( uinpfl )
      write(IOWSTD,'(2A)') 'Read: ',uinpfl(:strmin(uinpfl))
c
      action = 'Reading input EMBR filename.'
      write(IOWSTD,'(/,2A)',ERR=9999) 'Enter input EMBR filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7006) imbrfl
      call lftjst( imbrfl )
      write(IOWSTD,'(2A)') 'Read: ',imbrfl(:strmin(imbrfl))
c
      action = 'Reading output message filename.'
      write(IOWSTD,'(/,2A)',ERR=9999) 'Enter message filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7006) msgfl
      call lftjst( msgfl )
      write(IOWSTD,'(2A)') 'Read: ',msgfl(:strmin(msgfl))
c
      action = 'Reading output emissions filename.'
      write(IOWSTD,'(/,2A)',ERR=9999) 'Enter output emissions filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7006) oemsfl
      call lftjst( oemsfl )
      write(IOWSTD,'(2A)') 'Read: ',oemsfl(:strmin(oemsfl))
c
      action = 'Reading output stack header filename.'
      write(IOWSTD,'(/,2A)',ERR=9999) 
     &                            'Enter output stack header filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7006) ohdrfl
      call lftjst( ohdrfl )
      write(IOWSTD,'(2A)') 'Read: ',ohdrfl(:strmin(ohdrfl))
c
      action = 'Reading output stack/SCC totals filename.'
      write(IOWSTD,'(/,2A)',ERR=9999) 'Enter STACK/SCC filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7006) stksfl
      call lftjst( stksfl )
      if( strlen(stksfl) .EQ. 0 ) then
          lstksfl = .FALSE.
          write(IOWSTD,'(A)',ERR=9999) 'No file supplied.'
      else
          write(IOWSTD,'(2A)') 'Read: ',stksfl(:strmin(stksfl))
      endif
c
      action = 'Reading output error filename.'
      write(IOWSTD,'(/,2A)',ERR=9999) 
     &                    'Enter EMAR formatted error filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7005) errfl
      call lftjst( errfl )
      write(IOWSTD,'(2A)') 'Read: ',errfl(:strmin(errfl))
c
c   --- check for existence of input files ---
c
      fname = uinpfl
      action = 'Opening USERIN file.'
      inquire(file=uinpfl,exist=lcheck)
      if( .NOT. lcheck ) goto 7001
c
      fname = imbrfl
      action = 'Opening input EMBR file.'
      inquire(file=imbrfl,exist=lcheck)
      if( .NOT. lcheck ) goto 7001
c
c   --- check for output files that already exist ---
c
      fname = msgfl
      action = 'Opening output message file.'
      inquire(file=msgfl,exist=lcheck)
      if( lcheck ) goto 7002
c
      fname = oemsfl
      action = 'Opening output emissions file.'
      inquire(file=oemsfl,exist=lcheck)
      if( lcheck ) goto 7002
c
      fname = ohdrfl
      action = 'Opening output stack header file.'
      inquire(file=ohdrfl,exist=lcheck)
      if( lcheck ) goto 7002
c
      if( lstksfl ) then
         fname = stksfl
         action = 'Opening output stack/SCC totals file.'
         inquire(file=stksfl,exist=lcheck)
         if( lcheck ) goto 7002
      endif
c
      fname = errfl
      action = 'Opening output error file.'
      inquire(file=errfl,exist=lcheck)
      if( lcheck ) goto 7002
c
c   --- open files ---
c
      fname = uinpfl
      action = 'Opening USERIN file.'
      open(IORINP,file=uinpfl,status='OLD',ERR=7003)
c
      fname = imbrfl
      action = 'Opening input EMBR file.'
      open(IORMBR,file=imbrfl,form='UNFORMATTED',status='OLD',ERR=7003)
c
      fname = msgfl
      action = 'Opening output message file.'
      open(unit=IOWMSG,file=msgfl,status='UNKNOWN',ERR=7003)
c
      fname = oemsfl
      action = 'Opening output emissions file.'
      open(unit=IOWEMS,file=oemsfl,status='UNKNOWN',ERR=7003)
c
      fname = ohdrfl
      action = 'Opening output stack header file.'
      open(unit=IOWHDR,file=ohdrfl,status='UNKNOWN',ERR=7003)
c
      if( lstksfl ) then
        fname = stksfl
        action = 'Opening output stack/SCC totals file.'
        open(unit=IOWSTK,file=stksfl,form='UNFORMATTED',ERR=7003)
      endif
c
      fname = errfl
      action = 'Opening output error file.'
      open(unit=IOWERR,file=errfl,status='UNKNOWN',ERR=7003)
c
c  --- echo the filenames ---
c
      ioerr = IOWMSG
      call getime( cdate )
      write(IOWMSG,'(/,T20,A,A40,/)',ERR=7004) VERSON, cdate
      write(IOWMSG,9000,ERR=7004) '    Input Files'
      write(IOWMSG,9000,ERR=7004) 'USERIN file',uinpfl(:strmin(uinpfl))
      write(IOWMSG,9000,ERR=7004) 
     &                        'Input EMBR file',imbrfl(:strmin(imbrfl))
c
      write(IOWMSG,9000,ERR=7004)
      write(IOWMSG,9000,ERR=7004) '    Output Files'
c
      write(IOWMSG,9000,ERR=7004)
     &                          'Emissions file',oemsfl(:strmin(oemsfl))
c
      write(IOWMSG,9000,ERR=7004)
     &                       'Stack Header file',ohdrfl(:strmin(ohdrfl))
c
      if( lstksfl ) then
         write(IOWMSG,9000,ERR=7004) 
     &                        'STACKS/SCC file',stksfl(:strmin(stksfl))
      else
         write(IOWMSG,9000,ERR=7004) 'STACKS/SCC file',' Not requested.'
      endif
c
      write(IOWMSG,9000,ERR=7004) 'ERROR file',errfl(:strmin(errfl))
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
      write(ioerr,'(/,A)',ERR=9999) 'ERROR reading filenames.'
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
      write(IOWSTD,'(2A)',ERR=9999) 'File: ',msgfl(:strmin(msgfl))
      goto 9999
c
 7005 continue
      write(ioerr,'(/,A)',ERR=9999) 'ERROR reading filenames.'
      write(ioerr,'(A)',ERR=9999) action(:strmin(action))
      write(IOWSTD,'(2A,//)',ERR=9999) 'This version requires an ',
     &  'additional output file:  the ASC/FIPS emissions totals file.'
      goto 9999
c
 7006 continue
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
