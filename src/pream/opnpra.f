C**** OPNPRA.F 
c
      subroutine opnpra( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the filenames to be used in the PREAM program and opens all
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
      include 'embrdat.inc'
      include 'iopream.inc'
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
c   strmin  I   returns the actual length of a string
c
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
c   Data statements:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      ioerr = IOWSTD
c
c   --- read all filenames from standard input ---
c
      action = 'Reading USERIN filename.'
      write(IOWSTD,'(/,2A)',ERR=9999) 'Enter USERIN filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7005) uinpfl
      call lftjst( uinpfl )
      write(IOWSTD,'(2A)') 'Read: ',uinpfl(:strmin(uinpfl))
c
      action = 'Reading input AMS filename.'
      write(IOWSTD,'(/,2A)',ERR=9999) 'Enter input AMS filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7005) amsfl
      call lftjst( amsfl )
      write(IOWSTD,'(2A)') 'Read: ',amsfl(:strmin(amsfl))
c
      action = 'Reading message filename.'
      write(IOWSTD,'(/,2A)',ERR=9999) 'Enter message filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7005) msgfl
      call lftjst( msgfl )
      write(IOWSTD,'(2A)') 'Read: ',msgfl(:strmin(msgfl))
c
      action = 'Reading output EMBR filename.'
      write(IOWSTD,'(/,2A)',ERR=9999) 'Enter output EMBR filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7005) outmfl
      call lftjst( outmfl )
      write(IOWSTD,'(2A)') 'Read: ',outmfl(:strmin(outmfl))
c
      action = 'Reading error filename.'
      write(IOWSTD,'(/,2A)',ERR=9999) 'Enter error filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7005) errfl
      call lftjst( errfl )
      write(IOWSTD,'(2A)') 'Read: ',errfl(:strmin(errfl))
c
c   --- check for existence of input files ---
c
      call lftjst( uinpfl )
      fname = uinpfl
      action = 'Reading USERIN filename.'
      inquire(file=uinpfl,exist=lcheck)
      if( .NOT. lcheck ) goto 7001
c
      call lftjst( amsfl )
      fname = amsfl
      action = 'Reading input AMS filename.'
      inquire(file=amsfl,exist=lcheck)
      if( .NOT. lcheck ) goto 7001
c
c   --- check for output files that already exist ---
c
      call lftjst( msgfl )
      fname = msgfl
      action = 'Reading message filename.'
      inquire(file=msgfl,exist=lcheck)
      if( lcheck ) goto 7002
c
      call lftjst( outmfl )
      fname = outmfl
      action = 'Reading output EMBR filename.'
      inquire(file=outmfl,exist=lcheck)
      if( lcheck ) goto 7002
c
      call lftjst( errfl )
      fname = errfl
      action = 'Reading error filename.'
      inquire(file=errfl,exist=lcheck)
      if( lcheck ) goto 7002
c
c   --- open files ---
c
      fname = uinpfl
      action = 'Opening USERIN file.'
      open(unit=IORINP,file=uinpfl,status='OLD',ERR=7003)
c
      fname = amsfl
      action = 'Opening input AMS file.'
      open(unit=IORAMS,file=amsfl,status='OLD',ERR=7003)
c
      fname = msgfl
      action = 'Opening message file.'
      open(unit=IOWMSG,file=msgfl,status='UNKNOWN',ERR=7003)
c
      fname = outmfl
      action = 'Opening output EMBR file.'
      open(unit=IOWMBR,file=outmfl,form='UNFORMATTED',
     &                                    status='UNKNOWN',ERR=7003)
c
      fname = errfl
      action = 'Opening error file.'
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
      write(IOWMSG,9000,ERR=7004) 'Input AMS file',amsfl(:strmin(amsfl))
c
      write(IOWMSG,9000,ERR=7004)
      write(IOWMSG,9000,ERR=7004) '    Output Files'
      write(IOWMSG,9000,ERR=7004) 'Output EMBR file',
     &                                      outmfl(:strmin(outmfl))
c
      write(IOWMSG,9000,ERR=7004) 'EMAR error file',
     &                                        errfl(:strmin(errfl))
c
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
