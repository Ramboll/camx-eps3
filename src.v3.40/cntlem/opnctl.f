C**** OPNCTL.F 
c
      subroutine opnctl( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the filenames to be used in the CNTLEM program and opens all
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
      include 'iocntlem.inc'
      include 'ctltot.inc'
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
      ioerr = IOWSTD
c
c   --- read all filenames from standard input ---
c
      action = 'Reading USERIN filename.'
      write(IOWSTD,'(/,A)',ERR=9999) 'Enter USERIN filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7001) uinpfl
      call lftjst( uinpfl )
      write(IOWSTD,'(2A)'),'Read: ',uinpfl(:strmin(uinpfl))
c
      action = 'Reading input EMBR filename.'
      write(IOWSTD,'(/,A)',ERR=9999) 'Enter input EMBR filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7001) imbrfl
      call lftjst( imbrfl )
      write(IOWSTD,'(2A)'),'Read: ',imbrfl(:strmin(imbrfl))
c
      action = 'Reading control factors filename.'
      write(IOWSTD,'(/,A)',ERR=9999)
     &                  'Enter control factors filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7001) ctlfl
      call lftjst( ctlfl )
      write(IOWSTD,'(2A)'),'Read: ',ctlfl(:strmin(ctlfl))
c
      action = 'Reading output message filename.'
      write(IOWSTD,'(/,A)',ERR=9999) 'Enter message filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7001) msgfl
      call lftjst( msgfl )
      write(IOWSTD,'(2A)'),'Read: ',msgfl(:strmin(msgfl))
c
      action = 'Reading output emissions filename.'
      write(IOWSTD,'(/,A)',ERR=9999) 'Enter output emissions filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7001) ombrfl
      call lftjst( ombrfl )
      write(IOWSTD,'(2A)'),'Read: ',ombrfl(:strmin(ombrfl))
c
c   --- check for existence of input files ---
c
      fname = uinpfl
      action = 'Reading USERIN filename.'
      inquire (file=uinpfl,exist=lcheck)
      if ( .NOT. lcheck ) goto 7002
c
      fname = imbrfl
      action = 'Reading input EMBR filename.'
      inquire (file=imbrfl,exist=lcheck)
      if ( .NOT. lcheck ) goto 7002
c
      fname = ctlfl
      action = 'Reading control factors filename.'
      inquire (file=ctlfl,exist=lcheck)
      if ( .NOT. lcheck ) goto 7002
c
c   --- check for output files that already exist ---
c
      fname = msgfl
      action = 'Reading output message filename.'
      inquire (file=msgfl,exist=lcheck)
      if ( lcheck ) goto 7003
c
      fname = ombrfl
      action = 'Reading output EMBR filename.'
      inquire (file=ombrfl,exist=lcheck)
      if ( lcheck ) goto 7003
c
c   --- open files ---
c
      fname = uinpfl
      action = 'Opening USERIN file.'
      open(IORINP,file=uinpfl,ERR=7004,status='UNKNOWN')
c
      fname = imbrfl
      action = 'Opening input EMBR file.'
      open (IORMBR,file=imbrfl,form='UNFORMATTED',
     &                                  status='UNKNOWN',ERR=7004)
c
      fname = ctlfl
      action = 'Opening control factors file.'
      open (IORCTL,file=ctlfl,ERR=7004,status='UNKNOWN')
c
      fname = msgfl
      action = 'Opening output message file.'
      open (unit=IOWMSG,file=msgfl,ERR=7006,status='UNKNOWN')
c
      fname = ombrfl
      action = 'Opening output EMBR file.'
      open (unit=IOWMBR,file=ombrfl,form='UNFORMATTED',
     &                                    status='UNKNOWN',ERR=7004)
c
c   --- echo filenames ---
c
      ioerr = IOWMSG
      call getime( cdate )
      write(IOWMSG,'(/,T20,A,A40,/)',ERR=7005) VERSON, cdate
      write (IOWMSG,9000,ERR=7005) '    Input Files'
c
      write (IOWMSG,9000,ERR=7005) 'USERIN file',
     &                                        uinpfl(:strmin(uinpfl))
c
      write (IOWMSG,9000,ERR=7005) 'Input EMBR file',
     &                                        imbrfl(:strmin(imbrfl))
c
      write (IOWMSG,9000,ERR=7005) 'Control factors file',
     &                                        ctlfl(:strmin(ctlfl))
c
      write(IOWMSG,'(A)',ERR=7005)
      write(IOWMSG,9000,ERR=7005) '    Output Files'
c
      write (IOWMSG,9000,ERR=7005) 'Output EMBR file',
     &                                        ombrfl(:strmin(ombrfl))
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
      write(IOWSTD,'(2A)',ERR=9999) 'File: ',msgfl(:strmin(msgfl))
      goto 9999
c
 7006 continue
      ioerr = IOWSTD
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Opening file: ',
     &                                           fname(:strmin(fname))
      write(ioerr,'(A)',ERR=9999) action(:strmin(action))
      write(ioerr,'(/,2A)',ERR=9999) 'This version no longer supports ',
     &                                              'the MADJIN file.'
      write(ioerr,'(2A,//)',ERR=9999) 'Remove that line from your ',
     &                                         'script and try again.'
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
