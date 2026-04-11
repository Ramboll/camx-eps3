C*** OPNTMP.F 
c
      subroutine opntmp( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the filenames to be used in the TMPRL program and opens all
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
      include 'iotmprl.inc'
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
c
c   --- read all filenames from standard input ---
c
      action = 'Reading USERIN filename.'
      write(IOWSTD,'(/,A)',ERR=9999) 'Enter USERIN filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7005) uinpfl
      call lftjst( uinpfl )
      write(IOWSTD,'(2A)',ERR=9999) 'Read: ',uinpfl(:strmin(uinpfl))
c
      action = 'Reading input EMBR filename.'
      write(IOWSTD,'(/,A)',ERR=9999) 'Enter input EMBR filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7005) imbrfl
      call lftjst( imbrfl )
      write(IOWSTD,'(2A)',ERR=9999) 'Read: ',imbrfl(:strmin(imbrfl))
c
      action = 'Reading ASC-SCC/temporal code cross reference filename.'
      write(IOWSTD,'(/,A)',ERR=9999)
     &      'Enter ASC-SCC/temporal code cross reference filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7005) xrffl
      call lftjst( xrffl )
      write(IOWSTD,'(2A)',ERR=9999) 'Read: ',xrffl(:strmin(xrffl))
c
      action = 'Reading temporal profiles filename.'
      write(IOWSTD,'(/,A)',ERR=9999) 
     &                             'Enter temporal profiles filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7005) prffl
      call lftjst( prffl )
      write(IOWSTD,'(2A)',ERR=9999) 'Read: ',prffl(:strmin(prffl))
c
      action = 'Reading message filename.'
      write(IOWSTD,'(/,A)',ERR=9999) 'Enter message filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7005) msgfl
      call lftjst( msgfl )
      write(IOWSTD,'(2A)',ERR=9999) 'Read: ',msgfl(:strmin(msgfl))
c
      action = 'Reading output EMBR filename.'
      write(IOWSTD,'(/,A)',ERR=9999) 'Enter output EMBR filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7005) ombrfl
      call lftjst( ombrfl )
      write(IOWSTD,'(2A)',ERR=9999) 'Read: ',ombrfl(:strmin(ombrfl))
c
      action = 'Reading error filename.'
      write(IOWSTD,'(/,A)',ERR=9999) 
     &                            'Enter EMAR formatted error filename:'
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
      fname = imbrfl
      action = 'Reading input EMBR filename.'
      inquire(file=imbrfl,exist=lcheck)
      if( .NOT. lcheck ) goto 7001
c
      fname = xrffl
      action = 'Reading ASC-SCC/temporal code cross reference filename.'
      inquire(file=xrffl,exist=lcheck)
      if( .NOT. lcheck ) goto 7001
c
      fname = prffl
      action = 'Reading temporal profiles filename.'
      inquire(file=prffl,exist=lcheck)
      if( .NOT. lcheck ) goto 7001
c
c   --- check for output files that already exist ---
c
      fname = msgfl
      action = 'Reading message filename.'
      inquire(file=msgfl,exist=lcheck)
      if( lcheck ) goto 7002
c
      fname = ombrfl
      action = 'Reading output EMBR filename.'
      inquire(file=ombrfl,exist=lcheck)
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
      action = 'Opening USERIN file.'
      open(unit=IORINP,file=uinpfl,status='OLD',ERR=7003)
c
      fname = imbrfl
      action = 'Opening input EMBR file.'
      open(unit=IORMBR,file=imbrfl,form='UNFORMATTED',
     &                                    status='OLD',ERR=7003)
c
      fname = xrffl
      action = 'Opening ASC-SCC/temporal code cross reference file.'
      open(unit=IORXRF,file=xrffl,status='OLD',ERR=7003)
c
      fname = prffl
      action = 'Opening temporal profiles file.'
      open(unit=IORPRF,file=prffl,status='OLD',ERR=7003)
c
      fname = msgfl
      action = 'Opening message file.'
      open(unit=IOWMSG,file=msgfl,status='UNKNOWN',ERR=7003)
c
      fname = ombrfl
      action = 'Opening output EMBR file.'
      open(unit=IOWMBR,file=ombrfl,form='UNFORMATTED',
     &                               status='UNKNOWN',ERR=7003)
c
      fname = errfl
      action = 'Opening error file.'
      open(unit=IOWERR,file=errfl,status='UNKNOWN',ERR=7003)
c
c   --- echo program version and filenames to message file ---
c
      ioerr = IOWMSG
      call getime( cdate )
      write(IOWMSG,'(/,T20,A,A40,/)',ERR=7004) VERSON, cdate
      write(IOWMSG,9000,ERR=7004) '    Input Files'
c
      write(IOWMSG,9000,ERR=7004) 'USERIN file',uinpfl(:strmin(uinpfl))
c
      write(IOWMSG,9000,ERR=7004) 'Input EMBR file',
     &                                          imbrfl(:strmin(imbrfl))
c
      write(IOWMSG,9000,ERR=7004) 'ASC-SCC/temporal code xref file',
     &                                          xrffl(:strmin(xrffl))
c
      write(IOWMSG,9000,ERR=7004) 'Temporal profiles file',
     &                                          prffl(:strmin(prffl))
c
      write(IOWMSG,9000,ERR=7004)
      write(IOWMSG,9000,ERR=7004) '    Output Files'
      write(IOWMSG,9000,ERR=7004) 'Output EMBR file',
     &                                          ombrfl(:strmin(ombrfl))
c
      write(IOWMSG,9000,ERR=7004) 'EMAR error file',
     &                                          errfl(:strmin(errfl))
c
      write(IOWMSG,'(A)',ERR=7004)
c
c   ---- set error flag to sucess ---
c
      curifl = imbrfl
      curofl = ombrfl
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
