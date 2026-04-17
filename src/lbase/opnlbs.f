C**** OPNLBS.F 
c
      subroutine opnlbs( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the filenames to be used in the LBASE program and opens all
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
      include 'iolbase.inc'
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
c   cdate   C   character string for date
c   ilen    I   variable to store length of filename
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
      read(IORSTD,'(20X,A)',ERR=7000,END=7001) uinpfl
      call lftjst( uinpfl )
      write(IOWSTD,'(2A)') 'Read: ',uinpfl(:strmin(uinpfl))
c
      action = 'Reading input link emissions filename.'
      write(IOWSTD,'(/,A)',ERR=9999) 
     &                       'Enter input link emissions filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7001) linkfl
      call lftjst( linkfl )
      write(IOWSTD,'(2A)') 'Read: ',linkfl(:strmin(linkfl))
c
      action = 'Reading output message filename.'
      write(IOWSTD,'(/,A)',ERR=9999) 'Enter output message filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7001) msgfl
      call lftjst( msgfl )
      write(IOWSTD,'(2A)') 'Read: ',msgfl(:strmin(msgfl))
c
      action = 'Reading output EMBR filename.'
      write(IOWSTD,'(/,A)',ERR=9999) 'Enter output EMBR filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7001) embrfl
      call lftjst( embrfl )
      write(IOWSTD,'(2A)') 'Read: ',embrfl(:strmin(embrfl))
c
c   --- check for existence of input files ---
c
      action = 'Reading USERIN filename.'
      fname = uinpfl
      inquire(file=uinpfl,exist=lcheck)
      if( .NOT. lcheck ) goto 7002
c
      action = 'Reading input link emissions filename.'
      fname = linkfl
      inquire(file=linkfl,exist=lcheck)
      if( .NOT. lcheck ) goto 7002
c
c   --- check for output files that already exist ---
c
      action = 'Reading output message filename.'
      fname = msgfl
      inquire(file=msgfl,exist=lcheck)
      if( lcheck ) goto 7003
c
      action = 'Reading output EMBR filename.'
      fname = embrfl
      inquire(file=embrfl,exist=lcheck)
      if( lcheck ) goto 7003
c
c   --- open files ---
c
      action = 'Opening USERIN file.'
      fname = uinpfl
      open(unit=IORINP,file=uinpfl,ERR=7004)
c
      action = 'Opening input link emissions file.'
      fname = linkfl
      open(unit=IORLNK,file=linkfl,ERR=7004)
c
      action = 'Opening output message file.'
      fname = msgfl
      open(unit=IOWMSG,file=msgfl,ERR=7004)
c
      action = 'Opening output EMBR file.'
      fname = embrfl
      open(unit=IOWMBR,file=embrfl,form='UNFORMATTED',ERR=7004)
c
c   --- echo program version and filenames to message file ---
c
      ioerr = IOWMSG
      call getime(cdate)
      write(IOWMSG,'(/,T20,A,A40,/)',ERR=7004) VERSON, cdate
      write(IOWMSG,9000,ERR=7005) '    Input Files'
c
      write(IOWMSG,9000,ERR=7005) 'USERIN file',uinpfl(:strmin(uinpfl))
c
      write(IOWMSG,9000,ERR=7005) 'link emissions file',
     &                                        linkfl(:strmin(linkfl))
c
      write(IOWMSG,'(A)',ERR=7005)
      write(IOWMSG,9000,ERR=7005) '    Output Files'
      write(IOWMSG,9000,ERR=7005) 'Output EMBR file',
     &                                        embrfl(:strmin(embrfl))
c
      write(IOWMSG,'(A)',ERR=7005)
c
c   ---- set error flag to sucess ---
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
      write(IOWSTD,'(2A,//)',ERR=9999) 'Not enough filenames supplied.'
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
