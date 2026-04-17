c
      subroutine opnpts2ems( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the filenames to be used in the PTS2EMS_NCF program and 
c    opens all files
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
      include 'iopts2ems.inc'
      include 'ncf_iodat.inc'
      include 'netcdf.inc'
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
c   strlen      I   returns the actual length of a string
c   strmin      I   returns the actual length of a string (min of 1)
c   ncf_chkfile I checks if the file is NetCDF format
c
      integer*4 strlen
      integer*4 strmin
      integer*4 ncf_chkfile
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c   fname   C   character string for temporary storage of filenames
c   ilen    I   variable to store length of filename
c   lcheck  L   logical to check for existence of files
c
      character*(MXSTR) fname, action
      character*40      cdate
      character*10      cname
      character*1       comma
      integer*4         jerr, itmp, i
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
c --- set compression flag ---
c
      call ncf_set_compress_flag()
c
      ierr = IFAIL
      ioerr = IOWSTD
c
c   --- read all filenames from standard input ---
c
      action = 'Reading USERIN filename.'
      write(IOWSTD,'(/,A)',ERR=9999) 'Enter USERIN filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7000) uinpfl
      call lftjst( uinpfl )
      write(IOWSTD,'(2A)') 'Read: ',uinpfl(:strmin(uinpfl))
c
      action = 'Reading input point source filename.'
      write(IOWSTD,'(/,A)',ERR=9999) 'Enter input point source filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7000) inptsfl
      call lftjst( inptsfl )
      write(IOWSTD,'(2A)') 'Read: ',inptsfl(:strmin(inptsfl))
c
      action = 'Reading message filename.'
      write(IOWSTD,'(/,A)',ERR=9999) 'Enter message filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7000) msgfl
      call lftjst( msgfl )
      write(IOWSTD,'(2A)') 'Read: ',msgfl(:strmin(msgfl))
c
      action = 'Reading output emissions filename.'
      write(IOWSTD,'(/,A)',ERR=9999) 'Enter output emissions filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7000) oemsfl
      call lftjst( oemsfl )
      write(IOWSTD,'(2A)') 'Read: ',oemsfl(:strmin(oemsfl))
c
c   --- check for existence of input files ---
c
  111 continue
      action = 'Reading USERIN filename.'
      fname = uinpfl
      inquire(file=uinpfl,exist=lcheck)
      if( .NOT. lcheck ) goto 7001
c
      action = 'Reading input point source filename.'
      fname = inptsfl
      inquire(file=inptsfl,exist=lcheck)
      if( .NOT. lcheck ) goto 7001
c
c   --- check for output files that already exist ---
c
      action = 'Reading message filename.'
      fname = msgfl
      inquire(file=msgfl,exist=lcheck)
      if( lcheck ) goto 7002
c
      action = 'Reading output emissions filename.'
      fname = oemsfl
      inquire(file=oemsfl,exist=lcheck)
      if( lcheck ) goto 7002
c
c   --- open files ---
c
      action = 'Opening USERIN file.'
      fname = uinpfl
      open(IORINP,file=uinpfl,status='OLD',ERR=7003)
c
      action = 'Opening input point source file.'
      lnetcdf_in = .FALSE.
      cname = 'PTSOURCE  '
      jerr = ncf_chkfile(inpunit,inptsfl,action,cname)
      if( jerr .EQ. ISUCES ) then
         open(unit=inpunit,file=inptsfl,form='UNFORMATTED',
     &                                          status='OLD',ERR=7003)
      else
         lnetcdf_in = .TRUE.
          jerr = nf_open(inptsfl,NF_NOWRITE,inpunit)
         if( jerr .NE. ISUCES ) goto 7003
      endif
c
      action = 'Opening message file.'
      fname = msgfl
      open(unit=IOWMSG,file=msgfl,status='UNKNOWN',ERR=7003)
c
      action = 'Opening output NetCDF file.'
      call ncf_createfile(jerr,oemsfl,action,ncf_iowemis)
      if( jerr .NE. ISUCES ) goto 7003
c
c   --- echo filenames ---
c
      ioerr = IOWMSG
      call getime( cdate )
      write(IOWMSG,'(/,T20,A,A40,/)',ERR=7004) VERSON, cdate
      write(IOWMSG,9000,ERR=7004) '    Input Files'
c
      write(IOWMSG,9000,ERR=7004) 'USERIN file',uinpfl(:strmin(uinpfl))
      write(IOWMSG,9000,ERR=7004) 'Input point source file',
     &                                          inptsfl(:strmin(inptsfl))
      write(IOWMSG,9000,ERR=7004)
      write(IOWMSG,9000,ERR=7004) '    Output Files'
      write(IOWMSG,9000,ERR=7004) 'Gridded NetCDF file',
     &                                          oemsfl(:strmin(oemsfl))
c
c  --- set error flag to success ----
c
      ierr = ISUCES
      goto 9999
c
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
