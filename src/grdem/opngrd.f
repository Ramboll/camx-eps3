C**** OPNGRD.F 
c
      subroutine opngrd( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the filenames to be used in the GRDEM program and opens all
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
      include 'iogrdem.inc'
      include 'usrgrdem.inc'
      include 'grdsrgrpt.inc'
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
c   ilen    I   variable to store length of filename
c   lcheck  L   logical to check for existence of files
c
      character*(MXSTR) fname, action
      character*40      cdate
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
      ierr = IFAIL
      ioerr = IOWSTD
c
c --- set compression flag ---
c
      call ncf_set_compress_flag()
c
c   --- assume using a user input file ---
c
      lsurg  = .TRUE.
      lnkdef = .TRUE.
      lascfl = .TRUE.
      lsrpt  = .TRUE.
c
c   --- read all filenames from standard input ---
c
      action = 'Reading USERIN filename.'
      write(IOWSTD,'(/,A)',ERR=9999) 'Enter USERIN filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7006) uinpfl
      call lftjst( uinpfl )
      write(IOWSTD,'(2A)') 'Read: ',uinpfl(:strmin(uinpfl))
c
      action = 'Reading input EMBR filename.'
      write(IOWSTD,'(/,A)',ERR=9999) 'Enter input EMBR filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7006) imbrfl
      call lftjst( imbrfl )
      write(IOWSTD,'(2A)') 'Read: ',imbrfl(:strmin(imbrfl))
c
      action = 'Reading surrogate cross reference filename.'
      write(IOWSTD,'(/,A)',ERR=9999)
     &                  'Enter surrogate cross reference filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7006) xrffl
      call lftjst( xrffl )
      if( strlen(xrffl) .EQ. 0 ) then
         lsurg = .FALSE.
         write(IOWSTD,'(A)',ERR=9999) 'No file supplied.'
      else
         write(IOWSTD,'(2A)')'Read: ', xrffl(:strmin(xrffl))
      endif
c
      action = 'Reading gridded surrogate data filename.'
      write(IOWSTD,'(/,A)',ERR=9999) 
     &                   'Enter gridded surrogate data filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7006) srgfl
      call lftjst( srgfl )
      if( strlen(srgfl) .EQ. 0 ) then
         lsurg = .FALSE.
         write(IOWSTD,'(A)',ERR=9999) 'No file supplied.'
      else
         write(IOWSTD,'(2A)') 'Read: ',srgfl(:strmin(srgfl))
      endif
c
      action = 'Reading link definition filename.'
      write(IOWSTD,'(/,A)',ERR=9999) 'Enter link definition filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7006) lnkfl
      call lftjst( lnkfl )
      if( strlen(lnkfl) .EQ. 0 ) then
         lnkdef = .FALSE.
         write(IOWSTD,'(A)',ERR=9999) 'No file supplied.'
      else
         write(IOWSTD,'(2A)') 'Read: ',lnkfl(:strmin(lnkfl))
      endif
c
      action = 'Reading message filename.'
      write(IOWSTD,'(/,A)',ERR=9999) 'Enter message filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7006) msgfl
      call lftjst( msgfl )
      write(IOWSTD,'(2A)') 'Read: ',msgfl(:strmin(msgfl))
c
      loutfile = .FALSE.
      action = 'Reading output emissions filename.'
      write(IOWSTD,'(/,A)',ERR=9999) 'Enter output emissions filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7006) oemsfl
      call lftjst( oemsfl )
      if( strlen(oemsfl) .EQ. 0 ) then
         write(IOWSTD,'(A)',ERR=9999) 'No file supplied.'
      else
         loutfile = .TRUE.
         write(IOWSTD,'(2A)') 'Read: ',oemsfl(:strmin(oemsfl))
      endif
c
      lnetcdf_out = .FALSE.
      action = 'Reading output NetCDF filename.'
      write(IOWSTD,'(/,A)',ERR=9999) 'Enter output NetCDF filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7006) oncffl
      call lftjst( oncffl )
      if( strlen(oncffl) .EQ. 0 ) then
         write(IOWSTD,'(A)',ERR=9999) 'No file supplied.'
      else
         lnetcdf_out = .TRUE.
         write(IOWSTD,'(2A)') 'Read: ',oncffl(:strmin(oncffl))
      endif
c
      action = 'Reading ASC/FIPS totals filename.'
      write(IOWSTD,'(/,A)',ERR=9999) 
     &                              'Enter ASC/FIPS emissions filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7006) ascfl
      call lftjst( ascfl )
      if( strlen(ascfl) .EQ. 0 ) then
         lascfl = .FALSE.
         write(IOWSTD,'(A)',ERR=9999) 'No file supplied.'
      else
         write(IOWSTD,'(2A)') 'Read: ',ascfl(:strmin(ascfl))
      endif
c
      action = 'Reading output error filename.'
      write(IOWSTD,'(/,A)',ERR=9999) 
     &                            'Enter EMAR formatted error filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7006) errfl
      call lftjst( errfl )
      write(IOWSTD,'(2A)') 'Read: ',errfl(:strmin(errfl))
c
      action = 'Reading surrogate report filename.'
      write(IOWSTD,'(/,A)',ERR=9999) 
     &                              'Enter surrogate report filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7006) srptfl
      call lftjst( srptfl )
      if( strlen(srptfl) .EQ. 0 ) then
         lsrpt = .FALSE.
         write(IOWSTD,'(A)',ERR=9999) 'No file supplied.'
      else
         write(IOWSTD,'(2A)') 'Read: ',srptfl(:strmin(srptfl))
      endif
c
c   --- check for existence of input files ---
c
  111 continue
      action = 'Reading USERIN filename.'
      fname = uinpfl
      inquire(file=uinpfl,exist=lcheck)
      if( .NOT. lcheck ) goto 7001
c
      action = 'Reading input EMBR filename.'
      fname = imbrfl
      inquire(file=imbrfl,exist=lcheck)
      if( .NOT. lcheck ) goto 7001
c
      if( lsurg ) then
          action = 'Reading surrogate cross reference filename.'
          fname = xrffl
          inquire(file=xrffl,exist=lcheck)
          if( .NOT. lcheck ) goto 7001
      endif
c
      if( lsurg ) then
          action = 'Reading gridded surrogate data filename.'
          fname = srgfl
          inquire(file=srgfl,exist=lcheck)
          if( .NOT. lcheck ) goto 7001
      endif
c
      if( lnkdef ) then
          action = 'Reading link definition filename.'
          fname = lnkfl
          inquire(file=lnkfl,exist=lcheck)
          if( .NOT. lcheck ) goto 7001
      endif
c
c   --- check for output files that already exist ---
c
      action = 'Reading message filename.'
      fname = msgfl
      inquire(file=msgfl,exist=lcheck)
      if( lcheck ) goto 7002
c
      if( lascfl ) then
          action = 'Reading ASC/FIPS totals filename.'
          fname = ascfl
          inquire(file=ascfl,exist=lcheck)
          if( lcheck ) goto 7002
      endif
c
      if( loutfile ) then
         action = 'Reading output emissions filename.'
         fname = oemsfl
         inquire(file=oemsfl,exist=lcheck)
         if( lcheck ) goto 7002
      endif
c
      action = 'Reading output NetCDF filename.'
      fname = oncffl
      inquire(file=oncffl,exist=lcheck)
      if( lcheck ) goto 7002
c
      action = 'Reading output error filename.'
      fname = errfl
      inquire(file=errfl,exist=lcheck)
      if( lcheck ) goto 7002
c
      if( lsrpt ) then
          action = 'Reading surrogate report filename.'
          fname = srptfl
          inquire(file=srptfl,exist=lcheck)
          if( lcheck ) goto 7002
      endif
c
c   --- open files ---
c
      action = 'Opening USERIN file.'
      fname = uinpfl
      open(IORINP,file=uinpfl,status='OLD',ERR=7003)
c
      action = 'Opening input EMBR file.'
      fname = imbrfl
      open(IORMBR,file=imbrfl,form='UNFORMATTED',status='OLD',ERR=7003)
c
      if( lsurg ) then
         action = 'Opening surrogate cross reference filename.'
         fname = xrffl
         open(unit=IORXRF,file=xrffl,status='OLD',ERR=7003)
c
      action = 'Opening gridded surrogates data file.'
         fname = srgfl
         open(unit=IORSRG,file=srgfl,status='OLD',ERR=7003)
      endif
c
      if( lnkdef ) then
      action = 'Opening link definition file.'
         fname = lnkfl
         open(unit=IORLNK,file=lnkfl,status='OLD',ERR=7003)
      endif
c
      action = 'Opening message file.'
      fname = msgfl
      open(unit=IOWMSG,file=msgfl,status='UNKNOWN',ERR=7003)
c
      if( loutfile ) then
         action = 'Opening output emissions file.'
         fname = oemsfl
         open(unit=IOWEMS,file=oemsfl,form='UNFORMATTED',
     &                                      status='UNKNOWN',ERR=7003)
      endif
c
c  --- if doing NetCDF call routine to open file --
c
      if( lnetcdf_out ) then
         action = 'Opening output NetCDF filename.'
         call ncf_createfile(jerr,oncffl,action,ncf_iowemis)
         if( jerr .NE. ISUCES ) goto 9999
      endif
c
      if( lascfl ) then 
         action = 'Opening ASC/FIPS totals file.'
         fname = ascfl
         open(unit=IOWASC,file=ascfl,form='UNFORMATTED',
     &                                ERR=7003,status='UNKNOWN')
      endif
c
      action = 'Opening output error file.'
      fname = errfl
      open(unit=IOWERR,file=errfl,status='UNKNOWN',ERR=7003)
c
      if( lsrpt ) then 
         action = 'Opening surrogate report file.'
         fname = srptfl
         open(unit=IOWSRG,file=srptfl,status='UNKNOWN',ERR=7003)
      endif
c
c   --- echo filenames ---
c
      ioerr = IOWMSG
      call getime( cdate )
      write(IOWMSG,'(/,T20,A,A40,/)',ERR=7004) VERSON, cdate
      write(IOWMSG,9000,ERR=7004) '    Input Files'
c
      write(IOWMSG,9000,ERR=7004) 'USERIN file',uinpfl(:strmin(uinpfl))
      write(IOWMSG,9000,ERR=7004) 'Input EMBR file',
     &                                          imbrfl(:strmin(imbrfl))
      if( lsurg ) then
         write(IOWMSG,9000,ERR=7004) 'Surg. XREF file',
     &                                          xrffl(:strmin(xrffl))
         write(IOWMSG,9000,ERR=7004) 'Gridded Surg. file',
     &                                          srgfl(:strmin(srgfl))
      else
         write(IOWMSG,9000,ERR=7004) 'No surrogate provided.'
         write(IOWMSG,9000,ERR=7004) 'No surrogate provided.'
      endif
c
      if( lnkdef ) then
         write(IOWMSG,9000,ERR=7004)'Link definition file',
     &                                          lnkfl(:strmin(lnkfl))
      else
         write(IOWMSG,9000,ERR=7004) 'No link definition data provided.'
      endif
c
      write(IOWMSG,9000,ERR=7004)
      write(IOWMSG,9000,ERR=7004) '    Output Files'
      write(IOWMSG,9000,ERR=7004) 'Emissions file',
     &                                          oemsfl(:strmin(oemsfl))
      write(IOWMSG,9000,ERR=7004) 'Grodded NetCDF file',
     &                                          oncffl(:strmin(oncffl))
c
      if( lascfl ) then
         write(IOWMSG,9000,ERR=7004) 'ASC/FIPS Emiss file',
     &                                          ascfl(:strmin(ascfl))
      else
         write(IOWMSG,9000,ERR=7004) 'ASC/FIPS Emiss file',
     &                                                 ' Not requested.'
      endif
c
      write(IOWMSG,9000,ERR=7004) 'ERROR file',errfl(:strmin(errfl))
c
      if( lsrpt ) then
         write(IOWMSG,9000,ERR=7004) 'Surrogate Report file',
     &                                       srptfl(:strmin(srptfl))
      else
         write(IOWMSG,9000,ERR=7004) 'Surrogate Report file',
     &                                                 ' Not requested.'
      endif
      write(IOWMSG,'(A)',ERR=7004)
c
c  --- initialize the hash values for the surrogate report ---
c
      idx_last_fssf = IMISS
      last_fssf = ' '
      do i=1,MXHASH_FSSF
         idxhsh_fssf_first(i) = IMISS
      enddo
      do i=1,MXFSSF
         idxhsh_fssf_next(i) = IMISS
      enddo
c
c  --- set error flag to success ----
c
      ierr = ISUCES
      goto 9999
c
c
c-----------------------------------------------------------------------
c   Warning Messages:
c-----------------------------------------------------------------------
c
 6000 continue
      lsrpt = .FALSE.
      write(IOWSTD,'(A)',ERR=9999) 'No file supplied.'
      goto 111
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
      write(IOWSTD,'(2A,//)',ERR=9999) 'This version requires an ',
     &  'additional output file:  the ASC/FIPS emissions totals file.'
      goto 9999
c
 7006 continue
      write(ioerr,'(/,A)',ERR=9999) 'ERROR reading filenames.'
      write(ioerr,'(A)',ERR=9999) action(:strmin(action))
      write(ioerr,'(A)',ERR=9999) 'Not enough filenames supplied.'
      write(ioerr,'(2A)',ERR=9999) 'This version requires an addiitonal',
     &                         ' filename for the NetCDF output file.'
      write(ioerr,'(2A)',ERR=9999) 'Make sure there is a filename or',
     &                         ' a blank line between the output emissions'
      write(ioerr,'(A,//)',ERR=9999) 'file and the ASC/FIPS total file.'
      goto 9999
c
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
