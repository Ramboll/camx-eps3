C**** OPNPSTSHP.F
c
      subroutine opnpstshp( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the filenames to be used in the PSTSHP program and opens all
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
c    june 2015 mjimenez Updated to support the plume rise
c                       distribution override feature of CAMx
c
c-----------------------------------------------------------------------
c    Include Files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'iopstshp.inc'
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
      integer*4 strmin
      integer*4 strlen
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c   fname   C   character string for temporary storage of filenames
c   lcheck  L   logical to check for existence of files
c
      character*(MXSTR) fname, action
      logical*4         lcheck
c
c-----------------------------------------------------------------------
c   Data statments:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- initialize error code ---
c
      ierr = IFAIL
c
      ioerr = IOWSTD
      lelvfl = .TRUE.
      lsrffl = .TRUE.
c
c   --- read all filenames from standard input ---
c
      action = 'Reading USERIN filename.'
      write(IOWSTD,'(/,2A)',ERR=9999) 'Enter USERIN filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7005) uinpfl
      call lftjst( uinpfl )
      write(IOWSTD,'(2A)') 'Read: ',uinpfl(:strmin(uinpfl))
c
      action = 'Reading input EMBR filename.'
      write(IOWSTD,'(/,2A)',ERR=9999) 'Enter input EMBR filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7005) imbrfl
      call lftjst( imbrfl )
      write(IOWSTD,'(2A)') 'Read: ',imbrfl(:strmin(imbrfl))
c
      action = 'Reading Vessel/Stack XREF filename.'
      write(IOWSTD,'(/,2A)',ERR=9999) 
     &                  'Enter Vessel/Stack XREF filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7005) vslmapfl
      call lftjst( vslmapfl )
      write(IOWSTD,'(2A)') 'Read: ',vslmapfl(:strmin(vslmapfl))
c
      action = 'Reading message filename.'
      write(IOWSTD,'(/,2A)',ERR=9999) 'Enter message filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7005) msgfl
      call lftjst( msgfl )
      write(IOWSTD,'(2A)') 'Read: ',msgfl(:strmin(msgfl))
c
      action = 'Reading output elevated points emissions filename.'
      write(IOWSTD,'(/,2A)',ERR=9999) 
     &        'Enter output elevated points emissions filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7005) oelvfl
      call lftjst( oelvfl )
      if( strlen(oelvfl) .EQ. 0 ) then
          lelvfl = .FALSE.
          write(IOWSTD,'(A)',ERR=9999) 'No file supplied.'
      else
          write(IOWSTD,'(2A)') 'Read: ',oelvfl(:strmin(oelvfl))
      endif
c
      action = 'Reading output elevated points header filename.'
      write(IOWSTD,'(/,2A)',ERR=9999) 
     &        'Enter output elevated points header filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7005) ohdrfl
      call lftjst( ohdrfl )
      if( strlen(oelvfl) .EQ. 0 ) then
          if ( lelvfl )  goto 7006
          write(IOWSTD,'(A)',ERR=9999) 'No file supplied.'
      else
          write(IOWSTD,'(2A)') 'Read: ',ohdrfl(:strmin(ohdrfl))
      endif
c
      action = 'Reading output surface emissions filename.'
      write(IOWSTD,'(/,2A)',ERR=9999) 
     &        'Enter output surface emissions filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7005) osrffl
      call lftjst( osrffl )
      if( strlen(osrffl) .EQ. 0 ) then
          lsrffl = .FALSE.
          write(IOWSTD,'(A)',ERR=9999) 'No file supplied.'
      else
          write(IOWSTD,'(2A)') 'Read: ',osrffl(:strmin(osrffl))
      endif
c
      action = 'Reading output 3D emissions NetCDF filename.'
      write(IOWSTD,'(/,2A)',ERR=9999) 
     &                 'Enter output 3D emissions filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7005) ncf_emsfile
      call lftjst( ncf_emsfile )
      if( strlen(ncf_emsfile) .EQ. 0 ) then
          l3demiss = .FALSE.
          write(IOWSTD,'(A)',ERR=9999) 'No file supplied.'
      else
          l3demiss = .TRUE.
          write(IOWSTD,'(2A)') 
     &             'Read: ',ncf_emsfile(:strmin(ncf_emsfile))
      endif
c
      action = 'Reading error filename.'
      write(IOWSTD,'(/,2A)',ERR=9999) 'Enter error filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7005) errfl
      call lftjst( errfl )
      write(IOWSTD,'(2A)') 'Read: ',errfl(:strmin(errfl))
c
c   --- check for existence of input files ---
c
      fname = uinpfl
      action = 'Checking USERIN filename.'
      inquire(file=uinpfl,exist=lcheck)
      if( .NOT. lcheck ) goto 7001
c
      fname = imbrfl
      action = 'Checking input EMBR filename.'
      inquire(file=imbrfl,exist=lcheck)
      if( .NOT. lcheck ) goto 7001
c
      fname = vslmapfl
      action = 'Checking Vessel/Stack XREF filename.'
      inquire(file=vslmapfl,exist=lcheck)
      if( .NOT. lcheck ) goto 7001
c
c   --- check for output files that already exist ---
c
      fname = msgfl
      action = 'Checking message filename.'
      inquire(file=msgfl,exist=lcheck)
      if( lcheck ) goto 7002
c
      if ( lelvfl )  then
         fname = oelvfl
         action = 'Checking output elevated points emissions filename.'
         inquire(file=oelvfl,exist=lcheck)
         if( lcheck ) goto 7002
      endif
c
      if ( lelvfl )  then
         fname = ohdrfl
         action = 'Checking output elevated points header filename.'
         inquire(file=ohdrfl,exist=lcheck)
         if( lcheck ) goto 7002
      endif
c
      if ( lsrffl )  then
         fname = osrffl
         action = 'Checking output surface emissions filename.'
         inquire(file=osrffl,exist=lcheck)
         if( lcheck ) goto 7002
      endif
c
c  --- make sure NCF file has proper extension as well ---
c
      if( l3demiss ) then
         fname = ncf_emsfile
         action = 'Checking output 3D emissions NetCDF filename.'
         inquire(file=ncf_emsfile,exist=lcheck)
         if( lcheck ) goto 7002
      endif
c      
      fname = errfl
      action = 'Checking error filename.'
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
      fname = vslmapfl
      action = 'Opening Vessel/Stack XREF file.'
      open(unit=IORVSL,file=vslmapfl,status='OLD',ERR=7003)
c
      fname = msgfl
      action = 'Opening message file.'
      open(unit=IOWMSG,file=msgfl,status='UNKNOWN',ERR=7003)
c
      if ( lelvfl )  then
         fname = oelvfl
         action = 'Opening output elevated points emissions file.'
         open(unit=IOWELV,file=oelvfl,status='UNKNOWN',ERR=7003)
      endif
c
      if ( lelvfl )  then
         fname = ohdrfl
         action = 'Opening output elevated points header filename.'
         open(unit=IOWHDR,file=ohdrfl,status='UNKNOWN',ERR=7003)
      endif
c
      if ( lsrffl )  then
         fname = osrffl
         action = 'Opening output surface emissions file.'
         open(unit=IOWSRF,file=osrffl,form='UNFORMATTED',
     &               status='UNKNOWN',ERR=7003)
      endif
c
      if ( l3demiss )  then
         fname = ncf_emsfile
         action = 'Opening output 3D emissions NetCDF filename.'
         call ncf_createfile(ierr,ncf_emsfile,action,ncf_iowemis)
         if( ierr .NE. ISUCES ) goto 9999
      endif
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
      write(IOWMSG,9000,ERR=7004) 'Input EMBR file',
     &                                       imbrfl(:strmin(imbrfl))
      write(IOWMSG,9000,ERR=7004) 'Vessel/Stack XREF file',
     &                                       vslmapfl(:strmin(vslmapfl))
c
      write(IOWMSG,9000,ERR=7004)
      write(IOWMSG,9000,ERR=7004) '    Output Files'
c
      write(IOWMSG,9000,ERR=7004) 'Message file',
     &                                            msgfl(:strmin(msgfl))
c
      if ( lelvfl )  then
         write(IOWMSG,9000,ERR=7004) 'Elevated emissions file',
     &                                          oelvfl(:strmin(oelvfl))
      else
         write(IOWMSG,9000,ERR=7004) 'Elevated emissions file',
     &                                          'Not requested.'
      endif
c
      if ( lelvfl )  then
      write(IOWMSG,9000,ERR=7004) 'Elevated header file',
     &                                          ohdrfl(:strmin(ohdrfl))
      else
      write(IOWMSG,9000,ERR=7004) 'Elevated header file',
     &                                          'Not requested.'
      endif
c
      if ( lsrffl )  then
      write(IOWMSG,9000,ERR=7004) 'Surface emissions file',
     &                                          osrffl(:strmin(osrffl))
      else
      write(IOWMSG,9000,ERR=7004)  'Surface emissions file',
     &                                          'Not requested.'
      endif
c
      if ( l3demiss )  then
      write(IOWMSG,9000,ERR=7004) '3D emissions file',
     &                                  ncf_emsfile(:strmin(ncf_emsfile))
      else
      write(IOWMSG,9000,ERR=7004)  '3D emissions file',
     &                                          'Not requested.'
      endif
c
      write(IOWMSG,9000,ERR=7004) 'ERROR file',errfl(:strmin(errfl))
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
      write(ioerr,'(/,A)',ERR=9999) 'ERROR: Writing message file.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',msgfl(:strmin(msgfl))
      goto 9999
c
 7005 continue
      write(ioerr,'(/,A)',ERR=9999) 'ERROR reading filenames.'
      write(ioerr,'(A)',ERR=9999) action(:strmin(action))
      write(ioerr,'(A,//)',ERR=9999) 'Not enough filenames supplied.'
      goto 9999
c
 7006 continue
      write(ioerr,'(/,A)',ERR=9999) 'ERROR reading filenames.'
      write(ioerr,'(A)',ERR=9999) action(:strmin(action))
      write(ioerr,'(2A,//)',ERR=9999) 'Header file required when ',
     &            'elevated emissions requested.'
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
