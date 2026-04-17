C**** OPNPSTFIR.F
c
      subroutine opnpstfir( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the filenames to be used in the PSTFIR program and opens all
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
c     modified june 2015 mjimenez
c              add an additional optional output file -
c              the binary elevated point source file.  To support
c              the plume distribution override feature in CAMx
c-----------------------------------------------------------------------
c    Include Files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'iopstfir.inc'
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
      lptsfl = .TRUE.
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
          if ( .NOT. lelvfl ) then
             write(IOWSTD,'(2A)',ERR=9999) 
     &            'WARNING - No elevated file supplied. ',
     &                      ' Points header filename ignored.'
          endif
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
      action = 'Reading output binary ptsrce filename.'
      write(IOWSTD,'(/,2A)',ERR=9999) 
     &        'Enter output binary ptsrce filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7005) optsfl
      call lftjst( optsfl )
      if( strlen(optsfl) .EQ. 0 ) then
          lptsfl = .FALSE.
          write(IOWSTD,'(A)',ERR=9999) 'No file supplied.'
      else
          write(IOWSTD,'(2A)') 'Read: ',optsfl(:strmin(optsfl))
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
      if ( lptsfl )  then
         fname = optsfl
         action = 'Checking output binary ptsrce emissions filename.'
         inquire(file=optsfl,exist=lcheck)
         if( lcheck ) goto 7002
      endif
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
      fname = msgfl
      action = 'Opening message file.'
      open(unit=IOWMSG,file=msgfl,status='UNKNOWN',ERR=7003)
c
      if ( lelvfl )  then
         fname = oelvfl
         action = 'Opening output ptsrce emissions file.'
         open(unit=IOWELV,file=oelvfl,status='UNKNOWN',ERR=7003)
         fname = ohdrfl
         action = 'Opening output header emissions file.'
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
      if ( lptsfl )  then
         fname = optsfl
         action = 'Opening output binary ptsrce file.'
         open(unit=IOWPTS,file=optsfl,form='UNFORMATTED',
     &               status='UNKNOWN',ERR=7003)
      endif
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
         write(IOWMSG,9000,ERR=7004) 'Surface emissions file',
     &                                          'Not requested.'
      endif
c
      if ( lptsfl )  then
         write(IOWMSG,9000,ERR=7004) 'Binary ptsrce file',
     &                                          optsfl(:strmin(optsfl))
      else
         write(IOWMSG,9000,ERR=7004) 'Binary ptsrce file',
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
c   --- write message ---
c
      write(IOWMSG,'(//)',ERR=7000)
      write(IOWMSG,9001,ERR=7000)
     &'NOTE: This version of PSTFIR supports the CAMx plume rise ',
     &'      distribution override feature. The CAMx binary point ',
     &'      source file uses stack parameters in the time-invariant',
     &'      (hourly) stack list to define plume top and bottom.'

      if( lelvfl ) then
         write(IOWMSG,'(//)',ERR=7000)
         write(IOWMSG,9002,ERR=7000)
     &'WARNING: The requested ASCII elevated emissions and header',
     &'         files are for quality assurance purposes only. The',
     &'         hourly variation in plume distribution is NOT',
     &'         reflected in these files.', ' ',
     &'         These should not be used for modeling.  Only the',
     &'         binary output files should be used for modeling.'
      endif
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
 9001 format(10X,61('-'),4(/,10X,A),/,10X,61('-'))
 9002 format(10X,61('-'),7(/,10X,A),/,10X,61('-'))
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
