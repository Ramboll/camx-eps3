C**** OPNPIG.F version 2.0 09/01/2007
c
      subroutine opnpig( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the filenames to be used in the PIGEMS program and opens all
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
      include 'iopigems.inc'
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
      character*(MXSTR)  fname, action
      character*(KEYLEN) keyin
      character*40       cdate
      integer*4          i, jerr
      logical            lcheck
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
      nstkfl = 0
      nemsfl = 0
      lmaster = .FALSE.
      lmaskfl = .FALSE.
      lstaticfl = .FALSE.
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
      action = 'Reading input master stack list filename.'
      write(IOWSTD,'(/,2A)',ERR=9999) 
     &                        'Enter master stack list filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7005) masterfl
      call lftjst( masterfl )
      if( strlen(masterfl) .GT. 0 ) then
         lmaster = .TRUE.
         write(IOWSTD,'(2A)') 'Read: ',masterfl(:strmin(masterfl))
      else
         lmaster = .FALSE.
         write(IOWSTD,'(2A)') 'No input master stack list provided. ',
     &                        'A master stack list will be created.'
      endif
c
 111  continue
      action = 'Reading input stack filenames.'
      write(IOWSTD,'(/,A)',ERR=9999) 'Enter next stack filename:'
      write(IOWSTD,'(A)',ERR=9999) 'Enter /END/ to finish.'
      read(IORSTD,'(20X,A)',ERR=7000,END=7005) fname
      call lftjst( fname )
c
c  --- check for /END/ keyword ---
c
      keyin = fname(:KEYLEN)
      call low2up( keyin )
      if( keyin(:KEYLEN) .EQ. KEYEND ) goto 222
      write(IOWSTD,'(2A)') 'Read: ',fname(:strmin(fname))
c
c  --- check for array overflow ---
c
      nstkfl = nstkfl + 1      
      if( nstkfl .GT. MXSTKFL ) goto 7006
c
c  --- load name into array, and get next record ----
c
      istkfl(nstkfl) = fname
      goto 111
c
c  --- read the input emissions files ---
c
 222  continue
      action = 'Reading input emissions filenames.'
      write(IOWSTD,'(/,A)',ERR=9999) 'Enter next emissions filename:'
      write(IOWSTD,'(A)',ERR=9999) 'Enter /END/ to finish.'
      read(IORSTD,'(20X,A)',ERR=7000,END=7005) fname
      call lftjst( fname )
c
c  --- check for /END/ keyword ---
c
      keyin = fname(:KEYLEN)
      call low2up( keyin )
      if( keyin(:KEYLEN) .EQ. KEYEND ) goto 333
      write(IOWSTD,'(2A)') 'Read: ',fname(:strmin(fname))
c
c  --- check for array overflow ---
c
      nemsfl = nemsfl + 1
      if( nemsfl .GT. MXEMSFL ) goto 7007
c
c  --- load name into array, and get next record ----
c
      iemsfl(nemsfl) = fname
      goto 222
c
c  --- now read the rest of the filenames ----
c
 333  continue
      action = 'Reading the threshold mask filename.'
      write(IOWSTD,'(/,2A)',ERR=9999) 'Enter threshold mask filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7005) maskfl
      call lftjst( maskfl )
      if( strlen(maskfl) .GT. 0 ) then
         lmaskfl = .TRUE.
         write(IOWSTD,'(2A)') 'Read: ',maskfl(:strmin(maskfl))
      else
         lmaskfl = .FALSE.
         write(IOWSTD,'(2A)') 'No threshold mask file provided. ',
     &                'All cells will be assigned to threshold code: 0'
      endif
c
      action = 'Reading the model-ready static sources filename.'
      write(IOWSTD,'(/,2A)',ERR=9999) 
     &                     'Enter model-ready static sources filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7005) staticfl
      call lftjst( staticfl )
      if( strlen(staticfl) .GT. 0 ) then
         lstaticfl = .TRUE.
         write(IOWSTD,'(2A)') 'Read: ',staticfl(:strmin(staticfl))
         if( .NOT. lmaster ) then
           write(IOWSTD,'(2A)') 'You have asked to create a new ',
     &                                              'master stack list.'
           write(IOWSTD,'(2A)') 'You cannot provide an input ',
     &          'model-ready file when generating a master stack list.'
           write(IOWSTD,'(2A)') 'This file will be ignored.'
           lstaticfl = .FALSE.
         endif
      else
         lstaticfl = .FALSE.
         write(IOWSTD,'(2A)') 'No model-ready static sources ',
     &                                           'file provided.'
      endif
c
      action = 'Reading output message filename.'
      write(IOWSTD,'(/,2A)',ERR=9999) 'Enter message filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7005) msgfl
      call lftjst( msgfl )
      write(IOWSTD,'(2A)') 'Read: ',msgfl(:strmin(msgfl))
c
      action = 'Reading output master stack filename.'
      write(IOWSTD,'(/,2A)',ERR=9999) 
     &                         'Enter output master stack filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7005) ostkfl
      call lftjst( ostkfl )
      if( lmaster ) then
         write(IOWSTD,'(2A)') 'Input master stack list provided. ',
     &                   'No output master stack list will be created.'
      else
         if( strlen(ostkfl) .LE. 0 ) goto 7008
         write(IOWSTD,'(2A)') 'Read: ',ostkfl(:strmin(ostkfl))
      endif
c
      loemsfl = .FALSE.
      action = 'Reading output binary emissions filename.'
      write(IOWSTD,'(/,2A)',ERR=9999) 'Enter output binary emissions filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7005) oemsfl
      call lftjst( oemsfl )
      if( strlen(oemsfl) .GT. 0 ) then
         loemsfl = .TRUE.
         if( .NOT. lmaster ) then
           write(IOWSTD,'(2A)') 'A new master stack list will be ',
     &        'created. No binary CAMx emissions file will be created.'
         else
           write(IOWSTD,'(2A,//)') 'Read: ',oemsfl(:strmin(oemsfl))
         endif
      endif
c
      lnetcdf_out = .FALSE.
      action = 'Reading output NetCDF emissions filename.'
      write(IOWSTD,'(/,2A)',ERR=9999) 'Enter output NetCDF emissions filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7010) ncf_oemsfl
      call lftjst( ncf_oemsfl )
      if( strlen(ncf_oemsfl) .GT. 0 ) then
         lnetcdf_out = .TRUE.
         if( .NOT. lmaster ) then
           write(IOWSTD,'(2A)') 'A new master stack list will be ',
     &        'created. No NetCDF CAMx emissions file will be created.'
         else
           write(IOWSTD,'(2A,//)') 'Read: ',ncf_oemsfl(:strmin(ncf_oemsfl))
         endif
      endif
c
c   --- make sure that we are doing something ---
c
      if( .NOT. loemsfl .AND. .NOT. lnetcdf_out ) then
         if( lmaster ) goto 7009
         write(IOWSTD,'(2A,//)') 
     &                   'Creating master stack list. A ',
     &                     'CAMx emissions file will not be created.'
      endif
c
c   --- check for existence of input files ---
c
      fname = uinpfl
      action = 'Opening USERIN file.'
      inquire(file=uinpfl,exist=lcheck)
      if( .NOT. lcheck ) goto 7001
c
      if( lmaster ) then
         fname = masterfl
         action = 'Opening master stack list file.'
         inquire(file=masterfl,exist=lcheck)
         if( .NOT. lcheck ) goto 7001
      endif
c
      do i=1,nstkfl
        fname = istkfl(i)
        action = 'Opening input stack files.'
        inquire(file=istkfl(i),exist=lcheck)
        if( .NOT. lcheck ) goto 7001
      enddo
c
      do i=1,nemsfl
        fname = iemsfl(i)
        action = 'Opening input emissions files.'
        inquire(file=iemsfl(i),exist=lcheck)
        if( .NOT. lcheck ) goto 7001
      enddo
c
      if( lmaskfl ) then
         fname = maskfl
         action = 'Opening threshold mask file.'
         inquire(file=maskfl,exist=lcheck)
         if( .NOT. lcheck ) goto 7001
      endif
c
      if( lstaticfl ) then
         fname = staticfl
         action = 'Opening threshold static file.'
         inquire(file=staticfl,exist=lcheck)
         if( .NOT. lcheck ) goto 7001
      endif
c
c   --- check for output files that already exist ---
c
      fname = msgfl
      action = 'Opening output message file.'
      inquire(file=msgfl,exist=lcheck)
      if( lcheck ) goto 7002
c
      if( .NOT. lmaster ) then
         fname = ostkfl
         action = 'Opening output master stack list file.'
         inquire(file=ostkfl,exist=lcheck)
         if( lcheck ) goto 7002
      endif
c
      if( loemsfl ) then
         fname = oemsfl
         action = 'Opening output emissions file.'
         inquire(file=oemsfl,exist=lcheck)
         if( lcheck ) goto 7002
      endif
c
c   --- open files ---
c
      fname = uinpfl
      action = 'Opening USERIN file.'
      open(IORINP,file=uinpfl,status='OLD',ERR=7003)
c
      if( lmaster ) then
         fname = masterfl
         action = 'Opening master stack list file.'
         open(IORMSTR,file=masterfl,status='OLD',ERR=7003)
      endif
c
c  ---- just make sure the files can be opened
c        they will be opened later and read ---
c
      do i=1,nstkfl
        fname = istkfl(i)
        action = 'Opening input stack files.'
        open(IORSTK,file=istkfl(i),status='OLD',ERR=7003)
        close(IORSTK)
      enddo
c
c  ---- just make sure the files can be opened
c        they will be opened later and read ---
c
      do i=1,nemsfl
        fname = iemsfl(i)
        action = 'Opening input emissions files.'
        open(IOREMS,file=iemsfl(i),status='OLD',ERR=7003)
        close(IOREMS)
      enddo
c
      if( lmaskfl ) then
         fname = maskfl
         action = 'Opening threshold mask file.'
         open(IORMSK,file=maskfl,status='OLD',ERR=7003)
      endif
c
      if( lstaticfl ) then
         fname = staticfl
         action = 'Opening threshold static file.'
         open(IORSTAT,file=staticfl,status='OLD',
     &                                  FORM='UNFORMATTED',ERR=7003)
      endif
c
      fname = msgfl
      action = 'Opening output message file.'
      open(unit=IOWMSG,file=msgfl,status='UNKNOWN',ERR=7003)
c
      if( lmaster ) then
c
c  --- if doing NetCDF call routine to open file ---
c
         fname = ncf_oemsfl
         if( lnetcdf_out ) then
            action = 'Opening output NetCDF filename.'
            call ncf_createfile(jerr,ncf_oemsfl,action,ncf_iowemis)
            if( jerr .NE. ISUCES ) goto 9999
         endif
c
c  --- if doing UAM binary open file ---
c
         if( loemsfl ) then
            action = 'Opening output emissions file.'
            open(unit=IOWEMS,file=oemsfl,form='UNFORMATTED',
     &                                      status='UNKNOWN',ERR=7003)
         endif



      endif
c
      if( .NOT. lmaster ) then
         fname = ostkfl
         action = 'Opening output master stack list file.'
         open(unit=IOWSTK,file=ostkfl,status='UNKNOWN',ERR=7003)
      endif
c
c  --- echo the filenames ---
c
      ioerr = IOWMSG
      call getime( cdate )
      write(IOWMSG,'(/,T20,A,A40,/)',ERR=7004) VERSON, cdate
      write(IOWMSG,9000,ERR=7004) '    Input Files'
      write(IOWMSG,9000,ERR=7004) 'USERIN file',uinpfl(:strmin(uinpfl))
c
      if( lmaster ) then
          write(IOWMSG,9000,ERR=7004)  'Input master stack list file',
     &                                      masterfl(:strmin(masterfl))
      else
          write(IOWMSG,9000,ERR=7004)  'No master stack list file',
     &                              'Master stack list will be created.'
      endif
c
      if( nstkfl .GT. 0 ) then
         write(IOWMSG,9000,ERR=7004) 'Input stack list files'
         do i=1,nstkfl
           write(IOWMSG,9001,ERR=7004) 'Stack file ',
     &                          i,istkfl(i)(:strmin(istkfl(i)))
         enddo
      endif
c
      if( nemsfl .GT. 0 ) then
         write(IOWMSG,9000,ERR=7004) 'Input stack emissions files'
         do i=1,nemsfl
           write(IOWMSG,9001,ERR=7004) 'Emiss file ',
     &                           i,iemsfl(i)(:strmin(iemsfl(i)))
         enddo
      endif
c
      if( lmaskfl ) then
          write(IOWMSG,9000,ERR=7004)  'Threshold mask file',
     &                                      maskfl(:strmin(maskfl))
      else
          write(IOWMSG,9000,ERR=7004)  'Threshold mask file',
     &                                      ' Not provided.'
      endif
      if( lstaticfl ) then
          write(IOWMSG,9000,ERR=7004) 'Model-ready static sources file',
     &                                      staticfl(:strmin(staticfl))
      else
          write(IOWMSG,9000,ERR=7004) 'Model-ready static sources file',
     &                                      ' Not provided.'
      endif
c
      write(IOWMSG,9000,ERR=7004)
      write(IOWMSG,9000,ERR=7004) '    Output Files'
c
      if( lmaster ) then
         write(IOWMSG,9000,ERR=7004) 'Using Input Master stack list',
     &                  ' No output master stack list will be created.'
         if( loemsfl ) then
            write(IOWMSG,9000,ERR=7004) 'Output binary Emissions file',
     &                                           oemsfl(:strmin(oemsfl))
         else
            write(IOWMSG,9000,ERR=7004)
     &                          'No output binary Emissions created'
         endif
         if( lnetcdf_out ) then
            write(IOWMSG,9000,ERR=7004) 'Output NetCDF Emissions file',
     &                                  ncf_oemsfl(:strmin(ncf_oemsfl))
         else
            write(IOWMSG,9000,ERR=7004)
     &                          'No output NetCDF Emissions created'
         endif
      else
         write(IOWMSG,9000,ERR=7004) 
     &                        'New master stack list file',
     &                                          ostkfl(:strmin(ostkfl))
         write(IOWMSG,9000,ERR=7004) 'Emissions file',
     &                     ' CAMx emissions file will not be created.'
      endif
c
c  --- set error flag to success ----
c
      ioerr = IOWMSG
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
      write(ioerr,'(2A)',ERR=9999) 'NOTE: This version has an ',
     &           'additional file: the model-ready static sources file.'
      write(ioerr,'(2A)',ERR=9999) 'It should be included after the ',
     &                                      'threshold mask filename.'
      write(ioerr,'(2A)',ERR=9999) 'To turn off this option, enter ',
     &                                              'a blank filename.'
      write(ioerr,'(2A)',ERR=9999) 'You entered: ',
     &                                    staticfl(:strmin(staticfl))
      write(ioerr,'(A,/)',ERR=9999)'Please verify that this is correct.'
      goto 9999
      goto 9999
c
 7006 continue
      write(ioerr,'(/,A)',ERR=9999) 'ERROR reading filenames.'
      write(ioerr,'(2A)',ERR=9999) 'Maximum number of ',
     &                                 'input stack files exceeded.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maximum allowed: ',MXSTKFL
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXSTKFL ',
     &                                                  'and recompile.'
      goto 9999
c
 7007 continue
      write(ioerr,'(/,A)',ERR=9999) 'ERROR reading filenames.'
      write(ioerr,'(2A)',ERR=9999) 'Maximum number of ',
     &                               'input emissions files exceeded.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maximum allowed: ',MXEMSFL
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXEMSFL ',
     &                                                  'and recompile.'
      goto 9999
c
 7008 continue
      write(ioerr,'(/,A)',ERR=9999) 'ERROR reading filenames.'
      write(ioerr,'(2A)',ERR=9999) 'No input master stack list or ',
     &                           'output stack list filename provided.'
      write(ioerr,'(2A)',ERR=9999) 'You must either supply ',
     &                             'an input master stack list file ' 
      write(ioerr,'(2A,//)',ERR=9999) 'or supply a filename so one ',
     &                     'can be created from the input stack files.'
      write(ioerr,'(2A)',ERR=9999) 'NOTE: This version has an ',
     &           'additional file: the model-ready static sources file.'
      write(ioerr,'(2A)',ERR=9999) 'It should be included after the ',
     &                                      'threshold mask filename.'
      write(ioerr,'(2A)',ERR=9999) 'To turn off this option, enter ',
     &                                              'a blank filename.'
      write(ioerr,'(2A)',ERR=9999) 'You entered: ',
     &                                    staticfl(:strmin(staticfl))
      write(ioerr,'(A,/)',ERR=9999)'Please verify that this is correct.'
      goto 9999
c
 7009 continue
      write(ioerr,'(/,A)',ERR=9999) 'ERROR reading filenames.'
      write(ioerr,'(2A)',ERR=9999) 'No output master stack list or ',
     &                      'output binary emissions filename provided.'
      write(ioerr,'(2A)',ERR=9999) 'If you supply an input ',
     &                                        'master stack list file ' 
      write(ioerr,'(2A,//)',ERR=9999) 'you also must supply a name',
     &                     ' for the output binary CAMx emissions file.'
      goto 9999
c
 7010 continue
      write(ioerr,'(/,A)',ERR=9999) 'ERROR reading filenames.'
      write(ioerr,'(A)',ERR=9999) action(:strmin(action))
      write(ioerr,'(A)',ERR=9999) 'Not enough filenames supplied.'
      write(ioerr,'(2A)',ERR=9999) 'This version requires an addiitonal',
     &                         ' filename for the NetCDF output file.'
      write(ioerr,'(2A,//)',ERR=9999) 'Make sure there is a filename or',
     &                     ' a blank line at the end of the program input.'
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
 9000 format(T10,A,T40,:,':',A)
 9001 format(T10,A,I4.4,T40,:,':',A)
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
