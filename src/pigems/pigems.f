C***** PIGEMS version 2.0 09/01/2007
c
      program pigems
c
c-----------------------------------------------------------------------
c
c    This module reads the ASCII elevated point source files produced 
c    by PSTPNT and merges multiple files together, creating a binary
c    point source file for use in photochemical modeling.  The user
c    also has the ability to flag sources as PiGged in a a variety of
c    ways, including user-specified emissions threshold and stack
c    identification information. 
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
      include 'uamcntl.inc'
      include 'iopigems.inc'
      include 'usrpigems.inc'
      include 'stkpigems.inc'
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c     ierr  I   error flag for subroutine calls
c
      integer*4 ierr, i, j, k, l
c
c-----------------------------------------------------------------------
c    Entry Point:
c-----------------------------------------------------------------------
c
c  --- initialize the stack data structures ---
c
      nstklist = 0
      do i=1,nstklist
         do j=1,nspecs
           do k=1,MXHOUR
              emsstk(i,j,k) = 0.
           enddo
         enddo
      enddo
      do i=1,nstknew
         do j=1,nstkcomb(i)
            do k=1,nspecs
               do l=1,MXHOUR
                  spccomb(i,j,k,l) = 0.
               enddo
            enddo
         enddo
      enddo
c
c  --- call routine to get I/O filenames and open I/O files
c
      call opnpig(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to read user input file ---
c
      call rdupig(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to read the mask file ---
c
      if( lmaskfl ) then
         call rdmask(ierr)
         if( ierr .NE. ISUCES ) goto 9999
      endif
c
c  --- call routine to read the input CAMx file that contains
c      the static sources ---
c
      if( lstaticfl ) then
         call rdcamx(ierr)
         if( ierr .NE. ISUCES ) goto 9999
      endif
c
c  --- master stack list has been supplied ---
c
      if( lmaster ) then
c
c  --- call routine to read the master stack list ----
c
         call rdmaster(ierr) 
         if( ierr .NE. ISUCES ) goto 9999
c
c   --- call routine to read any additional stack list files ---
c
         call rdstknew(ierr)
         if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to read the emissions for this epsiode day ---
c
         call rdstkems(ierr)
         if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to average the stack parameters ---
c
         call stkavg()
c
c  --- call routine to add the "NEW" sources to the list ---
c
         call addnew(ierr)
         if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to write the output binary emissions files ---
c
         if( loemsfl ) then
             call wrtcamx(ierr)
            if( ierr .NE. ISUCES ) goto 9999
         endif
c
c  --- call routines to write the output NetCDF emissions files ---
c
         if( lnetcdf_out ) then
            call ncf_pig_wrthdr(ierr)
            if( ierr .NE. ISUCES ) goto 9999
            call ncf_wrt_data_pts(ierr)
            if( ierr .NE. ISUCES ) goto 9999
            call ncf_wrt_data_species(ierr)
            if( ierr .NE. ISUCES ) goto 9999
         endif
c
c  --- call routine to write the table of emissions processed ---
c
         call wrtemstot(ierr)
         if( ierr .NE. ISUCES ) goto 9999
      else
c
c  --- if there is not a master list, create it ---
c
         call rdstaks(ierr)
         if( ierr .NE. ISUCES ) goto 9999
c
c  --- read the emissions files, calculating pollutant totals
c      in preperation for setting the PiGs
c
         call rdstkmax(ierr)
         if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to PIG all sources based on thresholds ---
c
         call pigthresh()
c
c  --- call routine to calculate the colocation distance ---
c
         if( colodist .GT. 0. .AND. idxcoloc .GT. 0 ) then
             call colocate(ierr)
             if( ierr .NE. ISUCES ) goto 9999
         endif
c
c  --- call routine to write the master stack list ---
c
         call wrtmaster(ierr)
         if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to write the count of stacks processed ---
c
         call wrtstkcnt(ierr)
         if( ierr .NE. ISUCES ) goto 9999
      endif
c
c  --- looks successful, write message and reset flag ---
c
      write(IOWSTD,'(//,2A)',ERR=9999) 'Successful completion of ',
     &                                                       VERSON
      if( nwarn .EQ. 0 ) then
          write(IOWSTD,'(2A,//)',ERR=9999) 'There were no warning ',
     &                                             'messages written.'
      else if( nwarn .EQ. 1 ) then
          write(IOWSTD,'(2A,//)',ERR=9999) 'There was 1 warning ',
     &                                             'message written.'
      else
          write(IOWSTD,'(A,I2,A,//)',ERR=9999) 'There were ',nwarn,
     &                                   ' warning messages written.'
      endif
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Exit Point:
c-----------------------------------------------------------------------
c
c  --- call routine to close files ---
c
 9999 continue
      call clspig( )
      if( ierr .NE. ISUCES ) then
          write(IOWSTD,'(//,3A)',ERR=9999) 'An error occurred ',
     &                                       'while running ',VERSON
          write(IOWSTD,'(2A,//)',ERR=9999) 'If no explanation is ',
     &               'given above, check the message file for details.'
      endif
      end
