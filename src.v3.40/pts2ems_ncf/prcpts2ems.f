C**** PRCPTS2EMS.F 
c
      subroutine prcpts2ems( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the user input file and stores the data in globalvariables.
c
c    Argument Declaration.
c     Outputs:
c       ierr  I error flag
c     Inputs:
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
      include 'uamregn.inc'
      include 'uamcntl.inc'
      include 'ncf_iodat.inc'
      include 'iopts2ems.inc'
      include 'pts2emstot.inc'
      include 'netcdf.inc'
c
c-----------------------------------------------------------------------
c    Argument Declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
c
c-----------------------------------------------------------------------
c    Subroutines Called:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    External Functions:
c-----------------------------------------------------------------------
c
      integer strmin          
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
      character*200 action
      character*10  this_var
      character*4   this_iname(10)
      integer*4     data_start_pts(2), data_count_pts(2), icell, jcell
      integer*4     this_hour, this_spec, this_varid, jerr
      integer*4     data_start_grid(4), data_count_grid(4), ipt
      integer*4     nptsrce_in, idum, i
      real*4        rdum
c
      real*4,   allocatable, dimension(:)   :: emiss_pts
      real*4,   allocatable, dimension(:,:) :: emiss_grid
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
      ierr = IFAIL
c
c  ---- allocate local arrays for reading data ---
c
      allocate( emiss_pts(nptsrce)       )
      allocate( emiss_grid(nxcell,nycell) )
c
      data_start_grid(1) = 1
      data_count_grid(1) = nxcell
      data_start_grid(2) = 1
      data_count_grid(2) = nycell
      data_start_grid(3) = 1
      data_count_grid(3) = 1
      data_count_grid(4) = 1
c
      data_start_pts(1) = 1
      data_count_pts(1) = nptsrce
      data_count_pts(2) = 1
c
c   --- read the source locations from NetCDF file ---
c
      if( lnetcdf_in ) then
         action = 'Reading point source locations from input file.'
         this_var = 'xcoord'
         jerr = nf_inq_varid(inpunit, this_var, this_varid)
         if( ierr .EQ. NF_NOERR) goto 7000
         jerr = nf_get_vara_real(inpunit,this_varid,1,nptsrce,xlocpt)
         if( jerr .NE. NF_NOERR) goto 7001
c
         action = 'Reading point source locations from input file.'
         this_var = 'ycoord'
         jerr = nf_inq_varid(inpunit, this_var, this_varid)
         if( ierr .EQ. NF_NOERR) goto 7000
         jerr = nf_get_vara_real(inpunit,this_varid,1,nptsrce,ylocpt)
         if( jerr .NE. NF_NOERR) goto 7001
      else
c
c   --- read the source locations from UAM binary file,
c       and skip unneeded data  ---
c
          read(inpunit,ERR=7003) (xlocpt(i),ylocpt(i),
     &                                rdum,rdum,rdum,rdum,i=1,nptsrce)
      endif
c
c  ---- loop over the number of timesteps ---
c
      do this_hour=1,ncf_nsteps
c
          ncf_cur_tstep = this_hour
          write(*,'(A,I3)') 'Processing hour: ',this_hour
          call flush(6)
c
c   --- read the source locations from UAM binary file,
c       and skip unneeded data  ---
c
          if( .NOT. lnetcdf_in ) then
             read(inpunit,ERR=7004)
             read(inpunit,ERR=7004) idum, nptsrce_in
             read(inpunit,ERR=7004) 
          endif
c
          data_start_pts(2) = this_hour
          data_start_grid(4) = this_hour
c
c  ---- loop over species in file ---
c
          do this_spec=1,nspecs
             emiss_grid = 0.
c
c  --- get point source emissions from NetCDF file ---
c
             if( lnetcdf_in ) then
                 action = 'Reading point source emissions from input file.'
                 this_var = spcnam(this_spec)
                 jerr = nf_inq_varid(inpunit, this_var, this_varid)
                 if( jerr .NE. NF_NOERR) goto 7000
c
                 jerr = nf_get_vara_real(inpunit,this_varid,data_start_pts,
     &                                           data_count_pts,emiss_pts)
                 if( jerr .NE. NF_NOERR) goto 7001
c
c  --- get point source emissions from UAM binary file ---
c
             else
                 call ncf_wrt_data_tstep(jerr,action,ncf_iowemis,nspecs)
                 if( jerr .NE. NF_NOERR) goto 9999
                 read(inpunit,ERR=7004) idum,(this_iname(i),i=1,10),
     &                                     (emiss_pts(ipt),ipt=1,nptsrce_in)
                 write(this_var,'(10A1)') (this_iname(i),i=1,10)
             endif
c
c  ---- calculate the cell containing this source ---
c
             do ipt=1,nptsrce
c
c  ---- add emissions to input totals ----
c
                call totpts2ems(jerr,IDXIN,emiss_pts(ipt),this_spec,this_hour)
c                 
                icell = 1 + FLOOR( (xlocpt(ipt)/1000. - xorig)/dxcell )
                jcell = 1 + FLOOR( (ylocpt(ipt)/1000. - yorig)/dycell )
                if( icell .GE. 1 .AND. icell .LE. nxcell .AND. 
     &                           jcell .GE. 1 .AND. jcell .LE. nycell ) then
                   emiss_grid(icell,jcell) = emiss_grid(icell,jcell) + emiss_pts(ipt)
c
c  ---- add emissions to input totals ----
c
                   call totpts2ems(jerr,IDXOUT,emiss_pts(ipt),this_spec,this_hour)
                endif
             enddo
c
c  ---- write the grid to the output file ---
c
             action = 'Writing point source emissions to output file.'
             jerr = nf_inq_varid(ncf_iowemis, this_var, this_varid)
             if( jerr .NE. NF_NOERR) goto 7002
             ierr = nf_put_vara_real(ncf_iowemis,this_varid,data_start_grid,
     &                                                 data_count_grid,emiss_grid)
             if( ierr .NE. NF_NOERR ) goto 7001
c
c  --- next species ---
c
          enddo
c
c  --- process next hour ---
c
      enddo
  111 continue
c
c  --- entire file processed ---
c
      deallocate( emiss_pts  )
      deallocate( emiss_grid )
      jerr = nf_close(ncf_iowemis)
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(*,'(//,a)') 'ERROR in PRCPTS2EMS:'
      write(*,'(A)') action(:strmin(action))
      write(*,'(2A,//)') 'Cannot find variable id in input file for: ',
     &                                      this_var(:strmin(this_var))
      write(*,'(A,I5,//)') 'NetCDF error code: ',jerr
      ierr = IFAIL
      goto 9999
c
 7001 continue
      write(*,'(//,a)') 'ERROR in PRCPTS2EMS:'
      write(*,'(A)') action(:strmin(action))
      write(*,'(2A)') 'Cannot read data for variable: ',
     &                                      this_var(:strmin(this_var))
      write(*,'(A,I5)') 'NetCDF error code: ',jerr
      ierr = IFAIL
      goto 9999

 7002 continue
      write(*,'(//,a)') 'ERROR in PRCPTS2EMS:'
      write(*,'(A)') action(:strmin(action))
      write(*,'(2A,//)') 'Cannot write data to output file for varaible; ',
     &                                      this_var(:strmin(this_var))
      write(*,'(A,I5)') 'NetCDF error code: ',jerr
      ierr = IFAIL
      goto 9999
c
 7003 continue
      write(*,'(//,A)') 'ERROR in PRCPTS2EMS: '
      write(*,'(A)') 'Reading input point source file.'
      write(*,'(A,//)') 'Cannot read the header locations of source ',
     &                                          'in point source file.'
      goto 9999
c
 7004 continue
      write(*,'(//,A)') 'ERROR in PRCPTS2EMS: '
      write(*,'(A)') 'Reading input point source file.'
      write(*,'(A,//)') 'Cannot read the hourly emissions data ',
     &                                          'in point source file.'
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
