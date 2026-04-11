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
      character*16  this_var
      integer       data_start_pts(2), data_count_pts(2), icell, jcell
      integer       this_hour, this_spec, this_varid, jerr
      integer       data_start_grid(4), data_count_grid(4), ipt
c
      real,   allocatable, dimension(:)   :: emiss_pts
      real,   allocatable, dimension(:)   :: xlocpt
      real,   allocatable, dimension(:)   :: ylocpt
      real,   allocatable, dimension(:,:) :: emiss_grid
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
      allocate( xlocpt(nptsrce)          )
      allocate( ylocpt(nptsrce)          )
      allocate( emiss_grid(nxcell,nycell) )
      data_start_pts(1) = 1
      data_count_pts(1) = nptsrce
      data_count_pts(2) = 1
      data_start_grid(1) = 1
      data_count_grid(1) = nxcell
      data_start_grid(2) = 1
      data_count_grid(2) = nycell
      data_start_grid(3) = 1
      data_count_grid(3) = 1
      data_count_grid(4) = 1
c
c   --- read the source locations ---
c
      this_var = 'xcoord'
      jerr = nf_inq_varid(inpunit, this_var, this_varid)
      if( ierr .EQ. NF_NOERR) goto 7000
      jerr = nf_get_var_real(inpunit,this_varid,xlocpt)
      if( jerr .NE. NF_NOERR) goto 7001
c
      this_var = 'ycoord'
      jerr = nf_inq_varid(inpunit, this_var, this_varid)
      if( ierr .EQ. NF_NOERR) goto 7000
      jerr = nf_get_var_real(inpunit,this_varid,ylocpt)
      if( jerr .NE. NF_NOERR) goto 7001
c
c  ---- loop over the number of timesteps ---
c
      do this_hour=1,ncf_nsteps
          data_start_pts(2) = this_hour
          data_start_grid(4) = this_hour
c
c  ---- loop over species in file ---
c
          do this_spec=1,ncf_nsteps
             emiss_grid = 0.
c
c  --- get point source emissions from file ---
c
             this_var = spcnam(this_spec)
             jerr = nf_inq_varid(inpunit, this_var, this_varid)
             if( jerr .EQ. NF_NOERR) goto 7000
c
             jerr = nf_get_vara_real(inpunit,this_varid,data_start_pts,
     &                                           data_count_pts,emiss_pts)
             if( jerr .NE. NF_NOERR) goto 7001
c
c  ---- calculate the cell containing this source ---
c
             do ipt=1,nptsrce
                icell = 1 + FLOOR( (xlocpt(ipt)/1000. - xorig)/dxcell )
                jcell = 1 + FLOOR( (ylocpt(ipt)/1000. - yorig)/dycell )
                if( icell .GE. 1 .AND. icell .LE. nxcell .AND. 
     &                           jcell .GE. 1 .AND. jcell .LE. nycell ) then
                   emiss_grid(icell,jcell) = emiss_grid(icell,jcell) + emiss_pts(ipt)
                endif
             enddo
c
c  ---- write the grid to the output file ---
c
             jerr = nf_inq_varid(ncf_iowemis, this_var, this_varid)
             if( jerr .EQ. NF_NOERR) goto 7002
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
c
c  --- entire file processed ---
c
      deallocate( emiss_pts  )
      deallocate( xlocpt     )
      deallocate( ylocpt     )
      deallocate( emiss_grid )
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(*,'(//,a)') 'ERROR in NCF_RDPT_EMISS:'
      write(*,'(A)') action(:strmin(action))
      write(*,'(2A)') 'Cannot find variable id in input file for: ',
     &                                      this_var(:strmin(this_var))
      write(*,'(A,I5,//)') 'NetCDF error code: ',jerr
      ierr = IFAIL
      goto 9999
c
 7001 continue
      write(*,'(//,a)') 'ERROR in NCF_RDPT_EMISS:'
      write(*,'(A)') action(:strmin(action))
      write(*,'(2A)') 'Cannot read data for variable: ',
     &                                      this_var(:strmin(this_var))
      write(*,'(A,I5)') 'NetCDF error code: ',jerr
      ierr = IFAIL
      goto 9999

 7002 continue
      write(*,'(//,a)') 'ERROR in NCF_RDPT_EMISS:'
      write(*,'(A)') action(:strmin(action))
      write(*,'(2A)') 'Cannot write data to output file for varaible; ',
     &                                      this_var(:strmin(this_var))
      write(*,'(A,I5,//)') 'NetCDF error code: ',jerr
      ierr = IFAIL
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
