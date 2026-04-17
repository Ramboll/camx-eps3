c**** NCF_WRT_DATA_TSTEP
c
      subroutine ncf_wrt_data_tstep(ierr,action,iounit,nspcs)
      implicit none
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c   This routine writes the data for the timestep variables to the
c    NetCDF file
c
c      Argument description:
c       Inputs:
c           action C name of file to open
c           iounit I NetCDF file ID of file
c           nspcs  I number of species in the file
c       Outputs:
c           ierr   i error code
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'ncf_iodat.inc'
      include 'netcdf.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer       ierr
      character*(*) action
      integer       iounit
      integer       nspcs
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
      integer strmin
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*200 this_var
      integer,      allocatable, dimension(:,:) :: iarray_2d
      integer       this_varid, ispc, istep, nvars_time
      integer       data_start(3), data_count(3)
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
       data_start(1) = 1
       data_count(1) = 2
       data_start(2) = 1
       data_start(3) = ncf_cur_tstep
       data_count(3) = 1
c
c  --- get dimid for timestep variables ---
c
       ierr = nf_inq_dimlen(iounit,ncf_var_dimid,nvars_time)
       data_count(2) = nvars_time
c
c  --- variable for TFLAG ---
c
      allocate( iarray_2d(2,nvars_time) )
      do ispc=1,nvars_time
        if (ncf_tflag(1,ncf_cur_tstep)/1000. GT. 80) then
          iarray_2d(1,ispc) = 1900000+ncf_tflag(1,ncf_cur_tstep)
        else
          iarray_2d(1,ispc) = 2000000+ncf_tflag(1,ncf_cur_tstep)
        endif
        iarray_2d(2,ispc) = ncf_tflag(2,ncf_cur_tstep)
      enddo
      this_var = "TFLAG"
      this_varid = 0
      ierr = nf_inq_varid(iounit,this_var,this_varid)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_vara_int(iounit,this_varid,data_start,data_count,iarray_2d)
      if( ierr .NE. NF_NOERR ) goto 7001
      deallocate( iarray_2d )
c
c  --- variable for ETFLAG ---
c
      allocate( iarray_2d(2,nvars_time) )
      do ispc=1,nvars_time
        iarray_2d(1,ispc) = 2000000+ncf_etflag(1,ncf_cur_tstep)
        iarray_2d(2,ispc) = ncf_etflag(2,ncf_cur_tstep)
      enddo
      this_var = "ETFLAG"
      this_varid = 0
      ierr = nf_inq_varid(iounit,this_var,this_varid)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_vara_int(iounit,this_varid,data_start,data_count,iarray_2d)
      if( ierr .NE. NF_NOERR ) goto 7001
      deallocate( iarray_2d )

      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c    Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(*,'(//,a)') 'ERROR in NCF_WRT_DATA_TSTEP:'
      write(*,'(A)') action(:strmin(action))
      write(*,'(2A)') 'Cannot find variable id for: ',
     &                                      this_var(:strmin(this_var))
      write(*,'(A,I5)') 'NetCDF error code: ',ierr
      ierr = IFAIL
      goto 9999
c
 7001 continue
      write(*,'(//,a)') 'ERROR in NCF_WRT_DATA_TSTEP:'
      write(*,'(A)') action(:strmin(action))
      write(*,'(2A)') 'Cannot write data for the variable: ',
     &                                      this_var(:strmin(this_var))
      write(*,'(A,I5)') 'NetCDF error code: ',ierr
      ierr = IFAIL
      goto 9999
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
