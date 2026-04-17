c**** NCF_WRT_DATA_GRID
c
      subroutine ncf_wrt_data_grid(ierr,action,iounit,
     &         grid_ncol,grid_nrow,grid_xorig,grid_yorig,grid_deltax,
     &                           grid_deltay,nlays,grid_lat,grid_lon)
      implicit none
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c   This routine writes the variable data for the grid definiton 
c    variables to the NetCDF file
c
c      Argument description:
c       Inputs:
c           action      C name of file to open
c           iounit      I NetCDF file ID of file
c           grid_ncol   I number of grid cells in X direction
c           grid_nrow   I number of grid cells in Y direction
c           grid_xorig  R X coordinate of grid origin      
c           grid_yorig  R Y coordinate of grid origin      
c           grid_dxcell R cell width in X direction
c           grid_dycell R cell width in Y direction
c           nlays       I number of layers in this file
c           grid_lat    R latitude value of cell center
c           grid_lon    R longitude value of cell center
c       Outputs:
c           ierr        I error code
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
      include 'uamregn.inc'
      include 'netcdf.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer       ierr
      character*(*) action
      integer       iounit
      integer       grid_ncol
      integer       grid_nrow
      real          grid_xorig
      real          grid_yorig
      real          grid_deltax
      real          grid_deltay
      integer       nlays
      real          grid_lat(grid_ncol,grid_nrow)
      real          grid_lon(grid_ncol,grid_nrow)
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
      character*10 this_var
      real*8,      allocatable, dimension(:)   :: darray_1d
      real*8,      allocatable, dimension(:,:) :: darray_2d
      integer,     allocatable, dimension(:)   :: iarray_1d
      integer      this_varid, icol, irow, ilay
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c  --- variable for X coordinates ---
c
      this_var = "X"
      allocate( darray_1d(nxcell) )
      do icol=1,nxcell
        darray_1d(icol) = DBLE((grid_xorig + 
     &                           (REAL(icol)-0.5)*grid_deltax)/1000.)
      enddo
      ierr = nf_inq_varid(iounit,TRIM(this_var),this_varid)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_var_double(iounit,this_varid,darray_1d)
      if( ierr .NE. NF_NOERR ) goto 7001
      deallocate( darray_1d )
c
c  --- variable for Y coordinates ---
c
      this_var = "Y"
      allocate( darray_1d(nycell) )
      do irow=1,nycell
        darray_1d(irow) = DBLE((grid_yorig + 
     &                           (REAL(irow)-0.5)*grid_deltay)/1000.)
      enddo
      ierr = nf_inq_varid(iounit,TRIM(this_var),this_varid)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_var_double(iounit,this_varid,darray_1d)
      if( ierr .NE. NF_NOERR ) goto 7001
      deallocate( darray_1d )
c
c  --- variable for longitude coordinates ---
c
      allocate( darray_2d(nxcell,nycell) )
c
      this_var = "longitude"
      do icol=1,nxcell
        do irow=1,nycell
           darray_2d(icol,irow) = DBLE(grid_lon(icol,irow))
        enddo
      enddo
      ierr = nf_inq_varid(iounit,TRIM(this_var),this_varid)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_var_double(iounit,this_varid,darray_2d)
      if( ierr .NE. NF_NOERR ) goto 7001
c
c  --- variable for latitude coordinates ---
c
      this_var = "latitude"
      do icol=1,nxcell
        do irow=1,nycell
           darray_2d(icol,irow) = DBLE(grid_lat(icol,irow))
        enddo
      enddo
      ierr = nf_inq_varid(iounit,TRIM(this_var),this_varid)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_var_double(iounit,this_varid,darray_2d)
      if( ierr .NE. NF_NOERR ) goto 7001
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c    Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(*,'(//,a)') 'ERROR in NCF_WRT_DATA_GRID:'
      write(*,'(A)') action(:strmin(action))
      write(*,'(2A)') 'Cannot find variable id for: ',
     &                                      this_var(:strmin(this_var))
      write(*,'(A,I5)') 'NetCDF error code: ',ierr
      ierr = IFAIL
      goto 9999
c
 7001 continue
      write(*,'(//,a)') 'ERROR in NCF_WRT_DATA_GRID:'
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
