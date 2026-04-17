c**** NCF_WRT_DATA_PTS
c
      subroutine ncf_wrt_data_pts(ierr)
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
      include 'iopigems.inc'
      include 'stkpigems.inc'
      include 'netcdf.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer ierr
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
      character*200 this_var, action
      real*4,       allocatable, dimension(:)   :: darray_1d
      integer,      allocatable, dimension(:)   :: iarray_1d
      integer       this_varid, ipts
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      action = 'Writing output NetCDF point source emissions file.'
c
c  --- allocate the array for sending ---
c
      allocate( darray_1d(nstklist) )
      allocate( iarray_1d(nstklist) )
c
c  --- variable for X coordinates ---
c
      this_var = "xcoord"
      do ipts=1,nstklist
        darray_1d(ipts) = xloclist(ipts)
      enddo
      ierr = nf_inq_varid(ncf_iowemis,this_var,this_varid)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_var_real(ncf_iowemis,this_varid,darray_1d)
      if( ierr .NE. NF_NOERR ) goto 7001
c
c  --- variable for Y coordinates ---
c
      this_var = "ycoord"
      do ipts=1,nstklist
        darray_1d(ipts) = yloclist(ipts)
      enddo
      ierr = nf_inq_varid(ncf_iowemis,this_var,this_varid)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_var_real(ncf_iowemis,this_varid,darray_1d)
      if( ierr .NE. NF_NOERR ) goto 7001
c
c  --- variable for stack height ---
c
      this_var = "stkheight"
      do ipts=1,nstklist
        darray_1d(ipts) = ABS(hgtlist(ipts))
      enddo
      ierr = nf_inq_varid(ncf_iowemis,this_var,this_varid)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_var_real(ncf_iowemis,this_varid,darray_1d)
      if( ierr .NE. NF_NOERR ) goto 7001
c
c  --- variable for stack diameter ---
c
      this_var = "stkdiam"
      do ipts=1,nstklist
        darray_1d(ipts) = dialist(ipts)
      enddo
      ierr = nf_inq_varid(ncf_iowemis,this_var,this_varid)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_var_real(ncf_iowemis,this_varid,darray_1d)
      if( ierr .NE. NF_NOERR ) goto 7001
c
c  --- variable for stack gas exit temperature ---
c
      this_var = "stktemp"
      do ipts=1,nstklist
        darray_1d(ipts) = tptlist(ipts)
      enddo
      ierr = nf_inq_varid(ncf_iowemis,this_var,this_varid)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_var_real(ncf_iowemis,this_varid,darray_1d)
      if( ierr .NE. NF_NOERR ) goto 7001
c
c  --- variable for stack gas exit velocity ---
c
      this_var = "stkspeed"
      do ipts=1,nstklist
        darray_1d(ipts) = vellist(ipts)
      enddo
      ierr = nf_inq_varid(ncf_iowemis,this_var,this_varid)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_var_real(ncf_iowemis,this_varid,darray_1d)
      if( ierr .NE. NF_NOERR ) goto 7001
c
c  --- variable for PiG flag ---
c
      this_var = "pigflag"
      do ipts=1,nstklist
        iarray_1d(ipts) = 0
        if( dialist(ipts) .LT. 0. ) iarray_1d(ipts) = 1
      enddo
      ierr = nf_inq_varid(ncf_iowemis,this_var,this_varid)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_var_int(ncf_iowemis,this_varid,iarray_1d)
      if( ierr .NE. NF_NOERR ) goto 7001
c
c  --- variable for source region override ---
c
      this_var = "saoverride"
      do ipts=1,nstklist
        iarray_1d(ipts) = 0
      enddo
      ierr = nf_inq_varid(ncf_iowemis,this_var,this_varid)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_var_int(ncf_iowemis,this_varid,iarray_1d)
      if( ierr .NE. NF_NOERR ) goto 7001
c
c  --- deallocate the array and set return flag ---
c
      deallocate( darray_1d )
      deallocate( iarray_1d )
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c    Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(*,'(//,a)') 'ERROR in NCF_WRT_DATA_PTS:'
      write(*,'(A)') action(:strmin(action))
      write(*,'(2A)') 'Cannot find variable id for: ',
     &                                      this_var(:strmin(this_var))
      write(*,'(2A)') 'File: ',ncf_oemsfl(:strmin(ncf_oemsfl))
      write(*,'(A,I5,//)') 'NetCDF error code: ',ierr
      ierr = IFAIL
      goto 9999
c
 7001 continue
      write(*,'(//,a)') 'ERROR in NCF_WRT_DATA_PTS:'
      write(*,'(A)') action(:strmin(action))
      write(*,'(2A)') 'Cannot write data for the variable: ',
     &                                      this_var(:strmin(this_var))
      write(*,'(2A)') 'File: ',ncf_oemsfl(:strmin(ncf_oemsfl))
      write(*,'(A,I5,//)') 'NetCDF error code: ',ierr
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
