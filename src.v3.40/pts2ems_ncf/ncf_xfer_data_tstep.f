c**** NCF_XFER_DATA_TSTEP
c
      subroutine ncf_xfer_data_tstep(ierr)
      implicit none
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c   This routine writes the data for the timestep variables to the
c    NetCDF file
c
c     Copyright 1996 - 2018
c     Ramboll
c      Argument description:
c       Inputs:
c       Outputs:
c           ierr        I error code
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'uamcntl.inc'
      include 'ncf_iodat.inc'
      include 'iopts2ems.inc'
      include 'netcdf.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
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
       character*200 action
       character*10  this_var
       integer*4     data_start_in(3), data_count_in(3)
       integer*4     data_start_out(3), data_count_out(3)
       integer*4     this_dimid, this_hour, ispc, jerr
       integer*4     varid_tflag_in, varid_etflag_in
       integer*4     varid_tflag_out, varid_etflag_out
       integer*4     tflag_in(2), etflag_in(2), nvars_timestep
c
       integer,   allocatable, dimension(:,:) :: tflag_out
       integer,   allocatable, dimension(:,:) :: etflag_out
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
       ierr = IFAIL
c
c  --- get the timestep dimensions for input file ---
c
       action = 'Reading timestamp data from input file.'
       this_var = 'VAR'
       jerr = nf_inq_dimid(inpunit, this_var, this_dimid  )
       data_start_in(1) = 1
       data_count_in(1) = 2
       data_start_in(2) = 1
       data_count_in(2) = 1
       data_count_in(3) = 1
       data_start_out(1) = 1
       data_count_out(1) = 2
       data_start_out(2) = 1
       data_count_out(2) = nspecs
       data_count_out(3) = 1
c
       allocate( tflag_out(2,nspecs)  )
       allocate( etflag_out(2,nspecs) )
c
c  --- get varid for timestamp varaibles in input file ---
c
       this_var = 'TFLAG'
       jerr = nf_inq_varid(inpunit,this_var,varid_tflag_in)
       if( jerr .NE. NF_NOERR ) goto 7000
       this_var = 'ETFLAG'
       jerr = nf_inq_varid(inpunit,this_var,varid_etflag_in)
       if( jerr .NE. NF_NOERR ) goto 7000
c
c  --- get varid for timestamp varaibles in output file ---
c
       action = 'Writing timestamp data from output file.'
       this_var = 'TFLAG'
       jerr = nf_inq_varid(ncf_iowemis,this_var,varid_tflag_out)
       if( jerr .NE. NF_NOERR ) goto 7000
       this_var = 'ETFLAG'
       jerr = nf_inq_varid(ncf_iowemis,this_var,varid_etflag_out)
       if( jerr .NE. NF_NOERR ) goto 7000
c
c   --- loop over the number of time steps ---
c
       do this_hour=1,ncf_nsteps
          data_start_in(3) = this_hour
          data_start_out(3) = this_hour
c
c  --- get the TFLAG data from input file ---
c
          action = 'Reading timestamp data from input file.'
          this_var = 'TFLAG'
          jerr = nf_get_vara_int(inpunit,varid_tflag_in,
     &                      data_start_in,data_count_in,tflag_in)
          if( jerr .NE. NF_NOERR ) goto 7001 
          this_var = 'ETFLAG'
          jerr = nf_get_vara_int(inpunit,varid_etflag_in,
     &                      data_start_in,data_count_in,etflag_in)
          if( jerr .NE. NF_NOERR ) goto 7001 
c
c  --- put data in output arrays and write it ---
c
          do ispc=1,nspecs
             tflag_out(1,ispc) = tflag_in(1)
             tflag_out(2,ispc) = tflag_in(2)
             etflag_out(1,ispc) = etflag_in(1)
             etflag_out(2,ispc) = etflag_in(2)
          enddo
          action = 'Writing timestamp data to output file.'
          this_var = 'TFLAG'
          jerr = nf_put_vara_int(ncf_iowemis,varid_tflag_out,
     &                         data_start_out,data_count_out,tflag_out)
          if( jerr .NE. NF_NOERR ) goto 7002
          this_var = 'ETFLAG'
          jerr = nf_put_vara_int(ncf_iowemis,varid_etflag_out,
     &                         data_start_out,data_count_out,etflag_out)
          if( jerr .NE. NF_NOERR ) goto 7002
c
c  --- all hours processed ---
c
      enddo
c
      deallocate( tflag_out  )
      deallocate( etflag_out )
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c    Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(*,'(//,a)') 'ERROR in NCF_XFER_DATA_TSTEP:'
      write(*,'(A)') action(:strmin(action))
      write(*,'(2A)') 'Cannot find variable id for: ',
     &                                      this_var(:strmin(this_var))
      write(*,'(A,I5,//)') 'NetCDF error code: ',jerr
      goto 9999
c
 7001 continue
      write(*,'(//,a)') 'ERROR in NCF_XFER_DATA_TSTEP:'
      write(*,'(A)') action(:strmin(action))
      write(*,'(2A)') 'Cannot read data for the variable: ',
     &                                      this_var(:strmin(this_var))
      write(*,'(A,I5,//)') 'NetCDF error code: ',jerr
      ierr = IFAIL
      goto 9999
c
 7002 continue
      write(*,'(//,a)') 'ERROR in NCF_XFER_DATA_TSTEP:'
      write(*,'(A)') action(:strmin(action))
      write(*,'(2A)') 'Cannot write data for the variable: ',
     &                                      this_var(:strmin(this_var))
      write(*,'(A,I5,//)') 'NetCDF error code: ',jerr
      ierr = IFAIL
      goto 9999
c
 7003 continue
      write(*,'(//,a)') 'ERROR in NCF_XFER_DATA_TSTEP:'
      write(*,'(A)') action(:strmin(action))
      write(*,'(2A)') 'Cannot find dimension id for: ',
     &                                      this_var(:strmin(this_var))
      write(*,'(A,I5,//)') 'NetCDF error code: ',jerr
      goto 9999
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
