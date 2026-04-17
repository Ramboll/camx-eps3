C**** NCF_WRT_DATA_SPECIES
c
      subroutine ncf_wrt_data_species(ierr)
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
c       Outputs:
c           ierr      I error code
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
      include 'uamcntl.inc'
      include 'ncf_iodat.inc'
      include 'iopigems.inc'
      include 'pigemstot.inc'
      include 'stkpigems.inc'
      include 'netcdf.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer       ierr
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
      character*14  this_var
      integer       data_start(2), data_count(2), ispc, ipt
      integer       this_varid, this_hour
c
      real,         allocatable, dimension(:) :: emiss_out
      real,         allocatable, dimension(:) :: plumerise
      real,         allocatable, dimension(:) :: plume_bot
      real,         allocatable, dimension(:) :: plume_top
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c  --- set the position in the NetCDF variable to write ---
c
c
c  --- allocate the array that will be used to write the data ---
c
      allocate( emiss_out(nstklist)    )
      allocate( plumerise(nstklist)    )
      allocate( plume_bot(nstklist)    )
      allocate( plume_top(nstklist)    )
      data_start(1) = 1
      data_count(1) = nstklist
c
c  --- write output for each hour ---
c
      do this_hour=1,MXHOUR
         data_start(2) = this_hour
         data_count(2) = 1
c
c  --- write the timestep for this hour ---
c
         ncf_cur_tstep = this_hour
         action = 'Writing timestamp to output NetCDF file.'
         call ncf_wrt_data_tstep(ierr,action,ncf_iowemis,nspecs)
         if( ierr .NE. ISUCES ) goto 9999
c
c  --- load the plume variables ---
c
         do ipt=1,nstklist
             plumerise(ipt) = 0.
             plume_bot(ipt) = 0.
             plume_top(ipt) = 0.
             if( hgtlist(ipt) .LT. 0 .AND. vellist(ipt) .GE. 0 ) 
     &                                    plumerise(ipt) = -hgtlist(ipt)
             if( hgtlist(ipt) .LT. 0 .AND. vellist(ipt) .LT. 0 
     &                          .AND. hgtlist(ipt) .EQ. vellist(ipt) ) 
     &                                       plumerise(ipt) = -hgtlist(ipt)
             if( hgtlist(ipt) .LT. 0 .AND. vellist(ipt) .LT. 0 
     &                    .AND. hgtlist(ipt) .EQ. vellist(ipt) ) then
                 plume_bot(ipt) = MIN( ABS(vellist(ipt)), ABS(hgtlist(ipt)) )
                 plume_top(ipt) = MAX( ABS(vellist(ipt)), ABS(hgtlist(ipt)) )
             endif
         enddo
c
         this_var = 'plumerise'
         ierr = nf_inq_varid(ncf_iowemis,this_var(:strmin(this_var)),this_varid)
         if( ierr .NE. NF_NOERR ) goto 7000
         ierr = nf_put_vara_real(ncf_iowemis,this_varid,data_start,
     &                                              data_count,plumerise)
         if( ierr .NE. NF_NOERR ) goto 7001
c
         this_var = 'plume_bottom'
         ierr = nf_inq_varid(ncf_iowemis,this_var(:strmin(this_var)),this_varid)
         if( ierr .NE. NF_NOERR ) goto 7000
         ierr = nf_put_vara_real(ncf_iowemis,this_varid,data_start,
     &                                              data_count,plume_bot)
         if( ierr .NE. NF_NOERR ) goto 7001
c
         this_var = 'plume_top'
         ierr = nf_inq_varid(ncf_iowemis,this_var(:strmin(this_var)),this_varid)
         if( ierr .NE. NF_NOERR ) goto 7000
         ierr = nf_put_vara_real(ncf_iowemis,this_varid,data_start,
     &                                              data_count,plume_top)
         if( ierr .NE. NF_NOERR ) goto 7001
c
c  --- loop over all variables in this file ---
c
         action = 'Writing the emissions data to the output NetCDF point source file.'
         do ispc=1,nspecs
c
c  --- get name for this variable, and get it's variable ID
c      in this file ---
c
            this_var = spcnam(ispc)
            call lftjst(this_var)
            ierr = nf_inq_varid(ncf_iowemis,this_var(:strmin(this_var)),this_varid)
            if( ierr .NE. NF_NOERR ) goto 7000
c
c  --- load the data into the local array to write ---
c
            do ipt=1,nstklist
                emiss_out(ipt) = emsstk(ipt,ispc,this_hour)
            enddo
c
            ierr = nf_put_vara_real(ncf_iowemis,this_varid,data_start,data_count,emiss_out)
            if( ierr .NE. NF_NOERR ) goto 7001
c
         enddo
c
c  --- next hour ---
c
      enddo
c
c  ---- deallocate the local arrays ---
c
      deallocate( emiss_out    )
      deallocate( plumerise    )
      deallocate( plume_bot    )
      deallocate( plume_top    )
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c    Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(*,'(//,a)') 'ERROR in NCF_WRT_DATA_SPECIES:'
      write(*,'(A)') action(:strmin(action))
      write(*,'(2A)') 'Cannot find variable id for: ',
     &                                      this_var(:strmin(this_var))
      write(*,'(A,I5,//)') 'NetCDF error code: ',ierr
      ierr = IFAIL
      goto 9999
c
 7001 continue
      write(*,'(//,a)') 'ERROR in NCF_WRT_DATA_SPECIES:'
      write(*,'(A)') action(:strmin(action))
      write(*,'(2A)') 'Cannot write data for the variable: ',
     &                                      this_var(:strmin(this_var))
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
