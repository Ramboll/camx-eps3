!**** NCF_WRT_VARS_SPECIES_PTS
!
      subroutine ncf_wrt_vars_species_pts(ierr,action,iounit,numpts,        &
                         nspcs,spnames,spunits,splong,spdesc,spcoord)
      use eps3ncf_incs
      implicit none
!
!-----------------------------------------------------------------------
!    Description:
!-----------------------------------------------------------------------
!
!   This routine writes the variable definitions and descriptions to 
!    the NetCDF file
!
!      Argument description:
!       Inputs:
!           ierr     I  error code
!           action   C  name of file to open
!           iounit   I  NetCDF file ID of file
!           numpts   I  number of point sources
!           nspcs    I  number of species in the file
!           spnames  C  names of each species
!           spunits  C  units for each species
!           splong   C  long name for each species
!           spdesc   C  description of each species
!           spcoord  C  description of each species
!       Outputs:
!
!-----------------------------------------------------------------------
!    LOG:
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!    Include files:
!-----------------------------------------------------------------------
!
      include 'netcdf.inc'
!
!-----------------------------------------------------------------------
!    Argument declarations:
!-----------------------------------------------------------------------
!
      integer       ierr
      character*(*) action
      integer       iounit
      integer       numpts
      integer       nspcs
      character*(*) spnames(nspcs)
      character*(*) spunits(nspcs)
      character*(*) splong(nspcs)
      character*(*) spdesc(nspcs)
      character*(*) spcoord(nspcs)
!
!-----------------------------------------------------------------------
!    External functions:
!-----------------------------------------------------------------------
!
      integer strmin
!
!-----------------------------------------------------------------------
!    Local variables:
!-----------------------------------------------------------------------
!
      character*14 this_species
      integer      ispc, spec_dimid(2), spec_chunk(2), spec_varid
!
!-----------------------------------------------------------------------
!    Entry point:
!-----------------------------------------------------------------------
!
      spec_chunk(1) = INT(REAL(numpts)/NCF_CHUNK_SIZE_VAR_X)
      spec_chunk(2) = 1
!
      spec_dimid(1) = ncf_col_dimid
      spec_dimid(2) = ncf_tstep_dimid
!
      do ispc=1,nspcs
!
!  --- define everything for this species ---
!
        this_species = spnames(ispc)(:strmin(spnames(ispc)))
        ncf_species_units = spunits(ispc)
        ncf_species_long_name = splong(ispc)
        ncf_species_var_desc = spdesc(ispc)
        ncf_species_coordinates = spcoord(ispc)
!
!  --- define the variable ---
!
        ierr = nf_def_var(iounit,this_species(:strmin(this_species)),  &
                          NF_FLOAT, 2, spec_dimid, spec_varid)
        if( ierr .NE. NF_NOERR ) goto 7000
!
!  --- add the attributes ---
!
        ierr = nf_put_att_text(iounit,spec_varid,'long_name', &
                strmin(ncf_species_long_name),ncf_species_long_name)
        if( ierr .NE. NF_NOERR ) goto 7000

        ierr = nf_put_att_text(iounit,spec_varid,'units', &
                        strmin(ncf_species_units),ncf_species_units)
        if( ierr .NE. NF_NOERR ) goto 7000

        ierr = nf_put_att_text(iounit,spec_varid,'var_desc', &
                  strmin(ncf_species_var_desc),ncf_species_var_desc)
        if( ierr .NE. NF_NOERR ) goto 7000

        ierr = nf_put_att_text(iounit,spec_varid,'coordinates', &
                 strmin(ncf_species_coordinates),ncf_species_coordinates)
        if( ierr .NE. NF_NOERR ) goto 7000
!
!  --- chunkers go here ... Irie! ----
!
#ifdef CHUNK
        if( ncf_compress ) then
          ierr = nf_def_var_chunking(iounit, spec_varid, &
                                             NF_CHUNKED, spec_chunk)
          if( ierr .NE. NF_NOERR ) goto 7001
!
          ierr = nf_def_var_deflate(iounit, spec_varid, NCF_SHUFFLE,  &
                                      NCF_DEFLATE, NCF_DEFLATE_LEVEL )
          if( ierr .NE. NF_NOERR ) goto 7001
        endif
#endif
!
      enddo
!
      ierr = ISUCES
      goto 9999
!
!-----------------------------------------------------------------------
!    Error messages:
!-----------------------------------------------------------------------
!
 7000 continue
      write(*,'(//,A)') 'ERROR in NCF_WRT_VARS_SPECIES:'
      write(*,'(A)') action(:strmin(action))
      write(*,'(2A,//)') 'Cannot create file variable for species: ', &
                                    this_species(:strmin(this_species))
      ierr = IFAIL
      goto 9999
!
 7001 continue
      write(*,'(//,A)') 'ERROR in NCF_WRT_VARS_SPECIES:'
      write(*,'(A)') action(:strmin(action))
      write(*,'(2A,//)') 'Cannot set chunk parameters for species: ', &
                                    this_species(:strmin(this_species))
      ierr = IFAIL
      goto 9999
!
 7002 continue
      write(*,'(//,A)') 'ERROR in NCF_IODAT.INC:'
      write(*,'(A)') action(:strmin(action))
      write(*,'(A)') 'Cannot set chunk parameters for this file.'
      write(*,'(2A,/,A)') 'The NCF_CHUNK_SIZE_VAR_X parameter causes the ', &
           'chunk value to be larger','than the number of point sources.'
      write(*,'(A,I3,A,I4,//)') 'Number of columns: ',numpts
      ierr = IFAIL
      goto 9999
!
!-----------------------------------------------------------------------
!    Return point:
!-----------------------------------------------------------------------
!
 9999 continue
      return
      end
