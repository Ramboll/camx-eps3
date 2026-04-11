c**** NCF_WRT_VARS_BASE
c
      subroutine ncf_wrt_vars_base(ierr,action,iounit)
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c   This routine writes the variable definitions and descriptions to 
c    the NetCDF file
c
c      Argument description:
c       Inputs:
c           action  C  name of file to open
c           iounit  I  NetCDF file ID of file
c       Outputs:
c           ierr    I  error code
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
      integer grid_dimid(2), time_dimid(3), this_varid
      integer jerr
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      ierr = IFAIL
c
      grid_dimid(1) = ncf_col_dimid
      grid_dimid(2) = ncf_row_dimid
c
      time_dimid(1) = ncf_date_time_dimid
      time_dimid(2) = ncf_var_dimid
      time_dimid(3) = ncf_tstep_dimid
c
      jerr = nf_def_var(iounit, "X", NF_DOUBLE, 1, 
     &                                      ncf_col_dimid, this_varid)
      if( jerr .NE. NF_NOERR ) goto 7000
c
      jerr = nf_put_att_text(iounit,this_varid,'units',
     &                                    strmin(ncf_x_units),ncf_x_units)
      if( jerr .NE. NF_NOERR ) goto 7000
      jerr = nf_put_att_text(iounit,this_varid,'long_name',
     &                              strmin(ncf_x_long_name),ncf_x_long_name)
      if( jerr .NE. NF_NOERR ) goto 7000
      jerr = nf_put_att_text(iounit,this_varid,'var_desc',
     &                                strmin(ncf_x_var_desc),ncf_x_var_desc)
      if( jerr .NE. NF_NOERR ) goto 7000
c
      jerr = nf_def_var(iounit, "Y", NF_DOUBLE, 1, 
     &                                      ncf_row_dimid, this_varid)
      if( jerr .NE. NF_NOERR ) goto 7000
      jerr = nf_put_att_text(iounit,this_varid,'units',
     &                                    strmin(ncf_y_units),ncf_y_units)
      if( jerr .NE. NF_NOERR ) goto 7000
      jerr = nf_put_att_text(iounit,this_varid,'long_name',
     &                            strmin(ncf_y_long_name),ncf_y_long_name)
      if( jerr .NE. NF_NOERR ) goto 7000
      jerr = nf_put_att_text(iounit,this_varid,'var_desc',
     &                             strmin(ncf_y_var_desc),ncf_y_var_desc)
      if( jerr .NE. NF_NOERR ) goto 7000
c
      jerr = nf_def_var(iounit, "TFLAG", NF_INT, 3, 
     &                                  time_dimid, this_varid)
      if( jerr .NE. NF_NOERR ) goto 7000
      jerr = nf_put_att_text(iounit,this_varid,'units',
     &                          strmin(ncf_tflag_units),ncf_tflag_units)
      if( jerr .NE. NF_NOERR ) goto 7000
      jerr = nf_put_att_text(iounit,this_varid,'long_name',
     &                  strmin(ncf_tflag_long_name),ncf_tflag_long_name)
      if( jerr .NE. NF_NOERR ) goto 7000
      jerr = nf_put_att_text(iounit,this_varid,'var_desc',
     &                     strmin(ncf_tflag_var_desc),ncf_tflag_var_desc)
      if( jerr .NE. NF_NOERR ) goto 7000
c
      jerr = nf_def_var(iounit, "ETFLAG", NF_INT, 3, 
     &                                 time_dimid, this_varid)
      if( jerr .NE. NF_NOERR ) goto 7000
      jerr = nf_put_att_text(iounit,this_varid,'units',
     &                        strmin(ncf_etflag_units),ncf_etflag_units)
      if( jerr .NE. NF_NOERR ) goto 7000
      jerr = nf_put_att_text(iounit,this_varid,'long_name',
     &                strmin(ncf_etflag_long_name),ncf_etflag_long_name)
      if( jerr .NE. NF_NOERR ) goto 7000
      jerr = nf_put_att_text(iounit,this_varid,'var_desc',
     &                  strmin(ncf_etflag_var_desc),ncf_etflag_var_desc)
      if( jerr .NE. NF_NOERR ) goto 7000
c
      jerr = nf_def_var(iounit, "longitude", NF_DOUBLE, 2, 
     &                              grid_dimid, this_varid)
      if( jerr .NE. NF_NOERR ) goto 7000
      jerr = nf_put_att_text(iounit,this_varid,'units',
     &                  strmin(ncf_longitude_units),ncf_longitude_units)
      if( jerr .NE. NF_NOERR ) goto 7000
      jerr = nf_put_att_text(iounit,this_varid,'long_name',
     &          strmin(ncf_longitude_long_name),ncf_longitude_long_name)
      if( jerr .NE. NF_NOERR ) goto 7000
      jerr = nf_put_att_text(iounit,this_varid,'var_desc',
     &             strmin(ncf_longitude_var_desc),ncf_longitude_var_desc)
      if( jerr .NE. NF_NOERR ) goto 7000
      jerr = nf_put_att_text(iounit,this_varid,'coordinates',
     &        strmin(ncf_longitude_coordinates),ncf_longitude_coordinates)
      if( jerr .NE. NF_NOERR ) goto 7000
c
      jerr = nf_def_var(iounit, "latitude", NF_DOUBLE, 2, 
     &                                grid_dimid, this_varid)
      if( jerr .NE. NF_NOERR ) goto 7000
      jerr = nf_put_att_text(iounit,this_varid,'units',
     &                   strmin(ncf_latitude_units),ncf_latitude_units)
      if( jerr .NE. NF_NOERR ) goto 7000
      jerr = nf_put_att_text(iounit,this_varid,'long_name',
     &          strmin(ncf_latitude_long_name),ncf_latitude_long_name)
      if( jerr .NE. NF_NOERR ) goto 7000
      jerr = nf_put_att_text(iounit,this_varid,'var_desc',
     &             strmin(ncf_latitude_var_desc),ncf_latitude_var_desc)
      if( jerr .NE. NF_NOERR ) goto 7000
      jerr = nf_put_att_text(iounit,this_varid,'coordinates',
     &       strmin(ncf_latitude_coordinates),ncf_latitude_coordinates)
      if( jerr .NE. NF_NOERR ) goto 7000
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c    Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(*,'(//,a)') 'ERROR in NCF_WRT_VARS_BASE:'
      write(*,'(A)') action(:strmin(action))
      write(*,'(A)') 'Cannot create file variable for grid variables.'
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
