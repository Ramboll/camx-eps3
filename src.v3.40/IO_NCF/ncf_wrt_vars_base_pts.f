c**** NCF_WRT_VARS_BASE_PTS
c
      subroutine ncf_wrt_vars_base_pts(ierr,action,iounit)
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
c           ierr    I  error code
c           action  C  name of file to open
c           iounit  I  NetCDF file ID of file
c       Outputs:
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
      integer ptshr_dimid(2), time_dimid(3)
      integer this_varid
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      ptshr_dimid(1) = ncf_col_dimid
      ptshr_dimid(2) = ncf_tstep_dimid
c
      time_dimid(1) = ncf_date_time_dimid
      time_dimid(2) = ncf_var_dimid
      time_dimid(3) = ncf_tstep_dimid
c
      ierr = nf_def_var(iounit, "xcoord", NF_FLOAT, 1, 
     &                                      ncf_col_dimid, this_varid)
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_put_att_text(iounit,this_varid,'units',
     &                        strmin(ncf_xcoord_units),ncf_xcoord_units)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'long_name',
     &                 strmin(ncf_xcoord_long_name),ncf_xcoord_long_name)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'var_desc',
     &                    strmin(ncf_xcoord_var_desc),ncf_xcoord_var_desc)
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_def_var(iounit, "ycoord", NF_FLOAT, 1, 
     &                                      ncf_col_dimid, this_varid)
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_put_att_text(iounit,this_varid,'units',
     &                          strmin(ncf_ycoord_units),ncf_ycoord_units)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'long_name',
     &                 strmin(ncf_ycoord_long_name),ncf_ycoord_long_name)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'var_desc',
     &                    strmin(ncf_ycoord_var_desc),ncf_ycoord_var_desc)
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_def_var(iounit, "stkheight", NF_FLOAT, 1, 
     &                                      ncf_col_dimid, this_varid)
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_put_att_text(iounit,this_varid,'units',
     &                  strmin(ncf_stkheight_units),ncf_stkheight_units)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'long_name',
     &            strmin(ncf_stkheight_long_name),ncf_stkheight_long_name)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'var_desc',
     &               strmin(ncf_stkheight_var_desc),ncf_stkheight_var_desc)
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_def_var(iounit, "stkdiam", NF_FLOAT, 1, 
     &                                      ncf_col_dimid, this_varid)
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_put_att_text(iounit,this_varid,'units',
     &                      strmin(ncf_stkdiam_units),ncf_stkdiam_units)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'long_name',
     &                 strmin(ncf_stkdiam_long_name),ncf_stkdiam_long_name)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'var_desc',
     &                  strmin(ncf_stkdiam_var_desc),ncf_stkdiam_var_desc)
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_def_var(iounit, "stktemp", NF_FLOAT, 1, 
     &                                      ncf_col_dimid, this_varid)
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_put_att_text(iounit,this_varid,'units',
     &                          strmin(ncf_stktemp_units),ncf_stktemp_units)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'long_name',
     &                strmin(ncf_stktemp_long_name),ncf_stktemp_long_name)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'var_desc',
     &                  strmin(ncf_stktemp_var_desc),ncf_stktemp_var_desc)
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_def_var(iounit, "stkspeed", NF_FLOAT, 1, 
     &                                      ncf_col_dimid, this_varid)
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_put_att_text(iounit,this_varid,'units',
     &                      strmin(ncf_stkspeed_units),ncf_stkspeed_units)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'long_name',
     &             strmin(ncf_stkspeed_long_name),ncf_stkspeed_long_name)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'var_desc',
     &                strmin(ncf_stkspeed_var_desc),ncf_stkspeed_var_desc)
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_def_var(iounit, "pigflag", NF_INT, 1, 
     &                                      ncf_col_dimid, this_varid)
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_put_att_text(iounit,this_varid,'units',
     &                      strmin(ncf_pigflag_units),ncf_pigflag_units)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'long_name',
     &                 strmin(ncf_pigflag_long_name),ncf_pigflag_long_name)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'var_desc',
     &                   strmin(ncf_pigflag_var_desc),ncf_pigflag_var_desc)
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_def_var(iounit, "saoverride", NF_INT, 1, 
     &                                      ncf_col_dimid, this_varid)
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_put_att_text(iounit,this_varid,'units',
     &                  strmin(ncf_saoverride_units),ncf_saoverride_units)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'long_name',
     &            strmin(ncf_saoverride_long_name),ncf_saoverride_long_name)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'var_desc',
     &              strmin(ncf_saoverride_var_desc),ncf_saoverride_var_desc)
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_def_var(iounit, "TFLAG", NF_INT, 3,
     &                                  time_dimid, this_varid)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'units',
     &                          strmin(ncf_tflag_units),ncf_tflag_units)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'long_name',
     &                  strmin(ncf_tflag_long_name),ncf_tflag_long_name)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'var_desc',
     &                     strmin(ncf_tflag_var_desc),ncf_tflag_var_desc)
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_def_var(iounit, "ETFLAG", NF_INT, 3,
     &                                 time_dimid, this_varid)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'units',
     &                        strmin(ncf_etflag_units),ncf_etflag_units)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'long_name',
     &                strmin(ncf_etflag_long_name),ncf_etflag_long_name)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'var_desc',
     &                  strmin(ncf_etflag_var_desc),ncf_etflag_var_desc)
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_def_var(iounit, "flowrate", NF_FLOAT, 2,
     &                                 ptshr_dimid, this_varid)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'units',
     &                        strmin(ncf_flowrate_units),ncf_flowrate_units)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'long_name',
     &                strmin(ncf_flowrate_long_name),ncf_flowrate_long_name)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'var_desc',
     &                  strmin(ncf_flowrate_var_desc),ncf_flowrate_var_desc)
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_def_var(iounit, "plumerise", NF_FLOAT, 2,
     &                                 ptshr_dimid, this_varid)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'units',
     &                        strmin(ncf_plumerise_units),ncf_plumerise_units)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'long_name',
     &                strmin(ncf_plumerise_long_name),ncf_plumerise_long_name)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'var_desc',
     &                  strmin(ncf_plumerise_var_desc),ncf_plumerise_var_desc)
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_def_var(iounit, "plume_bottom", NF_FLOAT, 2,
     &                                 ptshr_dimid, this_varid)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'units',
     &                        strmin(ncf_plume_bot_units),ncf_plume_bot_units)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'long_name',
     &                strmin(ncf_plume_bot_long_name),ncf_plume_bot_long_name)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'var_desc',
     &                  strmin(ncf_plume_bot_var_desc),ncf_plume_bot_var_desc)
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = nf_def_var(iounit, "plume_top", NF_FLOAT, 2,
     &                                 ptshr_dimid, this_varid)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'units',
     &                        strmin(ncf_plume_top_units),ncf_plume_top_units)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'long_name',
     &                strmin(ncf_plume_top_long_name),ncf_plume_top_long_name)
      if( ierr .NE. NF_NOERR ) goto 7000
      ierr = nf_put_att_text(iounit,this_varid,'var_desc',
     &                  strmin(ncf_plume_top_var_desc),ncf_plume_top_var_desc)
      if( ierr .NE. NF_NOERR ) goto 7000
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c    Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(*,'(//,a)') 'ERROR in NCF_WRT_VARS_BASE_PTS:'
      write(*,'(A)') action(:strmin(action))
      write(*,'(A)') 'Cannot create file variable for grid variables.'
      write(*,'(A,I5,//)') 'NetCDF error code: ',ierr
      ierr = IFAIL
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
