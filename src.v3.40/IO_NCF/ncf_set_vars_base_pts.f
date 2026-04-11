c**** NCF_SET_VARS_BASE_PTS
c
      subroutine ncf_set_vars_base_pts()
      implicit none
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c   This routine sets the variable definitions and descriptions to 
c   the NetCDF file
c
c      Argument description:
c       Inputs:
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
      include 'uamregn.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      ncf_xcoord_units = "m"
      ncf_xcoord_long_name = "source X coordinate"
      if( llatlon ) then
         ncf_xcoord_var_desc = "longitude degrees east"
      else
         ncf_xcoord_var_desc = "X cartesian distance from projection origin"
      endif
      ncf_ycoord_units = "m"
      ncf_ycoord_long_name = "source Y coordinate"
      if( llatlon ) then
         ncf_ycoord_var_desc = "latitude degrees north"
      else
         ncf_ycoord_var_desc = "Y cartesian distance from projection origin"
      endif
      ncf_stkheight_units = "m"
      ncf_stkheight_long_name = "stack height"
      ncf_stkheight_var_desc = "source stack height"
      ncf_stkdiam_units = "m"
      ncf_stkdiam_long_name = "stack diameter"
      ncf_stkdiam_var_desc = "source stack diameter"
      ncf_stktemp_units = "K"
      ncf_stktemp_long_name = "stack temperature"
      ncf_stktemp_var_desc = "source stack exit temperature"
      ncf_stkspeed_units = "m hr-1"
      ncf_stkspeed_long_name = "stack exit velocity"
      ncf_stkspeed_var_desc = "source stack exit velocity"
      ncf_pigflag_units = " "
      ncf_pigflag_long_name = "PiG flag"
      ncf_pigflag_var_desc = "PiG flag (0=off 1=PiG)"
      ncf_saoverride_units = " "
      ncf_saoverride_long_name = "SA region override"
      ncf_saoverride_var_desc = "SA region override (0=off)"
      ncf_flowrate_units = "m3 hr-1"
      ncf_flowrate_long_name = "source stack flow rate"
      ncf_flowrate_var_desc = "source stack flow rate (0=use exit velocity)"
      ncf_plumerise_units = "m"
      ncf_plumerise_long_name = "plume rise override"
      ncf_plumerise_var_desc = "plume rise override (0=calculate plume rise) " 
      ncf_plume_bot_units = "m"
      ncf_plume_bot_long_name = "plume rise distribution bottom"
      ncf_plume_bot_var_desc = "bottom of plume" 
      ncf_plume_top_units = "m"
      ncf_plume_top_long_name = "plume rise distribution top"
      ncf_plume_top_var_desc = "top of plume" 
c
      ncf_tflag_units = "YYYYDDD,HHMMSS"
      ncf_tflag_long_name = "Start time flag"
      ncf_tflag_var_desc = "Timestep start date and time"
      ncf_etflag_units = "YYYYDDD,HHMMSS"
      ncf_etflag_long_name = "End time flag"
      ncf_etflag_var_desc = "Timestep end date and time"
c
      goto 9999
c
c-----------------------------------------------------------------------
c    Error messages:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
