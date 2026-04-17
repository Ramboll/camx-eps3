c**** UAM2NCF_GLOBAL
c
      subroutine uam2ncf_global()
      implicit none
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c   This routine sets the global file attributes for the NetCDF file
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
      include 'uamregn.inc'
      include 'uamcntl.inc'
      include 'ncf_iodat.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
      integer julcal
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c --- get current date/time ---
c
      call getime_camx(ncf_cdate,ncf_ctime)
      ncf_wdate = ncf_cdate
      ncf_wtime = ncf_ctime
c
      ncf_sdate = 2000000  + 1000*MOD(INT(ibgdat/1000),100) 
     &                                         + MOD(ibgdat,1000)
      ncf_sdatec =  2000000 + julcal( MOD(ncf_sdate,100000) )
      ncf_stime = begtim
      ncf_tstep = 10000
      ncf_nlays = 1
      ncf_nvars = nspecs
      ncf_ncols = nxcell
      ncf_nrows = nycell
      ncf_p_alp = tlat1
      ncf_p_bet = tlat2
      ncf_p_gam = polelon
      ncf_xcent = polelon
      ncf_ycent = polelat
      ncf_xorig = xorig*1000
      ncf_yorig = yorig*1000
      ncf_xcell = dxcell*1000
      ncf_ycell = dycell*1000
      if( llatlon ) then
         ncf_cproj = 0
         ncf_gdtyp = 1
      else if( lutm ) then
         ncf_cproj = 1
         ncf_gdtyp = 5
      else if( lambrt ) then
         ncf_cproj = 2
         ncf_gdtyp = 2
      else if( lrpolar ) then
         ncf_cproj = 3
         ncf_gdtyp = 4
      else if( lpolar ) then
         ncf_cproj = 4
         ncf_gdtyp = 6
      else if( lmerc ) then
         ncf_cproj = 5
         ncf_gdtyp = 7
      endif
      ncf_iutm = izone
      ncf_ftype = 1
      ncf_cproj = 2
      ncf_nthik = 1
      ncf_name = 'EMISSIONS'
      ncf_note = fnote
      ncf_filedesc = 'EMISSIONS'
      ncf_vgtyp = 6
      ncf_vgtop = 10000.
      ncf_vglvls = 0.
      ncf_gdnam = "CAMx v7.20"
      ncf_upnam = "CAMx v7.20"
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
