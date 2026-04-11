c**** NCF_SET_GLOBAL
c
      subroutine ncf_set_global(inname,igrd,begin_date,begin_time,
     &                       ending_date,ending_time,nspcs)
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
c         inname      C filetype of this kind of file
c         igrd        I grid index
c         begin_date  I model begin date (YYJJJ)
c         begin_time  R model begin time
c         ending_date I model end date (YYJJJ)
c         ending_time R model end time
c         nspcs       I number of species in this file
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
      include 'uamcntl.inc'
      include 'uamregn.inc'
      include 'layers.inc'
      include 'cntylst.inc'
      include 'ncf_iodat.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      character*10 inname
      integer      igrd
      integer      begin_date
      real         begin_time
      integer      ending_date
      real         ending_time
      integer      nspcs
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
      integer caldate
      real    xorg_grid, yorg_grid, delx_grid, dely_grid
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
c --- get current date/time ---
c
      ncf_sdate = 2000000  + 1000*MOD(INT(begin_time/1000),100) 
     &                                         + MOD(begin_date,100000)
      ncf_sdatec =  2000000 + julcal( MOD(ncf_sdate,100000) )
      ncf_stime = begin_time
      ncf_nlays = nlayer
c
c --- get current date/time ---
c
      ncf_nvars = nspcs
      ncf_ncols = nxcell
      ncf_nrows = nycell
      ncf_p_alp = tlat1
      ncf_p_bet = tlat2
      ncf_p_gam = polelon
      ncf_xcent = polelon
      ncf_ycent = polelat
c
c --- get current date/time ---
c
      ncf_xorig = xorig*1000.
      ncf_yorig = yorig*1000.
      ncf_xcell = dxcell*1000.
      ncf_ycell = dycell*1000.
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
c
c --- get current date/time ---
c
      ncf_iutm = izone
      ncf_istag = 0
      ncf_ftype = 1
      ncf_cproj = 2
      ncf_nthik = 1
      ncf_itzon = itzone
c
c --- get current date/time ---
c
      ncf_name = inname
      ncf_note = fnote
      ncf_filedesc = inname
c
c --- get current date/time ---
c
      ncf_vgtyp = 6
      ncf_vgtop = 10000.
      ncf_vglvls = 0.
      ncf_gdnam = "CAMx v7.20"
      ncf_upnam = "CAMx v7.20"
c
c --- get current date/time ---
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
c
c --- get current date/time ---
c
      return
      end
