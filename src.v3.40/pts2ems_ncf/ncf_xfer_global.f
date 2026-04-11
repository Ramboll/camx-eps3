c**** NCF_XFER_GLOBAL
c
      subroutine ncf_xfer_global(ierr)
      implicit none
c
c----CAMx v6.40 161223
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c      Reads the header data for a file and sends it back
c
c     Copyright 1996 - 2016
c     Ramboll Environ
c
c       Argument description:
c         Outputs:
c       ierrr            I error code
c         Inputs:
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
      include 'embrdat.inc'
      include 'ncf_iodat.inc'
      include 'iopts2ems.inc'
      include 'pts2emstot.inc'
      include 'netcdf.inc'
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
      integer strmin
      integer julcal
c
c-----------------------------------------------------------------------
c    Argument declaration:
c-----------------------------------------------------------------------
c
      integer ierr
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
      integer*4 fndchr
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*2000 varlist
      character*60   var_text
      character*16   var_name
      character*10   this_var
      integer        this_dimid, dim_len, var_int, string_length, ispc
      integer        jerr, ncount, idxspc, i
      real           var_real
      real*8         var_double
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      ierr = IFAIL
c
c  --- get the number of point sources in input file ---
c
      this_var = "COL"
      jerr = nf_inq_dimid(inpunit, this_var(:strmin(this_var)), this_dimid )
      if( jerr .NE. NF_NOERR ) goto 7002
      jerr = nf_inq_dimlen(inpunit, this_dimid, nptsrce )
      if( nptsrce .GT. MXPTSRCE ) goto 7002
c
      this_var = "VAR"
      jerr = nf_inq_dimid(inpunit, this_var(:strmin(this_var)), this_dimid )
      if( jerr .NE. NF_NOERR ) goto 7002
      jerr = nf_inq_dimlen(inpunit, this_dimid, nspecs )
      if( nptsrce .GT. MXPTSRCE ) goto 7002
c
      this_var = 'SDATE'
      jerr = nf_get_att_int(inpunit, NF_GLOBAL, this_var, ncf_sdate)
      ncf_sdate =  2000000 + MOD(ncf_sdate,100000) 
      jerr = nf_put_att_int(ncf_iowemis, NF_GLOBAL, this_var, NF_INT, 1, ncf_sdate)
c
      this_var = 'SDATEC'
      ncf_sdatec =  2000000 + julcal( MOD(ncf_sdate,100000) )
      jerr = nf_put_att_int(ncf_iowemis, NF_GLOBAL, this_var, NF_INT, 1, ncf_sdatec)
c
      this_var = 'STIME'
      jerr = nf_get_att_int(inpunit, NF_GLOBAL, this_var, var_int)
      jerr = nf_put_att_int(ncf_iowemis, NF_GLOBAL, this_var, NF_INT, 1, var_int)
c
      this_var = 'TSTEP'
      jerr = nf_get_att_int(inpunit, NF_GLOBAL, this_var, var_int)
      jerr = nf_put_att_int(ncf_iowemis, NF_GLOBAL, this_var, NF_INT, 1, var_int)
c
      this_var = 'NSTEPS'
      jerr = nf_get_att_int(inpunit, NF_GLOBAL, this_var, var_int)
      jerr = nf_put_att_int(ncf_iowemis, NF_GLOBAL, this_var, NF_INT, 1, var_int)
      ncf_nsteps = var_int
c
      this_var = 'NCOLS'
      jerr = nf_put_att_int(ncf_iowemis, NF_GLOBAL, this_var, NF_INT, 1, nxcell)
c
      this_var = 'NROWS'
      jerr = nf_put_att_int(ncf_iowemis, NF_GLOBAL, this_var, NF_INT, 1, nycell)
c
      this_var = 'NLAYS'
      jerr = nf_put_att_int(ncf_iowemis, NF_GLOBAL, this_var, NF_INT, 1, 1)
c
      this_var = 'P_ALP'
      jerr = nf_get_att_double(inpunit, NF_GLOBAL, this_var, var_double)
      jerr = nf_put_att_double(ncf_iowemis, NF_GLOBAL, this_var, NF_DOUBLE, 1, var_double)
c
      this_var = 'P_BET'
      jerr = nf_get_att_double(inpunit, NF_GLOBAL, this_var, var_double)
      jerr = nf_put_att_double(ncf_iowemis, NF_GLOBAL, this_var, NF_DOUBLE, 1, var_double)
c
      this_var = 'P_GAM'
      jerr = nf_get_att_double(inpunit, NF_GLOBAL, this_var, var_double)
      jerr = nf_put_att_double(ncf_iowemis, NF_GLOBAL, this_var, NF_DOUBLE, 1, var_double)
c
      this_var = 'XCENT'
      jerr = nf_get_att_double(inpunit, NF_GLOBAL, this_var, var_double)
      jerr = nf_put_att_double(ncf_iowemis, NF_GLOBAL, this_var, NF_DOUBLE, 1, var_double)
c
      this_var = 'YCENT'
      jerr = nf_get_att_double(inpunit, NF_GLOBAL, this_var, var_double)
      jerr = nf_put_att_double(ncf_iowemis, NF_GLOBAL, this_var, NF_DOUBLE, 1, var_double)
c
      this_var = 'XORIG'
      var_double = DBLE( xorig*1000. )
      jerr = nf_put_att_double(ncf_iowemis, NF_GLOBAL, this_var, NF_DOUBLE, 1, var_double)
c
      this_var = 'YORIG'
      var_double = DBLE( yorig*1000. )
      jerr = nf_put_att_double(ncf_iowemis, NF_GLOBAL, this_var, NF_DOUBLE, 1, var_double)
c
      this_var = 'XCELL'
      var_double = DBLE( dxcell*1000. )
      jerr = nf_put_att_double(ncf_iowemis, NF_GLOBAL, this_var, NF_DOUBLE, 1, var_double)
c
      this_var = 'YCELL'
      var_double = DBLE( dycell*1000. )
      jerr = nf_put_att_double(ncf_iowemis, NF_GLOBAL, this_var, NF_DOUBLE, 1, var_double)
c
      this_var = 'CPROJ'
      jerr = nf_get_att_int(inpunit, NF_GLOBAL, this_var, var_int)
      jerr = nf_put_att_int(ncf_iowemis, NF_GLOBAL, this_var, NF_INT, 1, var_int)
c
      this_var = 'GDTYP'
      jerr = nf_get_att_int(inpunit, NF_GLOBAL, this_var, var_int)
      jerr = nf_put_att_int(ncf_iowemis, NF_GLOBAL, this_var, NF_INT, 1, var_int)
c
      this_var = 'IUTM'
      jerr = nf_get_att_int(inpunit, NF_GLOBAL, this_var, var_int)
      jerr = nf_put_att_int(ncf_iowemis, NF_GLOBAL, this_var, NF_INT, 1, var_int)
c
      this_var = 'ISTAG'
      jerr = nf_get_att_int(inpunit, NF_GLOBAL, this_var, var_int)
      jerr = nf_put_att_int(ncf_iowemis, NF_GLOBAL, this_var, NF_INT, 1, var_int)
c
      this_var = 'ITZON'
      jerr = nf_get_att_int(inpunit, NF_GLOBAL, this_var, var_int)
      jerr = nf_put_att_int(ncf_iowemis, NF_GLOBAL, this_var, NF_INT, 1, var_int)
c
      this_var = 'CAMx_NAME'
      var_text = 'EMISSIONS                                                   '
      jerr = nf_put_att_text(ncf_iowemis, NF_GLOBAL, this_var, strmin(var_text), var_text)
c
      this_var = 'NOTE'
      jerr = nf_get_att_text(inpunit, NF_GLOBAL, this_var, var_text)
      jerr = nf_put_att_text(ncf_iowemis, NF_GLOBAL, this_var, strmin(var_text), var_text)
c
      this_var = 'FILEDESC'
      jerr = nf_get_att_text(inpunit, NF_GLOBAL, this_var, var_text)
      var_text = 'EMISSIONS'
      jerr = nf_put_att_text(ncf_iowemis, NF_GLOBAL, this_var, strmin(var_text), var_text)
c
      this_var = 'FTYPE'
      jerr = nf_get_att_int(inpunit, NF_GLOBAL, this_var, var_int)
      jerr = nf_put_att_int(ncf_iowemis, NF_GLOBAL, this_var, NF_INT, 1, var_int)
c
      this_var = 'NTHIK'
      jerr = nf_get_att_int(inpunit, NF_GLOBAL, this_var, var_int)
      jerr = nf_put_att_int(ncf_iowemis, NF_GLOBAL, this_var, NF_INT, 1, var_int)
c
      this_var = 'VGTYP'
      jerr = nf_get_att_int(inpunit, NF_GLOBAL, this_var, var_int)
      jerr = nf_put_att_int(ncf_iowemis, NF_GLOBAL, this_var, NF_INT, 1, var_int)
c
      this_var = 'VGTOP'
      jerr = nf_get_att_real(inpunit, NF_GLOBAL, this_var, var_real)
      jerr = nf_put_att_real(ncf_iowemis, NF_GLOBAL, this_var, NF_REAL, 1, var_real)
c
      this_var = 'VGLVLS'
      ncf_vglvls = 0.
      jerr = nf_get_att_int(inpunit, NF_GLOBAL, this_var, var_int)
      jerr = nf_put_att_int(ncf_iowemis, NF_GLOBAL, this_var, NF_INT, 2, ncf_vglvls)
c
      this_var = 'GDNAM'
      jerr = nf_get_att_text(inpunit, NF_GLOBAL, this_var, var_text)
      jerr = nf_put_att_text(ncf_iowemis, NF_GLOBAL, this_var, strmin(var_text), var_text)
c
      this_var = 'UPNAM'
      jerr = nf_get_att_text(inpunit, NF_GLOBAL, this_var, var_text)
      jerr = nf_put_att_text(ncf_iowemis, NF_GLOBAL, this_var, strmin(var_text), var_text)
c
      call getime_camx(ncf_cdate,ncf_ctime)
      ncf_wdate = ncf_cdate
      ncf_wtime = ncf_ctime
c
      this_var = 'CDATE'
      jerr = nf_put_att_int(ncf_iowemis, NF_GLOBAL, this_var, NF_INT, 1, ncf_cdate)
c
      this_var = 'CTIME'
      jerr = nf_put_att_int(ncf_iowemis, NF_GLOBAL, this_var, NF_INT, 1, ncf_ctime)
c
      this_var = 'WDATE'
      jerr = nf_put_att_int(ncf_iowemis, NF_GLOBAL, this_var, NF_INT, 1, ncf_wdate)
c
      this_var = 'WTIME'
      jerr = nf_get_att_int(inpunit, NF_GLOBAL, this_var, var_int)
      jerr = nf_put_att_int(ncf_iowemis, NF_GLOBAL, this_var, NF_INT, 1, ncf_wtime)
c
      this_var = 'IOAPI_VERSION'
      var_text = 'IOAPI-CAMx'
      jerr = nf_put_att_text(ncf_iowemis, NF_GLOBAL, this_var, strmin(var_text), var_text)
c
      this_var = 'EXEC_ID'
      var_text = 'PTST2EMS_NCF'
      jerr = nf_put_att_text(ncf_iowemis, NF_GLOBAL, this_var, strmin(var_text), var_text)
c
      this_var = 'HISTORY'
      jerr = nf_get_att_text(inpunit, NF_GLOBAL, this_var, var_text)
      var_text = var_text(:strmin(var_text))//' '//VERSON
      jerr = nf_put_att_text(ncf_iowemis, NF_GLOBAL, this_var, strmin(var_text), var_text)
c
      this_var = 'VAR-LIST'
      jerr = nf_get_att_text(inpunit, NF_GLOBAL, this_var, varlist)
c
      ncount = nspecs
      nspecs = 0
      do ispc=1,ncount
         var_name = varlist(1+(ispc-1)*16:16+(ispc-1)*16)
         if( var_name .EQ. 'flowrate        ' ) cycle
         if( var_name .EQ. 'plumerise       ' ) cycle
         if( var_name .EQ. 'plume_bottom    ' ) cycle
         if( var_name .EQ. 'plume_top       ' ) cycle
         nspecs = nspecs + 1
         spcnam(nspecs) = var_name(:10)
         idxspc = fndchr( spcnam(nspecs), 10, spcnam_pkt, nspecs_pkt )
         if( idxspc .GT. 0 ) then
           molwt_out(nspecs) = molwt(idxspc)
           lgas_out(nspecs) = lgas(idxspc)
         else
           write(IOWMSG,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
           write(IOWMSG,'(4A)',ERR=9999) 'WARNING: Species in input ',
     &             'file not found in /SPECIES LIST/ packet.'
           write(IOWMSG,'(4A,/,A)',ERR=9999) '         By default,',
     &     ' the species: ',spcnam(nspecs)(:strmin(spcnam(nspecs))),
     &                        ' will be considered a GAS speicies, ', 
     &                           '         with molcular weight of 1.'
           write(IOWMSG,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
           nwarn = nwarn + 1
         endif
      enddo
      varlist = ' '
      do ispc=1,nspecs
         varlist(1+(ispc-1)*16:16+(ispc-1)*16) = spcnam(ispc)//'      '
         lgas(ispc) = lgas_out(ispc)
      enddo
      string_length = 16*nspecs + 1
c
      this_var = 'NVARS'
      jerr = nf_put_att_int(ncf_iowemis, NF_GLOBAL, this_var, NF_INT, 1,  nspecs)
      this_var = 'VAR-LIST'
      jerr = nf_put_att_text(ncf_iowemis, NF_GLOBAL, this_var, string_length, varlist )
c
       ierr = ISUCES
       goto 9999
c
c-----------------------------------------------------------------------
c    Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(*,'(//,A)') 'ERROR in NCF_XFER_GLOBAL: '
      write(*,'(A)') 'Reading input point source file.'
      write(*,'(2A)') 'Cannot transfer dimensions for: ',
     &                                 this_var(:strmin(this_var))
      write(*,'(A,I5,//)') 'NetCDF error code: ',ierr
      goto 9999
c
 7002 continue
      write(*,'(//,A)') 'ERROR in NCF_XFER_GLOBAL: '
      write(*,'(A)') 'Reading input point source file.'
      write(*,'(A)') 'Parameter is not large enough: MXPTSRCE'
      write(*,'(A,I3,A,//)') 'Reset it to at least: ',nptsrce,
     &                                            ' and recompile.'
      goto 9999
c
 7003 continue
      write(*,'(//,A)') 'ERROR in NCF_XFER_GLOBAL: '
      write(*,'(A)') 'Reading input point source file.'
      write(*,'(A)') 'Parameter is not large enough: MXSPEC'
      write(*,'(A,I3,A,//)') 'Reset it to at least: ',nspecs,
     &                                            ' and recompile.'
      goto 9999
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
