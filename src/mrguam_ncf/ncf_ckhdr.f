c**** NCF_CKHDR
c
      subroutine ncf_ckhdr(ierr,iounit,ifile)
      implicit none
c
c----CAMx v7.20 220430
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c   This routine sets the global file attributes for the NetCDF file
c
c      Copyright 1996 - 2022
c     Ramboll
c      Argument description:
c       Inputs:
c         ierr    I  error flag
c         iounit  I  NCF file ID
c         ifile   I  index of file in array
c       Outputs:
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c     02/20/17   --gwilson--    Original development
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'uamregn.inc'
      include 'uamcntl.inc'
      include 'iomrguam.inc'
      include 'usrmrguam.inc'
      include 'netcdf.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer ierr
      integer iounit
      integer ifile
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
      integer strmin
      integer julcal
      integer fndchr
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*9000 varlist
      character*200  action
      character*10   this_var, proj_type, proj_type_in
      integer*4      itzon_in, itum_in, nxcell_in, nycell_in, nlays_in
      integer*4      jerr, iutm_in, this_dimid, ibgdat_in, ispc
      integer*4      data_start(3), data_count(3), nhours, i
      integer*4      tflag_in(2), etflag_in(2), this_varid, idareg
      integer*4      idatmp, iendat_in, izone_in, cproj_in, cproj
      real*4         xorig_in, yorig_in, dxcell_in, dycell_in
      real*4         begtim_in, endtim_in, timreg, rdiff, tlat1_in
      real*4         polelon_in, polelat_in, tlat2_in, timtmp
      real*8         dble_xcent, dble_ycent, dble_xorig, dble_yorig
      real*8         dble_xcell, dble_ycell, dble_p_alp, dble_p_bet
      logical*4      lregn, lepis, lgrid_size
c
c-----------------------------------------------------------------------
c    Parameters:
c-----------------------------------------------------------------------
c
      real FUZZ
      parameter( FUZZ = 0.01 )
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      action = 'Checking file header for NetCDF file: '//
     &                             innm(ifile)(:strmin(innm(ifile)))
c
c  --- get global variables for domain definition ----
c
      this_var = 'ITZON'
      jerr = nf_get_att_int(iounit, NF_GLOBAL, 'ITZON', itzon_in)
      if( jerr .NE. NF_NOERR ) goto 7000
c
      this_var = 'XCENT'
      jerr = nf_get_att_double(iounit, NF_GLOBAL, 'XCENT', dble_xcent)
      if( jerr .NE. NF_NOERR ) goto 7000
      polelon_in = REAL(dble_xcent)
c
      this_var = 'YCENT'
      jerr = nf_get_att_double(iounit, NF_GLOBAL, 'YCENT', dble_ycent)
      if( jerr .NE. NF_NOERR ) goto 7000
      polelat_in = REAL(dble_ycent)
c
      this_var = 'IUTM'
      jerr = nf_get_att_int(iounit, NF_GLOBAL, 'IUTM', iutm_in)
      if( jerr .NE. NF_NOERR ) goto 7000
c
      this_var = 'XORIG'
      jerr = nf_get_att_double(iounit, NF_GLOBAL, 'XORIG', dble_xorig)
      if( jerr .NE. NF_NOERR ) goto 7000
      xorig_in = REAL(dble_xorig)/1000.
c
      this_var = 'YORIG'
      jerr = nf_get_att_double(iounit, NF_GLOBAL, 'YORIG', dble_yorig)
      if( jerr .NE. NF_NOERR ) goto 7000
      yorig_in = REAL(dble_yorig)/1000.
c
      this_var = 'XCELL'
      jerr = nf_get_att_double(iounit, NF_GLOBAL, 'XCELL', dble_xcell)
      if( jerr .NE. NF_NOERR ) goto 7000
      dxcell_in = REAL(dble_xcell)/1000.
c
      this_var = 'YCELL'
      jerr = nf_get_att_double(iounit, NF_GLOBAL, 'YCELL', dble_ycell)
      if( jerr .NE. NF_NOERR ) goto 7000
      dycell_in = REAL(dble_ycell)/1000.
c
      this_var = 'COL'
      jerr = nf_inq_dimid(iounit, "COL", this_dimid  )
      if( jerr .NE. NF_NOERR ) goto 7001
      jerr = nf_inq_dimlen(iounit,this_dimid,nxcell_in)
      if( jerr .NE. NF_NOERR ) goto 7001
c
      this_var = 'ROW'
      jerr = nf_inq_dimid(iounit, "ROW", this_dimid  )
      if( jerr .NE. NF_NOERR ) goto 7001
      jerr = nf_inq_dimlen(iounit,this_dimid,nycell_in)
      if( jerr .NE. NF_NOERR ) goto 7001
c
      this_var = 'LAY'
      jerr = nf_inq_dimid(iounit, "LAY", this_dimid  )
      if( jerr .NE. NF_NOERR ) goto 7001
      jerr = nf_inq_dimlen(iounit,this_dimid,nlays_in)
      if( jerr .NE. NF_NOERR ) goto 7001
c
      this_var = 'P_ALP'
      jerr = nf_get_att_double(iounit, NF_GLOBAL, 'P_ALP', dble_p_alp)
      if( jerr .NE. NF_NOERR ) goto 7000
      tlat1_in = REAL(dble_p_alp)
c
      this_var = 'P_BET'
      jerr = nf_get_att_double(iounit, NF_GLOBAL, 'P_BET', dble_p_bet)
      if( jerr .NE. NF_NOERR ) goto 7000
      tlat2_in = REAL(dble_p_bet)
c
      this_var = 'CPROJ'
      jerr = nf_get_att_int(iounit, NF_GLOBAL, 'CPROJ', cproj_in)
      if( jerr .NE. NF_NOERR ) goto 7000
      if( cproj .EQ. 0 ) then
         proj_type_in = "Lat/Lon"
      else if( cproj .EQ. 1 ) then
         proj_type_in = "UTM"
      else if( cproj .EQ. 2 ) then
         proj_type_in = "Lambert"
      else if( cproj .EQ. 3 ) then
         proj_type_in = "RPolar"
      else if( cproj .EQ. 4 ) then
         proj_type_in = "Polar"
      else if( cproj .EQ. 5 ) then
         proj_type_in = "Mercator"
      endif
c
c   --- get beg/end date/time for this file ---
c
c
      this_var = 'NSTEPS'
      jerr = nf_get_att_int(iounit, NF_GLOBAL, 'NSTEPS', nhours)
      if( jerr .NE. NF_NOERR ) goto 7000
      data_start(1) = 1
      data_count(1) = 2
      data_start(2) = 1
      data_count(2) = 1
      data_count(3) = 1
      data_start(3) = 1
      this_var = 'TFLAG'
      jerr = nf_inq_varid(iounit,this_var,this_varid)
      if( jerr .NE. NF_NOERR ) goto 7002
      jerr = nf_get_vara_int(iounit,this_varid,data_start,data_count,tflag_in)
      if( jerr .NE. NF_NOERR ) goto 7003
      ibgdat_in = MOD(tflag_in(1),1000000)
      begtim_in = REAL(tflag_in(2))/10000.
      data_start(3) = nhours
      this_var = 'ETFLAG'
      jerr = nf_inq_varid(iounit,this_var,this_varid)
      if( jerr .NE. NF_NOERR ) goto 7002
      jerr = nf_get_vara_int(iounit,this_varid,data_start,data_count,etflag_in)
      if( jerr .NE. NF_NOERR ) goto 7003
      iendat_in = MOD(etflag_in(1),1000000)
      endtim_in = REAL(etflag_in(2))/10000.
c
c   --- stop if domain definition does not match ---
c
      lregn = .TRUE.
      lepis = .TRUE.
      lgrid_size = .TRUE.
      if( ABS(xorig_in-xorig) .GT. FUZZ ) lregn = .FALSE.
      if( ABS(yorig_in-yorig) .GT. FUZZ ) lregn = .FALSE.
      if( ABS(dxcell_in-dxcell) .GT. FUZZ ) lregn = .FALSE.
      if( ABS(dxcell_in-dycell) .GT. FUZZ ) lregn = .FALSE.
      if( nxcell_in .NE. nxcell ) lregn = .FALSE.
      if( nxcell_in .NE. nxcell ) lregn = .FALSE.
      if( nlays_in .NE. 1) lregn = .FALSE.
      if( nycell_in .NE. nycell ) lgrid_size = .FALSE.
      if( nycell_in .NE. nycell ) lgrid_size = .FALSE.
      if( nlays_in .NE. 1) lgrid_size = .FALSE.
c
c   --- stop if projection type does not match ---
c
      if( llatlon ) then
         cproj = 0
         proj_type = "Lat/Lon"
         xorig_in = xorig_in * 1000.
         yorig_in = xorig_in * 1000.
         dxcell_in = dxcell_in * 1000.
         dycell_in = dycell_in * 1000.
      else if( lutm ) then
         cproj = 1
         proj_type = "UTM"
      else if( lambrt ) then
         cproj = 2
         proj_type = "Lambert"
      else if( lrpolar ) then
         cproj = 3
         proj_type = "RPolar"
      else if( lpolar ) then
         cproj = 4
         proj_type = "Polar"
      else if( lmerc ) then
         cproj = 5
         proj_type = "Mercator"
      endif
      if( cproj_in .NE. cproj ) lregn = .FALSE.
c
c   --- compare time, if beginning hour is 24,
c       change day and set date to next day ---
c
      idatmp = ibgdat_in
      timtmp = begtim_in
      if( timtmp .EQ. 24. ) then
          idatmp = idatmp + 1
          timtmp = 0.
      endif
c
      idareg = ibgdat
      timreg = begtim/100
      if( timreg .EQ. 24. ) then
          idareg = idareg + 1
          timreg = 0.
      endif
      if( idatmp .NE. idareg ) lepis = .FALSE.
      if( ABS(timtmp - timreg) .GT. FUZZ) lepis = .FALSE.
c
c   --- if ending hour is 0, change day and set date to previous
c       day ---
c
      idatmp = iendat_in
      timtmp = endtim_in
      if( timtmp .EQ. 0. ) then
          idatmp = idatmp - 1
          timtmp = 24.
      endif
c
      idareg = iendat
      timreg = endtim/100
      if( timreg .EQ. 0. ) then
          idareg = idareg - 1
          timreg = 24.
      endif
c
      if( idatmp .NE. idareg ) lepis = .FALSE.
      if( ABS(timtmp - timreg) .GT. FUZZ) lepis = .FALSE.
      if(idatmp .NE. idareg) lepis = .FALSE.
      rdiff = ABS(timtmp - timreg)
      if(rdiff .GT. FUZZ) lepis = .FALSE.
c
      if( itzon_in .NE. itzone ) lepis = .FALSE.
      if(  lutm ) then
         if( iutm_in .NE. izone ) lregn = .FALSE.
      else
         if( ABS(polelon_in-polelon) .GT. FUZZ ) lregn = .FALSE.
         if( ABS(polelat_in-polelat) .GT. FUZZ ) lregn = .FALSE.
         if( ABS(tlat1_in-tlat1) .GT. FUZZ ) lregn = .FALSE.
         if( ABS(tlat2_in-tlat2) .GT. FUZZ 
     &                       .AND. .NOT. lpolar ) lregn = .FALSE.
c
c  --- order of the poles doesn't matter ---
c
         if( .NOT. lregn .AND. .NOT. lpolar ) then
            if( ABS(tlat2_in-tlat2) .GT. FUZZ ) lregn = .FALSE.
            if( ABS(tlat1_in-tlat1) .GT. FUZZ ) lregn = .FALSE.
         endif
      endif
c
c  --- numberf of grid cells mismatchis automatic error message ---
c
      if( .NOT. lgrid_size ) then
         write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Size of grid in ',
     &             'input file is not consistent with /REGION/ packet.'
         write(ioerr,'(2A)',ERR=9999) 'File: ',
     &                               innm(ifile)(:strmin(innm(ifile)))
c
         write(ioerr,'(A)',ERR=9999) 'Input EMISSIONS header:'
         write(ioerr,9002,ERR=9999) 'Number of cells',nxcell_in,nycell_in
         write(ioerr,9001) 'Number of layers: ',nlays_in
c
         write(ioerr,'(/,A)',ERR=9999) '/REGION/ header:'
         write(ioerr,9002,ERR=9999) 'Number of cells',nxcell,nycell
         write(ioerr,9001) 'Number of layers: ',1
         write(ioerr,'(//)') 
         ierr = IFAIL
         goto 9999
      endif
c
c  --- report inconsistenancy ---
c
      if( .NOT. lregn ) then
         if( lchkhd ) then
            write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Header read from ',
     &             'input file is not consistent with /REGION/ packet.'
            write(ioerr,'(2A)',ERR=9999) 'File: ',
     &                               innm(ifile)(:strmin(innm(ifile)))
         else
            write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
            write(ioerr,'(2A)',ERR=9999) 'WARNING: Header read ',
     &        'from input file is not consistent with /REGION/ packet.'
            write(ioerr,'(2A)',ERR=9999) 'File: ',
     &                               innm(ifile)(:strmin(innm(ifile)))
            nwarn = nwarn + 1
         endif
c
         write(ioerr,'(A)',ERR=9999) 'Input EMISSIONS header:'
         write(ioerr,9001) 'Projection type: ',cproj
         if( lutm ) write(ioerr,9001,ERR=9999) 'UTM zone',izone_in
         write(ioerr,9000,ERR=9999) 'Grid origin (km)',xorig_in,yorig_in
         write(ioerr,9000,ERR=9999) 'Grid cell width (km)',
     &                                               dxcell_in,dycell_in
         write(ioerr,9002,ERR=9999) 'Number of cells',nxcell_in,nycell_in
         write(ioerr,9001) 'Number of layers: ',nlays_in
         write(ioerr,9004) 'Longitude of projection pole:',polelon_in
         write(ioerr,9004) 'Latitude of projection pole:',polelat_in
         write(ioerr,9004) '1st true latitude:',tlat1_in
         if( .NOT. lpolar ) write(ioerr,9004) 
     &                                '2nd true latitude:',tlat2_in
c
         write(ioerr,'(/,A)',ERR=9999) '/REGION/ header:'
         write(ioerr,9001) 'Projection type: ',cproj
         if( lutm ) write(ioerr,9001,ERR=9999) 'UTM zone',izone
         write(ioerr,9000,ERR=9999) 'Grid origin (km)',xorig,yorig
         write(ioerr,9000,ERR=9999) 'Grid cell width (km)',
     &                                                 dxcell,dycell
         write(ioerr,9002,ERR=9999) 'Number of cells',nxcell,nycell
         write(ioerr,9001) 'Number of layers: ',1
         write(ioerr,9004) 'Longitude of projection pole:',polelon
         write(ioerr,9004) 'Latitude of projection pole:',polelat
         write(ioerr,9004) '1st true latitude:',tlat1
         if( .NOT. lpolar ) write(ioerr,9004) 
     &                                '2nd true latitude:',tlat2
c
         if( lchkhd ) then
           ierr = IFAIL
           goto 9999
         else
           write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
         endif
      endif
c
      if( .NOT. lepis ) then
         if( lchkhd ) then
             write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Header read from ',
     &             'input file is not consistent with /DATE/ packet.'
            write(ioerr,'(2A)',ERR=9999) 'File: ',
     &                               innm(ifile)(:strmin(innm(ifile)))
         else
            write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
            write(ioerr,'(2A)',ERR=9999) 'WARNING: Header read from ',
     &             'input file is not consistent with /DATE/ packet.'
            write(ioerr,'(2A)',ERR=9999) 'File: ',
     &                               innm(ifile)(:strmin(innm(ifile)))
            nwarn = nwarn + 1
         endif

         write(ioerr,'(A)',ERR=9999) 'Input EMISSIONS header:'
         write(ioerr,9003,ERR=9999) 'File note',fnote(:strmin(fnote))
         write(ioerr,9005,ERR=9999) 'Beginning date',ibgdat_in
         write(ioerr,9004,ERR=9999) 'Beginning time',begtim_in
         write(ioerr,9005,ERR=9999) 'Ending date',iendat_in
         write(ioerr,9004,ERR=9999) 'Ending time',endtim_in
         write(ioerr,9001,ERR=9999) 'Model Time zone',itzone
c
         write(ioerr,'(/A)',ERR=9999) '/DATE/ header:'
         write(ioerr,9005,ERR=9999) 'Beginning date (Calander)',ibgcal
         write(ioerr,9001,ERR=9999) 'Beginning date (Julian)',ibgdat
         write(ioerr,9004,ERR=9999) 'Beginning time',begtim/100
         write(ioerr,9005,ERR=9999) 'Ending date (Calander)',iencal
         write(ioerr,9001,ERR=9999) 'Ending date (Julian)',iendat
         write(ioerr,9004,ERR=9999) 'Ending time',endtim/100
         write(ioerr,9001,ERR=9999) 'Model Time zone',itzon_in
c
         if( lchkhd ) then
           ierr = IFAIL
           goto 9999
         else
           write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
         endif
      endif
c
c ---- setup table of indexes into global arrays ---
c 
      this_var = 'NVARS'
      jerr = nf_get_att_int(iounit, NF_GLOBAL, 'NVARS', iscnt(ifile))
      if( jerr .NE. NF_NOERR ) goto 7000
      this_var = 'VAR-LIST'
      jerr = nf_get_att_text(iounit, NF_GLOBAL, 'VAR-LIST', varlist)
      if( jerr .NE. NF_NOERR ) goto 7000
      do ispc=1,iscnt(ifile)
        table(ifile,ispc) = varlist(1+(ispc-1)*16:16+(ispc-1)*16)
        idxspc(ifile,ispc) =
     &           fndchr( table(ifile,ispc), 10, spcnam, nspecs )
      enddo
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c    Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(//,a)') 'ERROR in NCF_CKHDR:'
      write(ioerr,'(A)') action(:strmin(action))
      write(ioerr,'(2A)') 'Cannot find global attribute: ',
     &                                      this_var(:strmin(this_var))
      write(ioerr,'(A,I5)') 'NetCDF error code: ',jerr
      ierr = IFAIL
      goto 9999
c
 7001 continue
      write(ioerr,'(//,a)') 'ERROR in NCF_CKHDR:'
      write(ioerr,'(A)') action(:strmin(action))
      write(ioerr,'(2A)') 'Cannot find the dimension id for dimension: ',
     &                            this_var(:strmin(this_var))
      write(ioerr,'(A,I5)') 'NetCDF error code: ',jerr
      ierr = IFAIL
      goto 9999
c
 7002 continue
      write(ioerr,'(//,a)') 'ERROR in NCF_CKHDR:'
      write(ioerr,'(A)') action(:strmin(action))
      write(ioerr,'(2A)') 'Cannot find the variable id for variable: ',
     &                            this_var(:strmin(this_var))
      write(ioerr,'(A,I5)') 'NetCDF error code: ',jerr
      ierr = IFAIL
      goto 9999
c
 7003 continue
      write(ioerr,'(//,a)') 'ERROR in NCF_CKHDR:'
      write(ioerr,'(A)') action(:strmin(action))
      write(ioerr,'(2A)') 'Cannot read data for variable: ',
     &                            this_var(:strmin(this_var))
      write(ioerr,'(A,I5)') 'NetCDF error code: ,jerr
      ierr = IFAIL
      goto 9999
c
c-----------------------------------------------------------------------
c    Format statements:
c-----------------------------------------------------------------------
c
 9000 format(T10,A,T40,:,': (',F10.3,',',F10.3,')')
 9001 format(T10,A,T40,:,':',I5)
 9002 format(T10,A,T40,:,': (',I5,',',I5,')')
 9003 format(T10,A,T40,:,':',A)
 9004 format(T10,A,T40,:,':',F7.2)
 9005 format(T10,A,T40,:,':',I6)
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
