C**** PTS2EMS_NCF
c
      program pts2ems_ncf
c
c-----------------------------------------------------------------------
c
c    PTS2EMS_NCF will read a CAMx point source file, grid the sources 
c    and produce a gridded NetCDF File containing the emissions from
c    all sources. All sources are projected down to the surface layer.
c    The definition of the grid in the output file is contained in the
c    standard packets of the EPS3 USERIN file.
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Include Files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'uamregn.inc'
      include 'uamcntl.inc'
      include 'ncf_iodat.inc'
      include 'iopts2ems.inc'
      include 'pts2emstot.inc'
      include 'netcdf.inc'
c
c-----------------------------------------------------------------------
c    External functions: 
c-----------------------------------------------------------------------
c
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
      character*200 action
      character*60  var_text
      character*20  spec_units(MXSPEC), spec_long_name(MXSPEC)
      character*60  spec_desc(MXSPEC), spec_coords(MXSPEC)
      character*10  this_var
      character*4   name(10), inote(60), spcname_int(10,MXSPEC)
      integer*4     nxcell_in, nycell_in, nlayer, nseg
      integer*4     this_dimid, ierr, ispc, idum, i
      real*4        polelon_in, polelat_in, xorig_in, yorig_in
      real*4        dxcell_in, dycell_in, tlat1_in, tlat2_in
c
c-----------------------------------------------------------------------
c    Entry Point:
c-----------------------------------------------------------------------
c
c  --- call routine to read and open all of the files ---
c
      call opnpts2ems( ierr )
      if( ierr .NE. ISUCES ) goto 9999
c
c  ---- call routine to read the USERIN file (gets new grid definition)
c
      call rdupts2ems(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  ---- define, transfer and write header of NetCDF file  based on new grid ---
c
      action = 'Writing header to output NetCDF file.'
      if( lnetcdf_in ) then
         call grdprep(nxcell,nycell,cellon,cellat,xorig,yorig,dxcell,
     &                                          dycell,deltax,deltay)
         call ncf_set_vars_base()
         call ncf_xfer_global(ierr)
         if( ierr .NE. ISUCES ) goto 9999
         call ncf_set_specatt_ems(spec_units,spec_long_name,
     &                                             spec_desc,spec_coords)
         call ncf_wrt_dim(ierr,action,ncf_iowemis,nxcell,nycell,1,nspecs)
         if( ierr .NE. ISUCES ) goto 9999
         call ncf_wrt_vars_base(ierr,action,ncf_iowemis)
         if( ierr .NE. ISUCES ) goto 9999
         call ncf_wrt_vars_species(ierr,action,ncf_iowemis,nxcell,nycell,nspecs,
     &           spcnam,spec_units,spec_long_name,spec_desc,spec_coords)
         if( ierr .NE. ISUCES ) goto 9999
         ierr = nf_enddef(ncf_iowemis)
c
c  --- write the data for grid description variables ---
c
         call ncf_wrt_data_grid(ierr,action,ncf_iowemis,nxcell,nycell,
     &               xorig*1000.,yorig*1000.,dxcell*1000.,dycell*1000,
     &                                             1,cellat,cellon)
         if( ierr .NE. ISUCES ) goto 9999
c
c  --- write the data for grid description variables ---
c
         call ncf_xfer_data_tstep(ierr)
         if( ierr .NE. ISUCES ) goto 9999
c
c  ---- read header data from binary file and populate output file
c       metadata based on that ---
c
      else
c
         rewind(inpunit)
         read(inpunit,ERR=7000) name, inote, nseg, nspecs,
     &                   ibgdat, begtim, iendat, endtim
         write(fnote,'(60A1)') inote
         if( nspecs .GT. MXSPEC ) goto 7001
         ncf_nsteps = (iendat-ibgdat)*24 + INT(endtim-begtim)
         read(inpunit,ERR=7000) polelon_in, polelat_in, itzone,
     &            xorig_in, yorig_in, dxcell_in, dycell_in, nxcell_in, 
     &              nycell_in, nlayer, ncf_cproj,ncf_istag, tlat1_in, tlat2_in
         read(inpunit,ERR=7000)
         read(inpunit,ERR=7000) ((spcname_int(i,ispc),i=1,10),ispc=1,nspecs)
         do ispc=1,nspecs
           write(spcnam(ispc),'(10A1)') (spcname_int(i,ispc),i=1,10)
         enddo
         read(inpunit,ERR=7000) idum, nptsrce
c 
         call uam2ncf_global()
         call grdprep(nxcell,nycell,cellon,cellat,xorig,yorig,dxcell,
     &                                          dycell,deltax,deltay)
         call ncf_set_specatt_ems(spec_units,spec_long_name,
     &                                             spec_desc,spec_coords)
         call ncf_set_vars_base()
         call ncf_wrt_dim(ierr,action,ncf_iowemis,nxcell,nycell,1,nspecs)
         if( ierr .NE. ISUCES ) goto 9999
         call ncf_wrt_global(ierr,action,ncf_iowemis,nspecs)
         if( ierr .NE. ISUCES ) goto 9999
         this_var = 'IOAPI_VERSION'
         var_text = 'IOAPI-CAMx'
         ierr = nf_put_att_text(ncf_iowemis, NF_GLOBAL, this_var, strmin(var_text), var_text)
         this_var = 'EXEC_ID'
         var_text = 'PTST2EMS_NCF'
         ierr = nf_put_att_text(ncf_iowemis, NF_GLOBAL, this_var, strmin(var_text), var_text)
         this_var = 'HISTORY'
         ierr = nf_get_att_text(inpunit, NF_GLOBAL, this_var, var_text)
         var_text = var_text(:strmin(var_text))//' '//VERSON
         ierr = nf_put_att_text(ncf_iowemis, NF_GLOBAL, this_var, strmin(var_text), var_text)
         call ncf_wrt_vars_base(ierr,action,ncf_iowemis)
         if( ierr .NE. ISUCES ) goto 9999
         call ncf_wrt_vars_species(ierr,action,ncf_iowemis,nxcell,nycell,nspecs,
     &           spcnam,spec_units,spec_long_name,spec_desc,spec_coords)
         if( ierr .NE. ISUCES ) goto 9999
         ierr = nf_enddef(ncf_iowemis)
c
c  --- write the data for grid description variables ---
c
         call ncf_wrt_data_grid(ierr,action,ncf_iowemis,nxcell,nycell,
     &               xorig*1000.,yorig*1000.,dxcell*1000.,dycell*1000,
     &                                             1,cellat,cellon)
c
c  --- call routine to fille timestamp variables ---
c
         call ncf_set_tstep(ibgdat,begtim*100,iendat,endtim*100)
      endif
c
c  --- call routine to process each hour in the file ---
c
      call prcpts2ems(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to write totals to the output file ---
c
      call wtlpts2ems(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- all good ---
c
      write(IOWSTD,'(//,2A,//)',ERR=9999) 'Successful completion of ',
     &                                                       VERSON
      if( nwarn .EQ. 0 ) then
          write(IOWSTD,'(2A,//)',ERR=9999) 'There were no warning ',
     &                                             'messages written.'
      else if( nwarn .EQ. 1 ) then
          write(IOWSTD,'(2A,//)',ERR=9999) 'There was 1 warning ',
     &                                             'message written.'
      else
          write(IOWSTD,'(A,I2,A,//)',ERR=9999) 'There were ',nwarn,
     &                                   ' warning messages written.'
      endif
      goto 9999
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(*,'(//,A)') 'ERROR in PTS2EMS_NCF: '
      write(*,'(A)') 'Reading input point source file.'
      write(*,'(A,//)') 'Cannot read the header from input UAM binary ',
     &                                              'point source file.'
      goto 9999
c
 7001 continue
      write(*,'(//,A)') 'ERROR in PTS2EMS_NCF: '
      write(*,'(A)') 'Reading input point source file.'
      write(*,'(A)') 'Parameter is not large enough: MXSPEC'
      write(*,'(A,I3,A,//)') 'Reset it to at least: ',nspecs,
     &                                            ' and recompile.'
      goto 9999
c
c-----------------------------------------------------------------------
c   Exit Point:
c-----------------------------------------------------------------------
c
c  --- call routine to close files ---
c
 9999 continue
      ierr = nf_close(ncf_iowemis)
      end
