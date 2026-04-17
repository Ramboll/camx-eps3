C**** PSTFIR.F
c
      program pstfir
c
c-----------------------------------------------------------------------
c
c    PSTFIR is the EPS module that outputs the point source fire
c    emissions.  It generates both low level surface emissions
c    as well as the elevated point source file.
c
c    The input EMBR file must already be processed by
c    PREFIR (load module), temporally allocated, and speciated.
c    The fire emissions loading module, PREFIR, reads the fire inventory 
c    and sets the EMBR file 'stack diameter' field to the fire class bin.  
c    This value is used in PSTFIR to determine the plume distribution.
c    Each input EMBR record is flagged as type fire "F".
c
c    Calling Sequence:
c
c            PSTFIR
c              |----- init_hash_msgtot
c              |----- opnpstfir
c              |----- rdhdr
c              |----- rdupstfir
c              |         |----- rdspec
c              |         |----- rddate
c              |         |----- rdregn
c              |         |----- rdcnty
c              |         |----- rdotbl
c              |         |----- rdbins
c              |         |----- fndkey
c              |
c              |----- hdrpstfir
c              |         |----- getime
c              |         |----- rdhdr
c              |         |----- rdregn
c              |
c              |----- clcvert
c              |----- prcpstfir
c              |         |----- rdembr
c              |         |----- totpstfir
c              |         |----- setij
c              |         |----- wrtelefir
c              |         |----- wrtuamfir
c              |         |----- wrepstfir
c              |
c              |----- wtlpstfir
c              |         |----- wrctbl
c              |
c              |----- clspstfir
c
c  clcvert   -- calculates vertical plume distribution fractions
c  clspstfir -- closes all file units
c  fndkey -- searches a file for a keyword (library)
c  hdrpstfir -- creates the new EMBR header
c  init_hash_msgtot  -- initializes hash table indexes
c  opnpstfir -- reads filenames and opens files
c  prcpstfir -- main processing routine for generating ascii points per layer
c  rdbins -- reads the /FIRE BINS/ packet of USERIN (library)
c  rdcnty -- reads the /COUNTY/ packet of USERIN (library)
c  rddate -- reads the /DATE/ packet of USERIN (library)
c  rdembr -- reads an EMBR record (library)
c  rdhdr  -- reads EMBR header records (library)
c  rdotbl -- reads the /CRITERIA TABLES/ and /SPECIES TABLES/ packets
c  rdregn -- reads the /UAMREGN/ packet of USERIN (library)
c  rdspec -- reads the /SPCEIES LIST/ packet of USERIN (library)
c  rdupstfir -- reads USERIN file
c  setij  -- finds the grid cell containing a coordinate pair (library)
c  totpstfir -- stores emissions in totals arrays
c  wrctbl -- writes one table of totals
c  wrepstfir -- writes record to error file
c  wrrfir -- writes the data to EMBR file
c  wrtelefir -- writes the elevated points data and header files
c  wrtuamfir -- writes the surface emissions file
c  wtlpstfir -- writes the tables of processed emissions
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
      include 'uamcntl.inc'
      include 'uamregn.inc'
      include 'embrdat.inc'
      include 'layers.inc'
      include 'iopstfir.inc'
      include 'pstfirtot.inc'
      include 'ncf_iodat.inc'
      include 'netcdf.inc'
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c     ierr  I   error flag for subroutine calls
c
      character*200 action
      character*20  spec_units(MXSPEC), spec_long_name(MXSPEC)
      character*60  spec_desc(MXSPEC), spec_coords(MXSPEC)
      integer*4     ierr, ihour
c
c-----------------------------------------------------------------------
c    Entry Point:
c-----------------------------------------------------------------------
c
c --- set compression flag ---
c
      call ncf_set_compress_flag()
c
c  --- call routine to initialize the hash table indexes ---
c
      call init_hash_msgtot( )
c
c  --- call routine to get I/O filenames and open I/O files
c
      call opnpstfir(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c   --- call routine to read the first part of the header,
c       then rewind the file ---
c
      curifl = imbrfl
      call rdhdr(ierr,
     &                 IORMBR )
      if( ierr .NE. ISUCES ) goto 9999
      rewind(IORMBR)
c
c  --- call routine to read user input file ---
c
      call rdupstfir(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to write the EMBR header ---
c
      call hdrpstfir(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c --- call routine to set the vertical plume distribution factors ---
c
      call clcvert(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- write header data to NetCDF file, if requested ---
c
      if( l3demiss ) then
         action = 'Writing output NetCDF file.'
         call grdprep(nxcell,nycell,cellon,cellat,xorig,yorig,dxcell,
     &                                          dycell,deltax,deltay)
         call ncf_set_vars_base()
         call ncf_set_tstep(ibgdat,begtim,iendat,endtim)
         call ncf_set_specatt_ems(spec_units,spec_long_name,
     &                                             spec_desc,spec_coords)
         call ncf_set_global('EMISSIONS ',1,ibgdat,begtim,
     &                                            iendat,endtim,nspecs)
         call ncf_wrt_dim(ierr,action,ncf_iowemis,nxcell,nycell,nlayer,nspecs)
         if( ierr .NE. ISUCES ) goto 9999
         call ncf_wrt_global(ierr,action,ncf_iowemis,nspecs,spcnam)
         if( ierr .NE. ISUCES ) goto 9999
         call ncf_wrt_vars_base(ierr,action,ncf_iowemis)
         if( ierr .NE. ISUCES ) goto 9999
         call ncf_wrt_vars_species(ierr,action,ncf_iowemis,nxcell,nycell,nspecs,
     &           spcnam,spec_units,spec_long_name,spec_desc,spec_coords)
         if( ierr .NE. ISUCES ) goto 9999
         ierr = nf_enddef(ncf_iowemis)
      endif
c
c  --- call routine to process each input data record ---
c
      call prcpstfir(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- write emissions data to NetCDF file, if requested ---
c
      if( l3demiss ) then
         call ncf_wrt_data_grid(ierr,action,ncf_iowemis,nxcell,nycell,
     &               xorig*1000.,yorig*1000.,dxcell*1000.,dycell*1000,
     &                                             nlayer,cellat,cellon)
         if( ierr .NE. ISUCES ) goto 9999
         do ihour=1,ncf_nsteps
c
c  --- write the data for this hour ---
c
            ncf_cur_tstep = ihour
            action = 'Writing timestamp to output NetCDF file.'
            call ncf_wrt_data_tstep(ierr,action,ncf_iowemis,nspecs)
            if( ierr .NE. ISUCES ) goto 9999
c
            action = 'Writing hourly data to output NetCDF file.'
            call ncf_wrt_data_species(ierr,action,ncf_iowemis,ihour,
     &                              nxcell,nycell,nlayer,nspecs,spcnam)
            if( ierr .NE. ISUCES ) goto 9999
         enddo
      endif
c
c  --- call routine to write totals of tables ---
c
      call wtlpstfir(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- looks successful, write message and reset flag ---
c
      write(IOWSTD,'(//,2A)',ERR=9999) 'Successful completion of ',
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
      ierr = ISUCES
      goto 9999
c
c
c-----------------------------------------------------------------------
c   Exit Point:
c-----------------------------------------------------------------------
c
c  --- call routine to close files ---
c
 9999 continue
      call clspstfir( )
      if( ierr .NE. ISUCES ) then
          write(IOWSTD,'(//,3A)',ERR=9999) 'An error occurred ',
     &                                       'while running ',VERSON
          write(IOWSTD,'(2A,//)',ERR=9999) 'If no explanation is ',
     &               'given above, check the message file for details.'
      endif
      end
