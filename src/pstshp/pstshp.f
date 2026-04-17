C**** PSTSHP.F
c
      program pstshp
c
c-----------------------------------------------------------------------
c
c    PSTSHP is the EPS module that outputs the point source shipping
c    emissions.  It generates both a low level surface emissions file
c    as well as the elevated point source files. With the option
c    to generate only a surface or only elevated files of all emissions.
c
c    The input EMBR file must already be processed by the
c    PRESHP (load module), temporally allocated, and speciated.
c    The shipping emissions loading module, PRESHP, reads the shipping
c    inventory and sets the EMBR file 'plant' variable to the vessel 
c    type. The emissions are vertically resolved using a data file
c    containing the heights of the bottom and the top of the plume
c    based on vessel type and source category code.
c
c    NOTE: This program has been extensively modified to support the
c          the CAMx plume rise distribution override feature.
c           June 2015
c
c    Calling Sequence:
c
c      PSTSHP
c       |      
c       |----- init_hash_msgtot
c       |------ opnpstshp 
c       |------ rdhdr     
c       |------ rdupstshp 
c       |      |------ rdspec    
c       |      |------ rddate    
c       |      |------ rdregn    
c       |      |------ rdcnty    
c       |      |------ rdotbl    
c       |      |------ getime    
c       |      |------ rdlayers  
c       |      
c       |------ rdvslmap  
c       |------ hdrpstshp 
c       |      |------ getime    
c       |      |------ rdhdr     
c       |      |------ rdregn    
c       |      
c       |------ prcpstshp 
c       |      |------ rdembr    
c       |      |------ totpstshp 
c       |      |      |------ addtotcty 
c       |      |      |------ addtotasc 
c       |      |      |------ addtotfcd 
c       |      |      |------ addtothrs 
c       |      |      |------ addtotpid 
c       |      |      |------ addtothcd 
c       |      |      
c       |      |------ wrteleshp 
c       |      |------ wrtuamshp 
c       |      |------ wrepstshp 
c       |
c       |------ wtlpstshp 
c       |      |------ wrctbl8   
c       |      |------ wrctbl    
c       |      |------ wrtcty    
c       |      |------ wrtasc    
c       |      |------ wrtfac    
c       |      |------ wrtstak   
c       |      |------ wrtpnt    
c       |      |------ wrtfcd    
c       |      |------ wrthrs    
c       |      |------ wrthcd    
c       |      
c       |------ clspstshp 
c
c  addtotcty -- adds emissions to totals arrays by county (library)
c  addtotasc -- adds emissions to totals arrays by source category (library)
c  addtotpid -- adds emissions to totals arrays by plant/stack/point (library)
c  addtotfcd -- adds emissions to totals arrays by county/ASC pair (library)
c  addtothrs -- adds emissions to totals arrays by hour (library)
c  addtothcd -- adds emissions to totals arrays by hour and ASC code (library)
c  clspstshp -- closes all file units
c  getime    -- creates the current date and time string (library)
c  hdrpstshp -- creates the new EMBR header
c  init_hash_msgtot  -- initializes hash table indexes
c  opnpstshp -- reads filenames and opens files
c  prcpstshp -- main processing routine
c  rdcnty    -- reads the /COUNTY/ packet of USERIN (library)
c  rddate    -- reads the /DATE/ packet of USERIN (library)
c  rdembr    -- reads an EMBR record (library)
c  rdhdr     -- reads EMBR header records (library)
c  rdotbl    -- reads the /CRITERIA TABLES/ and /SPECIES TABLES/ packets
c  rdregn    -- reads the /UAMREGN/ packet of USERIN (library)
c  rdspec    -- reads the /SPCEIES LIST/ packet of USERIN (library)
c  rdlayers  -- reads the /LAYERS/ packet of USERIN (library)
c  rdupstshp -- reads USERIN file
c  rdvslmap  -- reads the Vessel/Stack XREF file
c  totpstshp -- stores emissions in totals arrays
c  wrepstshp -- writes record to error file
c  wrrshp    -- writes the data to EMBR file
c  wrteleshp -- writes the elevated points data and header files
c  wrtuamshp -- writes the surface emissions file
c  wtlpstshp -- writes the tables of processed emissions
c  wrctbl8   -- generic routine that writes a table using 8-bit reals (library)
c  wrctbl    -- generic routine that writes a table using 4-bit reals (library)
c  wrtcty    -- writes a table of totals by county (library)
c  wrtasc    -- writes a table of totals by source category code (library)
c  wrtfac    -- writes a table of totals by plant/stack/point ID (library)
c  wrtstak   -- writes a table of totals by stack ID (library)
c  wrtpnt    -- writes a table of totals by point ID (library)
c  wrtfcd    -- writes a table of totals by count and ASC pair (library)
c  wrthrs    -- writes a table of totals by hour (library)
c  wrthcd    -- writes a table of totals by hour and ASC (library)
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c    june 2015 mjimenez Updated to support the plume rise
c                       distribution override feature of CAMx
c
c-----------------------------------------------------------------------
c    Include Files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'uamcntl.inc'
      include 'uamregn.inc'
      include 'layers.inc'
      include 'iopstshp.inc'
      include 'pstshptot.inc'
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
c  --- call routine to get I/O filenames and open I/O files ---
c
      call opnpstshp(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c   --- call routine to read the first part of the header,
c       then rewind the file ---
c
      call rdhdr(ierr,
     &                 IORMBR )
      if( ierr .NE. ISUCES ) goto 9999
      rewind(IORMBR)
c
c  --- call routine to read user input file ---
c
      call rdupstshp(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to read the Vessel/Stack XREF file ---
c
      call rdvslmap(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to read the EMBR header and 
c      write the UAM header ---
c
      call hdrpstshp(ierr)
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
      call prcpstshp(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to write totals of tables ---
c
      call wtlpstshp(ierr)
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
      call clspstshp( )
      if( ierr .NE. ISUCES ) then
          write(IOWSTD,'(//,3A)',ERR=9999) 'An error occurred ',
     &                                       'while running ',VERSON
          write(IOWSTD,'(2A,//)',ERR=9999) 'If no explanation is ',
     &               'given above, check the message file for details.'
      endif
      end
