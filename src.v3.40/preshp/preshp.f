C**** PRESHP.F 
c
      program preshp
c
c-----------------------------------------------------------------------
c
c    PRESHP is an entry module for EPS.  It reads 
c
c
c    Calling Sequence:
c
c      PRESHP
c       |----- init_hash_msgtot
c       |      
c       |------ opnprs    
c       |      
c       |------ rduprs    
c       |      |------ rdcrit    
c       |      |------ rddate    
c       |      |------ rdregn    
c       |      |------ rdcnty    
c       |      |------ rdotbl    
c       |      |------ getime    
c       |      
c       |------ hdrprs    
c       |      |------ getime    
c       |      |------ wrhdr     
c       |      
c       |------ prcprs    
c       |      |------ totprs    
c       |      |------ addtotcty 
c       |      |------ addtotasc 
c       |      |------ addtotfcd 
c       |      |------ addtothrs 
c       |      |------ addtothcd 
c       |      |------ maputg    
c       |      |------ mapgtu    
c       |      |------ wrrprs    
c       |      |      |------ totprs    
c       |      |      |------ addtotcty 
c       |      |      |------ addtotasc 
c       |      |      |------ addtotpid 
c       |      |      |------ addtotfcd 
c       |      |      |------ addtothrs 
c       |      |      |------ addtothcd 
c       |      |      |------ wrembr    
c       |      |
c       |      |------ setij     
c       |      |------ gline     
c       |      |------ mkverts   
c       |      |------ grarea    
c       |      |------ mkchannel 
c       |      |------ wreprs    
c       |      
c       |------ traprs    
c       |------ wtlprs    
c       |      |------ wrctbl8   
c       |      |------ wrtcty    
c       |      |------ wrtasc    
c       |      |------ wrtfac    
c       |      |------ wrtfcd    
c       |      |------ wrthrs    
c       |      |------ wrthcd    
c       |      
c       |------ clsprs    
c
c  addtotcty -- adds emissions to totals arrays by county (library)
c  addtotasc -- adds emissions to totals arrays by source category (library)
c  addtotpid -- adds emissions to totals arrays by plant/stack/point (library)
c  addtotfcd -- adds emissions to totals arrays by county/ASC pair (library)
c  addtothrs -- adds emissions to totals arrays by hour (library)
c  addtothcd -- adds emissions to totals arrays by hour and ASC code (library)
c  clsprs    -- closes all file units
c  getime    -- creates the current date and time string (library)
c  gline     -- computes cells corssed by a link (library)
c  grarea    -- grids a polygon, giving the area contained in each cell
c  hdrprs    -- creates the new EMBR header
c  init_hash_msgtot  -- initializes hash table indexes
c  mapgtu    -- converts geodesic coordinates to UTM coordinates (library)
c  mkverts   -- calculates the vertices of a shipping channel polygon
c  mkchannel -- calculates the vertices of a "cell" shipping channel
c  opnprs    -- reads filenames and opens files
c  prcprs    -- main processing routine
c  rdcrit    -- reads the /CRITERIA POLLUTANT/ packet of USERIN (library)
c  rddate    -- reads the /DATE/ packet of USERIN (library)
c  rdcnty    -- reads the /COUNTY/ packet of USERIN (library)
c  rdxref    -- reads the ASC-SCC/VOC cross reference file (library)
c  rdotbl    -- reads the /CRITERIA TABLES/ packet of USERIN (library)
c  rdregn    -- reads the /UAMREGN/ packet of USERIN (library)
c  rduprs    -- reads USERIN file
c  setij     -- finds grid cell containing a coordinate pair (library)
c  totprs    -- stores emissions in totals arrays
c  traprs    -- writes the EMBR trailer data
c  wrembr    -- writes one data record to EMBR file (library)
c  wrhdr     -- writes the header to the EMBR file
c  wreprs    -- writes record to error file
c  wrrprs    -- writes the data to EMBR file
c  wrtrpt    -- wriits the stack report file
c  wtlprs    -- writes the tables of processed emissions
c  wrctbl8   -- generic routine that writes a table using 8-bit reals (library)
c  wrtcty    -- writes a table of totals by county (library)
c  wrtasc    -- writes a table of totals by source category code (library)
c  wrtfac    -- writes a table of totals by plant ID (library)
c  wrtfcd    -- writes a table of totals by count and ASC pair (library)
c  wrthrs    -- writes a table of totals by hour (library)
c  wrthcd    -- writes a table of totals by hour and ASC (library)
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
      include 'iopreshp.inc'
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c     ierr  I   error flag for subroutine calls
c
      integer*4 ierr
c
c-----------------------------------------------------------------------
c    Entry Point:
c-----------------------------------------------------------------------
c
c  --- call routine to initialize the hash table indexes ---
c
      call init_hash_msgtot( )
c
c  --- call routine to get I/O filenames and open I/O files
c
      call opnprs(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to read user input file ---
c
      call rduprs(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to write the EMBR header ---
c
      call hdrprs(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to process each SHIPS record ---
c
      call prcprs(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to write trailer ---
c
      call traprs(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to write totals of tables ---
c
      call wtlprs(ierr)
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
      call clsprs( )
      if( ierr .NE. ISUCES ) then
          write(IOWSTD,'(//,3A)',ERR=9999) 'An error occurred ',
     &                                       'while running ',VERSON
          write(IOWSTD,'(2A,//)',ERR=9999) 'If no explanation is ',
     &               'given above, check the message file for details.'
      endif
      end
