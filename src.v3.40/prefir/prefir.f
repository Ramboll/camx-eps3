C**** PREFIR.F
c
      program prefir
c
c-----------------------------------------------------------------------
c
c    PREFIR is an entry module for EPS.  It reads a fire point source
c    formatted file and converts it to an EMBR file.  The records
c    are screened so that only locations in the modeling region are
c    written to the EMBR file.  
c    Each EMBR record is flagged as type fire "F".
c
c    The PREFIR module reads fire emission data with x,y location and
c    virtual acreage of fire. Multiple points may optionally be combined 
c    in order to reduce the overall number of points to process.  The
c    combining can be done at a fine grid resolution, specified in the
c    module input packet.
c
c
c    Calling Sequence:
c
c            PREFIR
c              |----- init_hash_msgtot
c              |----- opnfir
c              |----- rdufir
c              |         |----- rdcrit
c              |         |----- rddate
c              |         |----- rdregn
c              |         |----- rdcnty
c              |         |----- rdotbl
c              |         |----- rdbins
c              |         |----- rdprfp
c              |         |        |----- fndkey
c              |
c              |----- hdrfir
c              |         |----- getime
c              |         |----- wrhdr
c              |
c              |----- prcfir
c              |         |----- totfir
c              |         |----- mapgtu
c              |         |----- setij
c              |         |----- getcell
c              |         |----- wrrfir
c              |         |        |----- wrembr
c              |         |----- wrefir
c              |
c              |----- trafir
c              |----- wtlfir
c              |         |----- wrctbl
c              |
c              |----- clsfir
c
c  clsfir -- closes all file units
c  fndkey -- searches a file for a keyword (library)
c  getcell -- sets grid cell based on finer resolution
c  getime -- creates the current date and time string (library)
c  hdrfir -- creates the new EMBR header
c  init_hash_msgtot  -- initializes hash table indexes
c  mapgtu -- converts geodesic coordinates to UTM coordinates (library)
c  opnfir -- reads filenames and opens files
c  prcfir -- main processing routine
c  rdbins -- reads optional /FIRE BINS/ packet (library)
c  rdcnty -- reads the /COUNTY/ packet of USERIN (library)
c  rdcrit -- reads the /CRITERIA POLLUTANT/ packet (library)
c  rddate -- reads the /DATE/ packet of USERIN (library)
c  rdotbl -- reads /CRITERIA TABLES/ & /SPECIES TABLES/ packets (library)
c  rdprfp -- reads USERIN file /PREFIR/ packet
c  rdregn -- reads the /UAMREGN/ packet of USERIN (library)
c  rdufir -- reads USERIN file
c  setij  -- finds the grid cell containing a coordinate pair (library)
c  totfir -- stores emissions in totals arrays
c  trafir -- writes the EMBR trailer data
c  wrctbl -- writes one table of totals
c  wrefir -- writes record to error file
c  wrembr -- writes one data record to EMBR file (library)
c  wrhdr  -- writes the header to the EMBR file
c  wrrfir -- writes the data to EMBR file
c  wtlfir -- writes the tables of processed emissions
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
      include 'ioprefir.inc'
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
c     --- call routine to get I/O filenames and open I/O files
c
      call opnfir(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c     --- call routine to read user input file ---
c
      call rdufir(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c     --- call routine to write the EMBR header ---
c
      call hdrfir(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c     --- call routine to process each input data record ---
c
      call prcfir(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c     --- call routine to write trailer ---
c
      call trafir(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c     --- call routine to write totals of tables ---
c
      call wtlfir(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c     --- looks successful, write message and reset flag ---
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
c     --- call routine to close files ---
c
 9999 continue
      call clsfir( )
      if( ierr .NE. ISUCES ) then
          write(IOWSTD,'(//,3A)',ERR=9999) 'An error occurred ',
     &                                       'while running ',VERSON
          write(IOWSTD,'(2A,//)',ERR=9999) 'If no explanation is ',
     &               'given above, check the message file for details.'
      endif
      end
