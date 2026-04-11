C**** TMPRL.F 
c
      program tmprl
c
c-----------------------------------------------------------------------
c
c    TMPRL performs the spatial allocation of emissions required for
c    an inventory to be used by the UAM.  An EMBR formatted file is read
c    and annual or seasonal emissions are resolved into hourly emissions
c    based on the temporal profiles determined by the source category
c    on the EMBR record.  A new EMBR file is wrritten with hourly
c    emissions.
c
c    Calling Sequence:
c
c            TMPRL
c              |
c              |----- init_hash_msgtot
c              |----- opntmp
c              |----- rdutmp
c              |         |----- rddate
c              |         |----- rdcnty
c              |
c              |----- rdtxrf
c              |         |----- settxf
c              |----- rdtprf
c              |----- hdrtmp
c              |         |----- rdhdr
c              |         |----- filhdr
c              |
c              |----- prctmp
c              |         |----- rdembr
c              |         |----- tottmp
c              |         |----- fndtxf
c              |         |----- wrembr
c              |         |----- wretmp
c              |
c              |----- tratmp
c              |         |----- rdtral
c              |
c              |----- wtltmp
c              |         |----- wtctmp
c              |
c              |----- clstmp
c
c      clstmp  --  closes all files
c      filhdr  --  writes the output EMBR header (library)
c      fndtxf  --  searches for the best match of data in XREF arrays
c      hdrtmp  --  reads and writes the EMBR header data
c      init_hash_msgtot  -- initializes hash table indexes
c      opntmp  --  reads the filenames and opens all files
c      prctmp  --  main processing routine; applies temporal adjustment
c                  to emissions read from EMBR record
c      rddate  --  reads the /DATE/ packet of USERIN (library)
c      rdcnty  --  reads the /COUNTY/ packet of USERIN (library)
c      rdembr  --  reads on EMBR data record (library)
c      rdhdr   --  reads the input EMBR header  (library)
c      rdtprf  --  reads the temporal profiles file
c      rdtral  --  reads and wirites the input EMBR trailer (library)
c      rdtxrf  --  reads the ASC/SCC cross reference file
c      rdutmp  --  reads the USERIN file storing data in common
c      settxf  --  searches for exact match of data in XREF arrays
c      tottmp  --  adds emissions to totals arrays
c      tratmp  --  writes the output EMBR trailer
c      wrctbl  --  writes a table with icharacter labels (library)
c      wrembr  --  writes one EMBR data record (library)
c      wretmp  --  writes error records to EMAR file
c      wtltmp  --  writes the tables of totals to message file
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
      include 'iotmprl.inc'
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
c
c  --- call routine to initialize the hash table indexes ---
c
      call init_hash_msgtot( )
c
c  --- call routine to get I/O filenames and open I/O files
c
      call opntmp(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to read user input file ---
c
      call rdutmp(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to read and write EMBR header data ---
c
      call hdrtmp(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to read ASC-SCC cross reference file ---
c
      call rdtxrf(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to read temporal profiles file ---
c
      call rdtprf(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to process EMBR records ---
c
      call prctmp(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to read and write new trailer ---
c
      call tratmp(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to write totals to message file ---
c
      call wtltmp(ierr)
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
c-----------------------------------------------------------------------
c   Exit Point:
c-----------------------------------------------------------------------
c
 9999 continue
      call clstmp( )
      if( ierr .NE. ISUCES ) then
          write(IOWSTD,'(//,3A)',ERR=9999) 'An error occurred ',
     &                                       'while running ',VERSON
          write(IOWSTD,'(2A,//)',ERR=9999) 'If no explanation is ',
     &               'given above, check the message file for details.'
      endif
      end
