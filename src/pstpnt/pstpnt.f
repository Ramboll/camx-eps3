C***** PSTPNT.F 
c
      program pstpnt
c
c-----------------------------------------------------------------------
c
c    This module creates the ASCII input file to the UAM point source
c    preprocessor.  All of the stack information for the elevated
c    stacks is written to the output file and the emissions for each
c    hour is written.  In order to keep the memory requirements low,
c    two scratch files are created.  These files are written to and
c    read from as the program marches through the hours of the episode.
c
c    Calling Sequence:
c
c
c            PSTPNT
c              |----- init_hash_msgtot
c              |----- opnpst
c              |----- rdupst
c              |         |----- rddate
c              |         |----- rdregn
c              |         |----- rdcnty
c              |         |----- fndkey
c              |
c              |----- hdrpst
c              |         |----- rdhdr
c              |
c              |----- prcpst
c              |         |----- rdhdr
c              |         |----- rdembr
c              |         |----- totpst
c              |         |----- setloc
c              |         |----- wrrpst
c              |         |        |----- wrembr
c              |         |
c              |         |----- wrtems
c              |         |----- wrepst
c              |                  |----- wremar
c              |
c              |----- wtlpst
c              |----- clspst
c              |----- wtlabr
c              |         |----- wrctbl
c              |
c              |----- clsabr
c
c   clspst  -- closes all file units
c   fndkey  -- searches a file for a keyword (library)
c   hdrpst  -- reads the EMBR header and writes new header (EMBR or UAM)
c   init_hash_msgtot  -- initializes hash table indexes
c   opnpst  -- reads all filenames and opens all files
c   prcpst  -- main processing routine, reads and processes each EMBR record
c   rddate  -- reads the /DATE/ packet of USERIN (library)
c   rdcnty  -- reads the /COUNTY/ packet of USERIN (library)
c   rdembr  -- reads one record of EMBR data (library)
c   rdhdr   -- reads the EMBR header data (library)
c   rdregn  -- reads the /UAMREGN/ packet of USERIN (library)
c   rdupst  -- reads the USERIN file
c   setloc  -- finds the location of point in domain
c   totpst  -- adds emissions to totals arrays
c   wrctbl  -- writes one table of processed emissions (library)
c   wremar  -- writes one record of EMAR data (library)
c   wrtems  -- writes emissions data to output file
c   wrepst  -- writes the erred records to EMAR file
c   wrrpst  -- writes the emissions records to output file
c   wtlpst  -- writes tables of emissions totals
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
      include 'iopstpnt.inc'
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
      call opnpst(ierr)
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
      call rdupst(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to read the EMBR header ---
c
      call hdrpst(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to process the data ---
c
      call prcpst(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to write totals of tables ---
c
      call wtlpst(ierr)
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
c  --- call routine to close files ---
c
 9999 continue
      call clspst( )
      if( ierr .NE. ISUCES ) then
          write(IOWSTD,'(//,3A)',ERR=9999) 'An error occurred ',
     &                                       'while running ',VERSON
          write(IOWSTD,'(2A,//)',ERR=9999) 'If no explanation is ',
     &               'given above, check the message file for details.'
      endif
      end
