C**** CNTLEM.F 
c
      program cntlem
c
c-----------------------------------------------------------------------
c
c  Description:
c       This module is responsible for all of the application of all
c    emissions controls.  All of the packets are read from the control
c    factors file and all appropriate data is tored in commom.  Each
c    inputs EMBR record is read and a search is made in each control
c    packet for a match of whatever identification information is
c    supplied in the packet.  If a match is found, the controls are
c    applied.  No effort is made to guard against the application of
c    multiple controls to a single source.  That is the responsibility
c    of the user.
c
c
c            CNTLEM
c              |----- init_hash_msgtot
c              |----- opnctl
c              |----- rductl
c              |         |----- rdcnty
c              |         |----- fndkey
c              |
c              |----- rdcfac
c              |         |----- rdsrc
c              |         |----- rdfips
c              |         |----- rdsbgr
c              |
c              |----- hdrctl
c              |         |----- getime
c              |         |----- rdhdr
c              |         |----- filhdr
c              |
c              |----- prcctl
c              |         |----- rdembr
c              |         |----- fnschr
c              |         |----- totctl
c              |         |----- fndxrf
c              |         |----- projec
c              |         |        |----- fndlcd
c              |         |
c              |         |        |----- fndpar
c              |         |
c              |         |----- wrembr
c              |----- tractl
c              |         |----- wrembr
c              |
c              |----- wtlctl
c              |         |----- wrctbl
c              |         |----- regtbl
c              |         |----- dsctbl
c              |
c              |----- clsctl
c
c   clsctl  -- closes all file units
c   dsctbl  -- writes tables of discretionary controls
c   filhdr  -- writes the new EMBR header (library)
c   fndchr  -- returns index of string in array of strings (library)
c   fndkey  -- searches a file for a keyword (library)
c   fndlcd  -- returns index of long FIPS and 5 digit code pair
c   fndlng  -- returns index of long FIPS code
c   fndsrc  -- returns index of long FIPS,plant,stack,point,SIC/SCC
c   fndpar  -- returns index of short FIPS and ASC code pair
c   fndrul  -- returns index of data in "rule" packet arrays
c   fndxrf  -- returns index of data in xref arrays (library)
c   getime  -- makes a system call to get date and time (library)
c   hdrctl  -- reads and writes the EMBR header
c   init_hash_msgtot  -- initializes hash table indexes
c   opnctl  -- reads input filenames and opens all files
c   prcctl  -- main processing routine
c   projec  -- applies future year projections
c   rdcfac  -- reads the control factors file
c   rdcnty  -- reads the /COUNTY/ packet of USERIN (library)
c   rdembr  -- reads one data record of input EMBR file (library)
c   rdfips  -- reads the /FIPS CODE/ packet
c   rdhdr   -- reads the existing EMBR header (library)
c   rdpfac  -- reads the projection packets
c   rdsrc   -- reads the /SOURCE CATEGORY/ packet
c   rdtral  -- reads the existing EMBR trailer (library)
c   rductl  -- reads the USERIN file
c   totctl  -- adds emssions to totals arrays
c   tractl  -- reads and writes the EMBR trailer
c   wrctbl  -- writes one table of processed emissions (library)
c   wremar  -- writes one record to EMAR error file (library)
c   wrembr  -- writes one data record to output EMBR file (library)
c   wtlctl  -- writes the output tables
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
      include 'iocntlem.inc'
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
      call opnctl(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to read user input file ---
c
      call rductl(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to read the EMBR header ---
c
      call hdrctl(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to read the control factors file ---
c
      call rdcfac(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to process the data ---
c
      call prcctl(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to write trailer ---
c
      call tractl(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to write totals of tables ---
c
      call wtlctl(ierr)
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
c
      call clsctl( )
      if( ierr .NE. ISUCES ) then
          write(IOWSTD,'(//,3A)',ERR=9999) 'An error occurred ',
     &                                       'while running ',VERSON
          write(IOWSTD,'(2A,//)',ERR=9999) 'If no explanation is ',
     &               'given above, check the message file for details.'
      endif
      end
