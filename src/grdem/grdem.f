C***** GRDEM.F 
c
      program grdem
c
c-----------------------------------------------------------------------
c
c    This module is responsible for assigning grid cells to each source
c    in an EMBR file.  For point sources, this means calculating the
c    grid cell based on the location of the point. For area and motor
c    vehicle sources, the cells assigned to a source are calculated
c    via a surrogate cross reference scheme based on source category.
c    One of two output emissions files is produced:  an EMBR file or
c    a UAM emissions file. 
c
c    Calling Sequence:
c
c            GRDEM
c              |----- init_hash_msgtot
c              |----- opngrd
c              |----- rdugrd
c              |         |----- rddate
c              |         |----- rdregn
c              |         |----- rdcnty
c              |         |----- fndkey
c              |
c              |----- rdgxrf
c              |----- rdgsrg
c              |         |----- maputg
c              |         |----- mapgtu
c              |         |----- setij
c              |
c              |----- rdglnk
c              |         |----- maputg
c              |         |----- mapgtu
c              |         |----- gline
c              |
c              |----- hdrgrd
c              |         |----- getime
c              |         |----- rdhdr
c              |         |----- filhdr
c              |
c              |----- prcgrd
c              |         |----- rdembr
c              |         |----- wrrgrd
c              |         |        |----- wrembr
c              |         |
c              |         |----- wrembr
c              |         |----- totgrd
c              |         |----- prcar
c              |         |        |----- wrembr
c              |         |        |----- totgrd
c              |         |
c              |         |----- prclnk
c              |         |        |----- wrembr
c              |         |        |----- totgrd
c              |         |        |----- setij
c              |         |
c              |         |----- prcpt
c              |         |        |----- totgrd
c              |         |        |----- wrembr
c              |         |
c              |         |----- wrtuam
c              |         |----- wregrd
c              |                  |----- wremar
c              |
c              |----- tragrd
c              |         |----- rdtral
c              |
c              |----- wtlgrd
c              |         |----- wrctbl
c              |
c              |----- clsgrd
c
c   clsgrd  -- closes all file units
c   filhdr  -- writes the new EMBR header
c   fndkey  -- searches a file for a keyword (library)
c   getime  -- gets the current date and time string (library)
c   gline   -- finds the grid cells crossed by a link (library)
c   init_hash_msgtot  -- initializes hash table indexes
c   mapgtu  -- converts geodesic coordinates to UTM coordinates (library)
c   maputg  -- converts UTM COordinates to geodesic coordinates (library)
c   opngrd  -- reads all filename and opens all files
c   prcar   -- processes an area source record
c   prcgrd  -- main processing routine
c   prclnk  -- processes a motor vehicle source record
c   prcpt   -- processes a point source record
c   rddate  -- reads the /DATE/ packet (library)
c   rdcnty  -- reads the /COUNTY/ packet (library)
c   rdembr  -- reads one data record of the EMBR file
c   rdglnk  -- reads the griddded link data
c   rdgsrg  -- reads the gridded surrogates file
c   rdgxrf  -- reads the surrogate cross reference file
c   rdhdr   -- reads the existing input EMBR header (library)
c   rdregn  -- reads the /UAMREGN/ packet (library)
c   rdtral  -- reads and writes the existing EMBR trailer (library)
c   rdugrd  -- reads USERIN file
c   setij   -- finds grid cell containing a coordinate pair (library)
c   totgrd  -- adds current emissions to totals arrays
c   tragrd  -- writes the new EMBR trailer
c   wrctbl  -- writes one table of totals
c   wremar  -- writes one date record of EMAR file (library)
c   wrembr  -- writes one data record to the EMBR file
c   wregrd  -- writes the EMAR error file
c   wrrgrd  -- writes the EMBR records and stores totals
c   wrtuam  -- writes one hour of emissions to UAM file
c   wtlgrd  -- writes the tables of processed emissions
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
      include 'embrdat.inc'
      include 'iogrdem.inc'
      include 'usrgrdem.inc'
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
      call opngrd(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c   --- call routine to read the first part of the header,
c       then rewind the file ---
c
      curofl = oemsfl
      curifl = imbrfl
      call rdhdr(ierr,
     &                 IORMBR )
      if( ierr .NE. ISUCES ) goto 9999
      rewind(IORMBR)
c
c  --- call routine to read user input file ---
c
      call rdugrd(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- if surrogate data is available, call routine to read ASC-SCC
c      cross reference file and call routine to read gridded
c      surrogate data ---
c
      if( lsurg ) then
         call rdgxrf(ierr)
         if( ierr .NE. ISUCES ) goto 9999
c
         call rdgsrg(ierr)
         if( ierr .NE. ISUCES ) goto 9999
      endif
c
c  --- if link data is available, call routine to read link data ---
c
      if( lnkdef ) then
         call rdglnk(ierr)
         if( ierr .NE. ISUCES ) goto 9999
      endif
c
c  --- call routine to read the EMBR header ---
c
      call hdrgrd(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- if doing NetCDF output call routine to write calculstr and write
c      NetCDF metadata ---
c
      if( lnetcdf_out ) then
         call ncf_grd_wrthdr( ierr )
         if( ierr .NE. ISUCES ) goto 9999
      endif
c
c  --- call routine to process the data ---
c
      call prcgrd(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to write trailer ---
c
      if( loutfile .AND. (.NOT. luam .AND. .NOT. lmeds .AND. 
     &                                    .NOT. lcomp) ) then
         call tragrd(ierr)
         if( ierr .NE. ISUCES ) goto 9999
      endif
c
c  --- call routine to write totals of tables ---
c
      call wtlgrd(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to write FIPS/ASC totals ---
c
      if( lascfl ) then
         call wrfasc(ierr)
         if( ierr .NE. ISUCES ) goto 9999
      endif
c
c  --- call routine to write surrogate totals report ---
c
      if( lsrpt ) then
         call wrtsrg(ierr)
         if( ierr .NE. ISUCES ) goto 9999
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
c-----------------------------------------------------------------------
c   Exit Point:
c-----------------------------------------------------------------------
c
c
c  --- call routine to close files ---
c
 9999 continue
      call clsgrd( )
      if( ierr .NE. ISUCES ) then
          write(IOWSTD,'(//,3A)',ERR=9999) 'An error occurred ',
     &                                       'while running ',VERSON
          write(IOWSTD,'(2A)',ERR=9999) 'If no explanation is ',
     &               'given above, check the message file for details.'
      endif
      end
