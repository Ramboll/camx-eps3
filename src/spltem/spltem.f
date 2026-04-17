C**** SPLTEM.F
c
      program spltem
c
c-----------------------------------------------------------------------
c
c    SPLTEM reads a compressed, gridded, hourly, EMBR file.
c    SPLTEM produces an ascii output file of a user specified
c    subregion of hourly, gridded, speciated emissions. 
c    To do this, SPLTEM uses the SCC cross reference file.
c
c    CHMSPL compares the species list from the /EPISODE/ packet
c    of the user input file with the species list in the splits
c    file.  if a species is listed in the /EPISODE/ packet but
c    not the splits file, execution terminates.  in the reverse
c    situation, where the species exists in the splits file but
c    not the /EPISODE/ packet, the user can specify whether the
c    program continues or terminates.
c
c    Calling Sequence:
c
c            spltem
c               |---  opnspl
c               |
c               |---  hdrspl
c               |         |---  rdhdr
c               |
c               |---  rduspl
c               |         |---  rdregn
c               |
c               |---  rdxref
c               |
c               |---  rdcdb
c               |
c               |---  rdwfrc
c               |         |---  rdprof
c               |
c               |---  prcspl
c               |         |---  rdcomp
c               |         |---  totspl
c               |
c               |---  wrtspl
c               |         |---  wrtems
c               |
c               |---  wtlspl
c               |         |---  wrctbl
c               |
c               |---  clsspl
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c      08/07/97  --mmj--  original development
c      01/14/08  --mmj--  updated to support EPS3 file formats and
c                         USERIN packets
c
c-----------------------------------------------------------------------
c    Include Files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'iospltem.inc'
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c     ierr  i   error flag for subroutine calls
c
      integer*4 ierr
c
c-----------------------------------------------------------------------
c    Entry Point:
c-----------------------------------------------------------------------
c
c  --- call routine to get I/O filenames and open I/O files
c
      call opnspl(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to read header of EMBR file
c
      call hdrspl(ierr, 
     &                IORMBR)
      if ( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to read user input file ---
c
      call rduspl(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to read ASC-SCC cross reference file ---
c
      call rdxref(ierr,IORXRF)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to read the weight fractions file ---
c
      call rdwfrc(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to read the species/compound names from compound db ---
c
      call rdcdb(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to renormalize VOC_to_TOG profiles ---
c
      call cvtrea(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to process each EMBR record ---
c
      call prcspl(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to write output data files ---
c
      call wrtspl(ierr)
      if( ierr .NE. ISUCES ) goto 9999
c
c  --- call routine to write totals of tables ---
c
      call wtlspl(ierr)
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
c
c  --- call routine to close files ---
c
      call clsspl( )
      if( ierr .NE. ISUCES ) then
          write(IOWSTD,'(//,3A)',ERR=9999) 'An error occurred ',
     &                                       'while running ',VERSON
          write(IOWSTD,'(2A,//)',ERR=9999) 'If no explanation is ',
     &               'given above, check the message file for details.' 
      endif

      end
