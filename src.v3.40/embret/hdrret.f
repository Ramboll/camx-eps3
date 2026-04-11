C**** HDRRET.F 
c
      subroutine hdrret( ierr )
c
c-----------------------------------------------------------------------
c
c    Sets the header variables and writes the EMAR header for the
c    EMBRET program
c
c    Argument Declaration.
c     Outputs:
c       ierr   I error flag
c     Inputs:
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
      include 'msgtot.inc'
      include 'ioembret.inc'
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Argument Declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
c
c-----------------------------------------------------------------------
c    Local Parameters:
c-----------------------------------------------------------------------
c
c   KEYDAT   C   parameter for /DATA/ keyword
c
      character*(KEYLEN) KEYDAT
c
      parameter(KEYDAT = '/DATA/')
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c   cdate   C   string for date and time
c   string  C   character string for temporary storage
c
      character*(MXSTR)  action
      character*(LENREC) string
      character*40       cdate
      integer*4          irec, i
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      irec = 1
      curifl = embrfl
      curofl = emarfl
c
c   --- read/write the file type ---
c
      action = 'Reading the file type.'
      read(IORMBR,ERR=7000,END=7001) filtyp
      action = 'Writing the file type.'
      write(IOWEMR,'(2A)',ERR=7002) 'File type: ',
     &                                     filtyp(:strmin(filtyp))
c
c   --- read the region definition ----
c
      irec = irec + 1
      action = 'Reading the region definition.'
      read(IORMBR,ERR=7000,END=7001 ) string
      read(string,'(2F10.0,3I10,2F10.0)',ERR=7000) rgutme, rgutmn,
     &                              irgzn, nxrgn, nyrgn, dxrgn, dyrgn
      action = 'Writing the region definition.'
      write(IOWEMR,'(A,2(F10.2,1X))',ERR=7000) 'Domain Origin: ',
     &                                                  rgutme, rgutmn
      write(IOWEMR,'(A,I10)',ERR=7000) 'UTM Zone: ',irgzn
      write(IOWEMR,'(A,2I10)',ERR=7000) 'Number of cells: ',nxrgn, nyrgn
      write(IOWEMR,'(A,2(F10.2,1X))',ERR=7000)'Cell Width: ',dxrgn,dyrgn
c
c   --- write the number of pollutants and species ---
c
      irec = irec + 1
      action = 'Reading the number of species.'
      read(IORMBR,ERR=7000,END=7001 ) string
      read(string,'(2I10)',ERR=7000) ncrthd, nspchd
      if( ncrthd .LE. 0 ) goto 7003
      if( ncrthd .GT. MXCRIT ) goto 7004
      if( nspchd .GT. MXSPEC ) goto 7005
      action = 'Writing the number of species.'
      write(IOWEMR,'(A,I10)',ERR=7002) 'Number of criteria: ',ncrthd
      write(IOWEMR,'(A,I10)',ERR=7002) 'Number of speciated: ',nspchd
      action = 'Reading the names of the criteria pollutants.'
      irec = irec + 1
      read(IORMBR,ERR=7000,END=7001 ) string
      read(string,'(100(:,A10,I10))',ERR=7000) (crtnam(i),
     &                                         isarod(i),i=1,ncrthd)
      action = 'Writing the names of the criteria pollutants.'
      write(IOWEMR,'(100(:,A10,1X,I10,/))',ERR=7002) (crtnam(i),
     &                                         isarod(i),i=1,ncrthd)
c
c   --- read CBIV species list and molecular weights in two lines ---
c
      if( nspchd .GT. 0 ) then
         irec = irec + 1
         action = 'Reading the names of the speciated pollutants.'
         read(IORMBR,ERR=7000,END=7001 ) (spclst(i),molwt(i),
     &                                      spcrit(i),i=1,nspchd)
         action = 'Writing the names of the speciated pollutants.'
         write(IOWEMR,9001,ERR=7002 ) (spclst(i),molwt(i),
     &                                      spcrit(i),i=1,nspchd)
c
c  --- don't try to do species tables ---
c
      else
         lspccty(IDXFMT) = .FALSE.
         lspcasc(IDXFMT) = .FALSE.
         lspcfcd(IDXFMT) = .FALSE.
         lspchr(IDXFMT) = .FALSE.
         lspchcd(IDXFMT) = .FALSE.
         lspcfac(IDXFMT) = .FALSE.
         lspcstk(IDXFMT) = .FALSE.
         lspcpnt(IDXFMT) = .FALSE.
         lspcpid = .FALSE.
      endif
c
c   --- read and write rest of header until /DATA/ is found ---
c
  111 continue
      irec = irec + 1
      action = 'Looking for DATA section.'
      read(IORMBR,ERR=7000,END=7001) string
      if( string(:KEYLEN) .EQ. KEYDAT ) goto 222
      write(IOWEMR,'(A)',ERR=7001) string(:strmin(string))
      goto 111
c
c   --- call routine to get date and time string, and create header
c       string ---
c
  222 continue
      call getime(cdate)
      write(IOWEMR,'(A)',ERR=7001) VERSON // cdate
c
c  --- set return flag to success ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,A,I8)',ERR=9999)
     &           'ERROR: Reading HEADER of EMBR file at line: ',irec
      write(ioerr,'(A)',ERR=9999) action(:strmin(action))
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',curifl(:strmin(curifl))
      goto 9999
c
 7001 continue
      write(ioerr,'(/,A,I8)',ERR=9999)
     &           'ERROR: Reading HEADER of EMBR file at line: ',irec
      write(ioerr,'(A)',ERR=9999) 'Premature End-of-File encountered.'
      write(ioerr,'(A)',ERR=9999) action(:strmin(action))
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',curifl(:strmin(curifl))
      goto 9999
c
 7002 continue
      write(ioerr,'(/,A)',ERR=9999)'ERROR: Writing HEADER of EMAR file.'
      write(ioerr,'(A)',ERR=9999) action(:strmin(action))
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',curofl(:strmin(curofl))
      goto 9999
c
 7003 continue
      write(ioerr,'(/,A,I8)',ERR=9999)
     &           'ERROR: Reading HEADER of EMBR file at line: ',irec
      write(ioerr,'(A)',ERR=9999) action(:strmin(action))
      write(ioerr,'(A)',ERR=9999)
     &                      'No criteria pollutants found in the file.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',curifl(:strmin(curifl))
      goto 9999
c
 7004 continue
      write(ioerr,'(/,A)',ERR=9999)
     &                          'ERROR: Reading HEADER of EMBR file.'
      write(ioerr,'(A)',ERR=9999)
     &                    'Number of criteria pollutants exceeds max.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Pollutants in EMBR : ',ncrthd
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maxumium allowed   : ',MXCRIT
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXCRIT ',
     &                                                  'and recompile.'
      goto 9999
c
 7005 continue
      write(ioerr,'(/,A)',ERR=9999)
     &                          'ERROR: Reading HEADER of EMBR file.'
      write(ioerr,'(A)',ERR=9999)
     &                    'Number of speciated pollutants exceeds max.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Species in EMBR : ',nspchd
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maxumium allowed: ',MXSPEC
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXSPEC ',
     &                                                  'and recompile.'
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
 9000 format(2F10.4,3I10,2F10.4)
 9001 format(100(:,A10,1X,F10.1,1X,A10,/))
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
