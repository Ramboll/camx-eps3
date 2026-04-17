C**** FILHDR.F 
c   
      subroutine filhdr( ierr, strmod,
     &                         inunit, iounit )
c
c-----------------------------------------------------------------------
c
c    Reads the remaining records in the header of an EMBR file and
c    writes the new EMBR header file with the current module name and
c    and date and time
c
c    Argument declaration.
c     Outputs:
c       ierr   I error flag
c       strmod C character string containing new module information
c     Inputs:
c       inunit I logical unit number of file to read
c       iounit I logical unit number of file to write
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'embrdat.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer*4     ierr
      character*(*) strmod
      integer*4     inunit
      integer*4     iounit
c
c-----------------------------------------------------------------------
c    Local parameters:
c-----------------------------------------------------------------------
c
c   KEYDAT  C   character string for /data/ keyword
c
       character*(KEYLEN) KEYDAT
c
       parameter( KEYDAT = '/DATA/' )
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
c   string  C   character string for temporary storage
c
      character*(LENREC) string
      character*(MXSTR)  action
      integer*4          irec, i
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      irec = 1
c
c   --- write the file type ---
c
      action = 'Writing the file type.'
      write( iounit,ERR=7000 ) filtyp
c
c   --- write the region definition ----
c
      irec = irec + 1
      action = 'Writing the region definition.'
      write( string,'(2F10.4,3I10,2F10.4)',ERR=7000) rgutme, rgutmn,
     &                              irgzn, nxrgn, nyrgn, dxrgn, dyrgn
      write( iounit,ERR=7000 ) string
c
c   --- write the number of pollutants and species ---
c
      irec = irec + 1
      action = 'Writing the number of species.'
      write( string,'(2I10)',ERR=7000) ncrthd, nspchd
      write( iounit,ERR=7000 ) string
c
c   --- write the data for the criteria pollutants ---
c
      if( ncrthd .LE. 0 ) goto 7001
      irec = irec + 1
      action = 'Writing the names of the criteria pollutants.'
      write( string,'(100(:,A10,I10))',ERR=7000) (crtnam(i),
     &                                       isarod(i),i=1,ncrthd)
      write( iounit,ERR=7000 ) string
c
c   --- read CBIV species list and molecular weights in two lines ---
c
      if( nspchd .GT. 0 ) then
         irec = irec + 1
         action = 'Writing the names of the speciated pollutants.'
         write( iounit,ERR=7000) (spclst(i),molwt(i),
     &                                          spcrit(i),i=1,nspchd)
      endif
c
c   --- read and write rest of header until an /DATA/ is found ---
c
  111 continue
      irec = irec + 1
      action = 'Looking for /DATA/ keyword.'
      read( inunit,END=7003,ERR=7002) string
      if( string(:KEYLEN) .EQ. KEYDAT ) goto 222
      write( iounit,ERR=7000 ) string
      goto 111
c
c  --- write current moduule string ---
c
  222 continue
      irec = irec + 1
      write( iounit,ERR=7000 ) strmod
      irec = irec + 1
      string = KEYDAT
      write( iounit,ERR=7000 ) string
c
c  --- set return flag to success ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,A,I8)',ERR=9999)
     &           'ERROR: Writing HEADER of EMBR file at line: ',irec
      write(ioerr,'(A)',ERR=9999) action(:strmin(action))
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',curofl(:strmin(curofl))
      goto 9999
c
 7001 continue
      write(ioerr,'(/,A,I8)',ERR=9999)
     &           'ERROR: Writing HEADER of EMBR file at line: ',irec
      write(ioerr,'(A)',ERR=9999) action(:strmin(action))
      write(ioerr,'(A)',ERR=9999)
     &                      'No criteria pollutants in the inventory.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',curofl(:strmin(curofl))
      goto 9999
c
 7002 continue
      write(ioerr,'(/,A,I8)',ERR=9999)
     &           'ERROR: Reading HEADER of EMBR file at line: ',irec
      write(ioerr,'(A)',ERR=9999) action(:strmin(action))
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',curifl(:strmin(curifl))
      goto 9999
c
 7003 continue
      write(ioerr,'(/,2A,I8)',ERR=9999) 'ERROR: Premature EOF found',
     &                  ' reading HEADER of EMBR file at line: ',irec
      write(ioerr,'(A)',ERR=9999) action(:strmin(action))
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',curifl(:strmin(curifl))
      goto 9999
c
c-----------------------------------------------------------------------
c   Format statements:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
