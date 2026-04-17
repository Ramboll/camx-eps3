C**** RDHDR.F 
c
      subroutine rdhdr( ierr,
     &                        iounit )
c
c-----------------------------------------------------------------------
c
c    Reads the first records in the HEADER of an EMBR file and
c    stores the data in common variables
c
c    Argument declaration.
c     Outputs:
c       ierr   I error flag
c     Inputs:
c       iounit I logical unit number of file to read
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
      include 'uamcntl.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
      integer*4 iounit
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
      integer*4 strmin        
      integer*4 fndchr
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
c   string  C   character string for temporary storage
c   pad     C   temporary storage for last part of species list records
c
      character*(LENREC) string
      character*(MXSTR)  action
      integer*4          irec, i, j
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      irec = 1
      nspecs = 0
c
c   --- read the file type ---
c
      action = 'Reading the file type.'
      read( iounit,ERR=7000,END=7001) filtyp
c
c   --- read the region definition ----
c
      irec = irec + 1
      action = 'Reading the region definition.'
      read( iounit,ERR=7000,END=7001 ) string
      read( string,'(2F10.0,3I10,2F10.0)',ERR=7000) rgutme, rgutmn,
     &                              irgzn, nxrgn, nyrgn, dxrgn, dyrgn
c
c   --- read the number of pollutants and species ---
c
      irec = irec + 1
      action = 'Reading the number of species.'
      read( iounit,ERR=7000,END=7001 ) string
      read( string,'(2I10)',ERR=7000) ncrthd, nspchd
c
c   --- read the data for the criteria pollutants ---
c
      if( ncrthd .LE. 0 ) goto 7002
      if( ncrthd .GT. MXCRIT ) goto 7004
      if( nspchd .GT. MXSPEC ) goto 7005
      irec = irec + 1
      action = 'Reading the names of the criteria pollutants.'
      read( iounit,ERR=7000,END=7001 ) string
      read( string,'(100(:,A10,I10))',ERR=7000) (crtnam(i),
     &                                         isarod(i),i=1,ncrthd)
c
c   --- read CBIV species list and molecular weights in two lines ---
c
      if( nspchd .GT. 0 ) then
         irec = irec + 1
         action = 'Reading the names of the speciated pollutants.'
         nspecs = nspchd
         read( iounit,ERR=7000,END=7001 ) (spclst(i),molwt(i),
     &                                      spcrit(i),i=1,nspchd)
      endif
c
c  --- if there is a species list, get the index of criteria pollutant --
c
      if( nspchd .GT. 0 ) then
          do i=1,nspchd
             ispcrt(i) = 0
             do j=1,ncrthd
                if( spcrit(i) .EQ. crtnam(j) ) ispcrt(i) = j
             enddo
             if( ispcrt(i) .EQ. 0 ) goto 7003
          enddo
      endif
c
c  --- set error flag to success ---
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
      write(ioerr,'(/,A,I8)',ERR=9999) 
     &           'ERROR: Reading HEADER of EMBR file at line: ',irec
      write(ioerr,'(A)',ERR=9999) action(:strmin(action))
      write(ioerr,'(A)',ERR=9999) 
     &                      'No criteria pollutants found in the file.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',curifl(:strmin(curifl))
      goto 9999
c
 7003 continue
      write(ioerr,'(/,A)',ERR=9999) 
     &                          'ERROR: Reading HEADER of EMBR file.'
      write(ioerr,'(3A)',ERR=9999) 
     &'Criteria pollutant for species: ', spclst(i)(:strmin(spclst(i))),
     &                              ' does not appear to be in the file.'
      write(ioerr,'(2A)',ERR=9999) 'File: ',curifl(:strmin(curifl))
      write(ioerr,'(A)',ERR=9999) 'Associated criteria pollutant: ',
     &                                     spcrit(i)(:strmin(spcrit(i)))
      write(ioerr,'(A)',ERR=9999) 'Criteria pollutants in the file:'
      do j=1,ncrthd
          write(ioerr,'(20X,A)') crtnam(j)(:strmin(crtnam(j)))
      enddo
      write(ioerr,'(//)',ERR=9999)
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
