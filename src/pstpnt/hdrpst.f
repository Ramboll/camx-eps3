C***** HDRPST.F 
c
      subroutine hdrpst( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the header and stores the variables in common varibales
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
      include 'uamcntl.inc'
      include 'iopstpnt.inc'
c
c-----------------------------------------------------------------------
c    Argument Declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
c
c-----------------------------------------------------------------------
c    Subroutines Called:
c-----------------------------------------------------------------------
c
c     getime  --  makes system call to get date and time (library)
c     rdhdr   --  reads the input EMBR header (library)
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local Parameters:
c-----------------------------------------------------------------------
c
c   KEYDAT  C   character string for /DATA/ keyword
c
       character*(KEYLEN) KEYDAT
c
       parameter( KEYDAT = '/DATA/' )
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c   jerr    I   error flag for subroutine calls
c   string  C   character string for temporary storage
c
      character*(LENREC) string
      character*40       cdate
      integer*4          jerr, irec, i
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      curofl = oemsfl
      curifl = imbrfl
c
c   --- call routine to get date and time string, and create header
c       string ---
c
      call getime(cdate)
      string = VERSON // cdate
c
c   --- call routine to read the first part of the header ---
c
      call rdhdr(jerr,
     &                 IORMBR )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- do some checks on the species list ---
c
      if( nspchd .NE. nspecs ) goto 7000
      do i=1,nspecs
           if( spcnam(i) .NE. spclst(i) ) goto 7001
      enddo
c
c   --- read rest of header until /DATA/ is found ---
c
      irec = 4
  111 continue
      irec = irec + 1
      read(IORMBR,END=7002,ERR=7003) string
      if( string(1:KEYLEN) .EQ. KEYDAT ) goto 222
      goto 111
c
c   --- if doing a County/Stack file, write the header ---
c
  222 continue
      if( lstksfl ) then
        write(IOWSTK,ERR=7004) ncrthd, nspchd
        write(IOWSTK,ERR=7004) (crtnam(i),isarod(i),i=1,ncrthd)
        write(IOWSTK,ERR=7004) (spclst(i),molwt(i),spcrit(i),i=1,nspchd)
      endif
c
c   --- set error flag to sucess ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Errror Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR:  Number of species in ',
     &                  'EMBR file is not consistent with USERIN file.'
      write(ioerr,'(10X,2A)',ERR=9999)
     &                       'Number of species in EMBR file: ',nspchd
      write(ioerr,'(10X,2A)',ERR=9999)
     &                     'Number of species in USERIN file: ',nspecs
      write(ioerr,'(10X,2A,//)',ERR=9999) 'Change the USERIN file ',
     &                                                'and try again.'
      goto 9999
c
 7001 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR:  Species list in ',
     &                  'EMBR file is not consistent with USERIN file.'
      write(ioerr,'(10X,A)',ERR=9999) 'Species in EMBR file:'
      do i=1,nspchd
        write(ioerr,'(20X,A)',ERR=9999) spclst(i)
      enddo
      write(ioerr,'(10X,A)',ERR=9999) 'Species in USERIN file:'
      do i=1,nspecs
        write(ioerr,'(20X,A)',ERR=9999) spcnam(i)
      enddo
      goto 9999
c
 7002 continue
      write(ioerr,'(/,A,I8)',ERR=9999)
     &            'ERROR: Reading header of EMBR file at line: ',irec
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',curifl(:strmin(curifl))
      goto 9999
c
 7003 continue
      write(ioerr,'(/,A,I8)',ERR=9999)
     &            'ERROR: Reading header of EMBR file at line: ',irec
      write(ioerr,'(A)',ERR=9999) 'Premature end-of-file encountered.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',curifl(:strmin(curifl))
      goto 9999
c
 7004 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR:  Writing header of ',
     &                                      'output STACKS/SCC file.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',stksfl(:strmin(stksfl))
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
