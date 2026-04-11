C**** HDRGRD.F 
c
      subroutine hdrgrd( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the header and stores the variables in common varibales
c    then it creates the program name/date/time string and writes
c    the rest of the header
c    If the output file type is UAM the header is just skipped
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
      include 'uamregn.inc'
      include 'uamcntl.inc'
      include 'usrgrdem.inc'
      include 'iogrdem.inc'
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
c     filhdr  --  writes the new EMBR header (library)
c     getime  --  makes system call to get date and time (library)
c     rdhdr   --  reads the input EMBR header (library)
c
c-----------------------------------------------------------------------
c    External functions
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
c   cdate   C   character string for date and time
c   ifnote  I   file note as an integer
c   iftype  I   file type as integer
c   ispecs  I   species list as integers
c   jerr    I   error flag for subroutine calls
c   string  C   character string for temporary storage
c
      character*(LENREC) string
      character*40       cdate
      character*10       cftype
      character*4        ifnote(60), iftype(10), ispecs(10,MXSPEC)
      integer*4          jerr, nseg, ixy, irec, i, j
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
      data cftype /'EMISSIONS '/
      data ixy   /0/
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
c   --- if output is an EMBR file, write new header ---
c
      if( .NOT. lnetcdf_out .AND. .NOT. luam .AND. .NOT. lmeds ) then
         rgutme = xorig
         rgutmn = yorig
         irgzn = izone
         nxrgn = nxcell
         nyrgn = nycell
         dxrgn = dxcell
         dyrgn = dycell
         call filhdr(jerr, string,
     &                          IORMBR, IOWEMS )
         if( jerr .NE. ISUCES ) goto 9999
c
c  --- if output is UAM file, then write UAM header
c      first verify that species lists are compatible ----
c
      else
c
c   --- since some machines will get some fuzz after dividing the
c       origin variables by 1000, we read the clean values again ---
c
         call rdregn( jerr,
     &                      IORINP )
         if( jerr .NE. ISUCES ) goto 9999
c
c   --- read rest of header until /DATA/ is found ---
c
         irec = 4
  111    continue
         irec = irec + 1
         read(IORMBR,ERR=7000,END=7001) string
         if( string(:KEYLEN) .EQ. KEYDAT ) goto 222
         goto 111
c
c   --- write out UAM header date ---
c
  222    continue
c
c   --- if MEDS output or not doing legacy output, we're done ---
c
         if( lmeds .OR. .NOT. loutfile ) goto 333
         if( nspchd .NE. nspecs ) goto 7003
         do i=1,nspecs
            if( spcnam(i) .NE. spclst(i) ) goto 7004
         enddo
         nseg = 1
         read(cftype,'(10A1)') (iftype(i),i=1,10)
         read(fnote,'(60A1)') (ifnote(i),i=1,60)
         do j=1,nspecs
            read(spcnam(j),'(10A1)') (ispecs(i,j),i=1,10)
         enddo
         write(IOWEMS,ERR=7005) iftype, ifnote, nseg, nspecs, ibgdat, 
     &                                 begtim/100., iendat, endtim/100.
         write(IOWEMS,ERR=7005) utmx, utmy, izone, xorig*1000., 
     &                        yorig*1000., dxcell*1000., dycell*1000.,
     &                        nxcell, nycell, nzcell, nzlowr, nzuppr, 
     &                                            htsur, htlowr, htuppr
         write(IOWEMS,ERR=7005) ixy, ixy, nxcell, nycell
         write(IOWEMS,ERR=7005) ((ispecs(i,j),i=1,10),j=1,nspecs)
      endif
c
c   --- set error flag to sucess ---
c
  333 continue
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Errror Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,A,I8)',ERR=9999)
     &            'ERROR: Reading header of EMBR file at line: ',irec
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',curifl(:strmin(curifl))
      goto 9999
c
 7001 continue
      write(ioerr,'(/,A,I8)',ERR=9999)
     &            'ERROR: Reading header of EMBR file at line: ',irec
      write(ioerr,'(A)',ERR=9999) 'Premature end-of-file encountered.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',curifl(:strmin(curifl))
      goto 9999
c
 7003 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR:  Number of species in ',
     &                  'EMBR file is not consistent with USERIN file.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 
     &                       'Number of species in EMBR file: ',nspchd
      write(ioerr,'(10X,A,I10)',ERR=9999) 
     &                     'Number of species in USERIN file: ',nspecs
      write(ioerr,'(10X,2A,//)',ERR=9999) 'Change the USERIN file and ',
     &                                                    'try again.'
      goto 9999
c
 7004 continue
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
 7005 continue
      write(ioerr,'(/,A)',ERR=9999)
     &            'ERROR: Writing header of output emissions file.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',curofl(:strmin(curofl))
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
