C**** WRITBL.F 
c   
      subroutine writbl( ierr,
     &                        iounit, title, units, ndim, ncod,
     &                            icode, codtit, npol, polnam, array )
c
c----------------------------------------------------------------------
c
c    Writes a table of emissions by any integer code.  An array
c    of emissions by code and emissions species is written.  The argument
c    variables appear in the table as shown:
c
c                               title
c                               units
c
c        codtit       polnam(1)    polnam(2)    ...
c        --------------------------------------------------------------
c        icode(1)    array(1,1)    array(1,2)   ...
c        icode(2)    array(2,1)    array(2,2)   ...
c           .             .             .
c           .             .             .
c           .             .             .
c
c    Argument declaration.
c     Outputs:
c       ierr     I   error flag
c     Inputs:
c       iounit   I   logical unit number of file to write
c       title    C   title appearing above each table
c       units    C   character string for identifying units
c       ndim     I   declared first dimension of the array
c       ncod     I   actual number of codes
c       icode    I   array of integer codes
c       codtit   C   character string for identifying the codes
c       npol     I   number of pollutants
c       polnam   C   array of pollutant names (can be criteria of CBIV)
c       array    R   two dimensional array of emissions values
c
c----------------------------------------------------------------------
c    LOG:
c----------------------------------------------------------------------
c
c----------------------------------------------------------------------
c    Include files:
c----------------------------------------------------------------------
c
      include 'param.inc'
      include 'embrdat.inc'
c
c----------------------------------------------------------------------
c    Argument declarations:
c----------------------------------------------------------------------
c
      integer*4     ierr
      integer*4     iounit
      character*80  title
      character*20  units
      integer*4     ndim
      integer*4     ncod
      integer*4     icode(ncod)
      character*11  codtit
      integer*4     npol
      character*10  polnam(npol)
      real*4        array(ndim,npol)
c
c----------------------------------------------------------------------
c    External functions:
c----------------------------------------------------------------------
c
c   strmin    I     returns actual length of string
c
      integer*4 strmin
c
c----------------------------------------------------------------------
c    Local variables:
c----------------------------------------------------------------------
c
c   PAGLEN  I  number of lines per page
c   PAGWID  I  number of columns per page
c
      integer*4 PAGLEN
      integer*4 PAGWID
c
      parameter( PAGLEN = 9999999 )
      parameter( PAGWID = 132 )
c
c----------------------------------------------------------------------
c    Local variables:
c----------------------------------------------------------------------
c
c   string  C    trmporary character string for each line
c   blank   C    string for writing blanks
c   nper    I    number of pollutants per line (used to avoid the havin
c                just one pollutant on a line
c   ispcln  I    array for length of species names
c   ititln  I    length of title string
c   iunln   I    length of units string
c   ilencd  I    length of code title
c   strfmt  C    array of character strings for formatting the codes
c   idxfmt  I    index of array of format strings
c   imax    I    maximum integer code
c
      character*(PAGWID) string, blank
      character*8        strfmt(11)
      integer*4          nper, ispcln(MXSPEC), ititln, iunln, ilencd
      integer*4          ibegpl, iendpl, idxfmt, imax
      integer*4          jlen, ibegcd, iendcd, ipos, jpos, i, j
      real*4             total(MXSPEC)
c
c----------------------------------------------------------------------
c   Data statements:
c----------------------------------------------------------------------
c
      data strfmt /'(1X,I1) ','(1X,I2) ','(1X,I3) ','(1X,I4) ',
     &             '(1X,I5) ','(1X,I6) ','(1X,I7) ','(1X,I8) ',
     &             '(1X,I9) ','(1X,I10)','(1X,I11)'/
      data blank /' '/
c
c----------------------------------------------------------------------
c   Entry point:
c----------------------------------------------------------------------
c
c   --- set error flag ----
c
      ierr = IFAIL
c
c   --- find maximum code for formatting ---
c
      imax = -9999999
      do i=1,ncod
         if( icode(i) .GT. imax ) imax = icode(i)
      enddo
c
c   --- determine which format strong will be used ---
c
      idxfmt = 11
      do i=1,10
         if( imax/10**i .LT. 1 ) then
            idxfmt = i
            goto 111
         endif
      enddo
c
c   --- compute length of strings ---
c
  111 continue
      ititln = strmin ( title )
      iunln = strmin ( units )
      ilencd = strmin ( codtit )
      do i=1,npol
         ispcln(i) = strmin ( polnam(i) )
         total(i) = 0.
      enddo
c
c   --- loop through pollutants, setting number per page ---
c
      nper = (PAGWID - ilencd - 3)/12
  222 continue
      if( MOD(npol,nper) .LE. 2 .AND. MOD(npol,nper) .GT. 0) then
          nper = nper - 1
          goto 222
      endif
      do ibegpl = 1,npol,nper
         iendpl = MIN(ibegpl+nper-1,npol)
c
c   ---- loop through codes, PAGLEN number per page ---
c
         do ibegcd=1,ncod,PAGLEN
            iendcd = MIN(ibegcd+PAGLEN-1,ncod)
c
c   ---- write title and headings ---
c
            write(iounit,9001,ERR=7000) blank(1:(PAGWID-ititln)/2),
     &         title(1:ititln),blank(1:(PAGWID-iunln)/2), units(1:iunln)
            ipos = (MAX(idxfmt,ilencd)-ilencd)/2+1
            write(iounit,9002,ERR=7000)blank(1:ipos),codtit(1:ilencd),
     &                       blank(1:ipos+1),(blank(1:(10-ispcln(i))/2),
     &                                          polnam(i)(1:ispcln(i)),
     &        blank(1:10-(10-ispcln(i))/2-ispcln(i)+2),i=ibegpl,iendpl)
            ipos = MAX(idxfmt,ilencd)
            write(iounit,9003,ERR=7000)
     &                          ('-',i=1,(iendpl-ibegpl+1)*12+ipos+1)
c
c   ---- write emissions table ----
c
           do i=ibegcd,iendcd
              string = blank
              ipos = (MAX(ilencd,idxfmt)-idxfmt)/2+1
              write(string(ipos:ipos+idxfmt),strfmt(idxfmt),ERR=7000)
     &                                                         icode(i)
              do j=ibegpl,iendpl
                  jpos = (j-ibegpl)*12+MAX(idxfmt,ilencd)+4
                  if( array(i,j) .EQ. 0. ) then
                    write(string(jpos:jpos+11),9005,ERR=7000) array(i,j)
                  else
                    if( array(i,j) .GT. 10000.0 .OR.
     &                                    array(i,j) .LT. 0.0001 ) then
                        write(string(jpos:jpos+11),9006,ERR=7000)
     &                                                      array(i,j)
                    else
                        write(string(jpos:jpos+11),9007,ERR=7000)
     &                                                      array(i,j)
                    endif
                    total(j) = total(j) + array(i,j)
                  endif
              enddo
              jlen = strmin ( string )
              write(iounit,9008,ERR=7000) string(1:jlen)
           enddo
         enddo
c
c   --- write totals for each species ---
c
         string = blank
         ipos = MAX(idxfmt,ilencd)
         write(iounit,9003,ERR=7000)
     &                          ('-',i=1,(iendpl-ibegpl+1)*12+ipos+1)
         string(1:ipos+1) = ' Total'
         do j=ibegpl,iendpl
            jpos = (j-ibegpl)*12+MAX(idxfmt,ilencd)+4
            if( total(j) .EQ. 0. ) then
              write(string(jpos:jpos+11),9005,ERR=7000) total(j)
            else
              if( total(j) .GT. 10000.0 .OR. total(j) .LT. 0.0001 ) then
                  write(string(jpos:jpos+11),9006,ERR=7000) total(j)
              else
                  write(string(jpos:jpos+11),9007,ERR=7000) total(j)
              endif
            endif
         enddo
        jlen = strmin ( string )
        write(iounit,9008,ERR=7000) string(1:jlen)
c
      enddo
c
c  --- set error flag to success ---
c
      ierr = ISUCES
      goto 9999
c
c----------------------------------------------------------------------
c   Error messages:
c----------------------------------------------------------------------
c
 7000 continue
      ioerr = IOWSTD
      write(ioerr,'(/,A)',ERR=9999)
     &          'ERROR:  Writing message file generating output tables.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',curofl(:strmin(curofl))
      goto 9999
c
c----------------------------------------------------------------------
c   Format statements:
c----------------------------------------------------------------------
c
 9001 format('1',/,A,A,/,A,A)
 9002 format(/,A,A,A,30(:,A,A,A))
 9003 format(1X,250(:,A))
 9005 format(4X,F3.0,5X)
 9006 format(1E10.4,2X)
 9007 format(F10.4,2X)
 9008 format(A)
c
c----------------------------------------------------------------------
c   Return point:
c----------------------------------------------------------------------
c
 9999 continue
      return
      end
