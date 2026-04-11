**** MSGTBL.F 
c    
      subroutine msgtbl( ierr,
     &                        iounit, title, units, ndim, ncod, icdlen,
     &                            codnam, codtit, npol, header, polnam, 
     &                                              arrayin, arrayout )
c
c----------------------------------------------------------------------
c
c    Writes a table of emissions by any character string code.  An array
c    of emissions by code and emissions species is written. The argument
c    variables appear in the table as shown:
c
c                               title
c                               units
c
c                     header(1,1)    header(2,1)
c        codtit       polnam(1,1)    polnam(2,1)    ...
c        --------------------------------------------------------------
c        codnam(1)    arrayin(1,1)    arrayout(1,2)  ...
c        codnam(2)    arrayin(2,1)    arrayout(2,2)  ...
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
c       icdlen   I   length of character strings containing code names
c       codnam   C   character string of names of codes
c       codtit   C   character string for identifying the codes
c       npol     I   number of pollutants
c       polnam   C   array of pollutant names (can be criteria of CBIV)
c       array    R   two dimensional array of emissions values
c
c----------------------------------------------------------------------
c    LOG:
c----------------------------------------------------------------------
c
c    -gwilson-   Jul 22, 2009   fixed bug in printing headers that
c                               caused seg fault for some tables
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
      integer*4     icdlen
      character*(*) codnam(ncod)
      character*(*) codtit
      integer*4     npol
      character*10  header(2,npol)
      character*10  polnam(2,npol)
      real*4        arrayin(ndim,npol)
      real*4        arrayout(ndim,npol)
c
c----------------------------------------------------------------------
c    External functions:
c----------------------------------------------------------------------
c
c   strmin    I     returns actual length of string (min of 1)
c
      integer*4 strmin
c
c----------------------------------------------------------------------
c    Local parameters:
c----------------------------------------------------------------------
c
c   MXHED   I  number of lines per page
c
      integer*4 MXHED
c
      parameter( MXHED = 9999 )
c
c----------------------------------------------------------------------
c    Local variables:
c----------------------------------------------------------------------
c
c   string  C    trmporary character string for each line
c   blank   C    string for writing blanks
c   ititln  I    length of title string
c   iunln   I    length of units string
c   ilencd  I    length of code title
c   ltot    L    flag for determining if the totals shuld be written
c
      character*(MXSPEC*30+100) string, blank
      character*12        polcntr(MXHED,2), hedcntr(MXHED,2)
      integer*4           ispcln, ititln, iunln, ilencd, iwide, icnt
      integer*4           jlen, ipos, idiff, i, j
      real*4              totalin(npol), totalout(npol)
      logical             ltot
c
c----------------------------------------------------------------------
c   Data statements:
c----------------------------------------------------------------------
c
      data blank /' '/
c
c----------------------------------------------------------------------
c   Entry point:
c----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c   --- compute length of strings ---
c
      ititln = strmin ( title )
      ititln = MAX(1,ititln)
      iunln = strmin ( units )
      ilencd = strmin ( codtit )
      ltot = .TRUE.
      if( ilencd .EQ. 0 ) ltot = .FALSE.
      do i=1,npol
         ispcln = strmin ( polnam(1,i) )
         polcntr(i,1) = blank(:(10-ispcln)/2)//polnam(1,i)(:ispcln)//
     &                              blank(:10-(10-ispcln)/2-ispcln+2)
         ispcln = strmin ( polnam(2,i) )
         polcntr(i,2) = blank(:(10-ispcln)/2)//polnam(2,i)(:ispcln)//
     &                              blank(:10-(10-ispcln)/2-ispcln+2)
         ispcln = strmin ( header(1,i) )
         hedcntr(i,1) = blank(:(10-ispcln)/2)//header(1,i)(:ispcln)//
     &                              blank(:10-(10-ispcln)/2-ispcln+2)
         ispcln = strmin ( header(2,i) )
         hedcntr(i,2) = blank(:(10-ispcln)/2)//header(2,i)(:ispcln)//
     &                              blank(:10-(10-ispcln)/2-ispcln+2)
         totalin(i) = 0.
         totalout(i) = 0.
      enddo
c
c   ---- set the width of the page ----
c
      iwide = (npol*2)*12+icdlen+1
      iwide = MAX(iwide,ititln+1)
      iwide = MAX(iwide,iunln+1)
      idiff = ABS(icdlen-ilencd)
c
c   ---- write title and headings ---
c
      write(iounit,'(//)',ERR=7000) 
      write(iounit,9001,ERR=7000) blank(:(iwide-ititln)/2),
     &         title(:ititln),blank(:(iwide-iunln)/2), units(:iunln)
      write(iounit,'(/)',ERR=7000) 
      write(iounit,9002,ERR=7000) blank(:(idiff)/2),
     &        codtit(:ilencd),blank(:MAX(1,icdlen-(idiff)/2-ilencd)),
     &                            (hedcntr(i,1),hedcntr(i,2),i=1,npol)
      write(iounit,9002,ERR=7000) blank(:(idiff)/2),
     &        blank(:ilencd),blank(:MAX(1,icdlen-(idiff)/2-ilencd)),
     &                            (polcntr(i,1),polcntr(i,2),i=1,npol)
      write(iounit,9003,ERR=7000) ('-',i=1,iwide)
c
c   ---- write emissions table ----
c
      do 10 i=1,ncod
          string = blank
c
c   ---- if this code string is just a "$", then just write a blank line ---
c
          jlen = strmin(codnam(i) )
          if( jlen .EQ. 1 .AND. codnam(i)(1:1) .EQ. '$' ) then
              write(iounit,9003,ERR=7000)
              goto 10
          endif
c
          write(string(1:icdlen+3),9004,ERR=7000) codnam(i)
          icnt = 0
          do j=1,npol
             icnt = icnt + 1
c
c  --- first do the input array ---
c
             ipos = (icnt-1)*12+icdlen+4
             if( arrayin(i,j) .EQ. 0. ) then
                write(string(ipos:ipos+11),9005,ERR=7000) arrayin(i,j)
             else
                if( arrayin(i,j) .GT. 10000.0 .OR.
     &                                 arrayin(i,j) .LT. 0.0001 ) then
                   write(string(ipos:ipos+11),9006,ERR=7000)
     &                                                    arrayin(i,j)
                else
                   write(string(ipos:ipos+11),9007,ERR=7000)
     &                                                    arrayin(i,j)
                endif
                totalin(j) = totalin(j) + arrayin(i,j)
             endif
c
c  --- next do the output array ---
c
             icnt = icnt + 1
             ipos = (icnt-1)*12+icdlen+4
             if( arrayout(i,j) .EQ. 0. ) then
                write(string(ipos:ipos+11),9005,ERR=7000) arrayout(i,j)
             else
                if( arrayout(i,j) .GT. 10000.0 .OR.
     &                                 arrayout(i,j) .LT. 0.0001 ) then
                   write(string(ipos:ipos+11),9006,ERR=7000)
     &                                                    arrayout(i,j)
                else
                   write(string(ipos:ipos+11),9007,ERR=7000)
     &                                                    arrayout(i,j)
                endif
                totalout(j) = totalout(j) + arrayout(i,j)
             endif
          enddo
          write(iounit,9008,ERR=7000) string(:strmin(string))
   10 continue
c
c   --- write totals for each species ---
c
      write(iounit,9003,ERR=7000) ('-',i=1,iwide)
      if( .NOT. ltot ) goto 111
      string = blank
      string(1:icdlen+3) = ' Total  '
      icnt = 0
      do j=1,npol
         icnt = icnt + 1
c
c   --- input totals first ---
c
         ipos = (icnt-1)*12+icdlen+4
         if( totalin(j) .EQ. 0. ) then
           write(string(ipos:ipos+11),9005,ERR=7000) totalin(j)
         else
           if( totalin(j) .GT. 10000.0 .OR. totalin(j) .LT. 0.0001) then
               write(string(ipos:ipos+11),9006,ERR=7000) totalin(j)
           else
               write(string(ipos:ipos+11),9007,ERR=7000) totalin(j)
           endif
         endif
c
c   --- now output totals ---
c
         icnt = icnt + 1
         ipos = (icnt-1)*12+icdlen+4
         if( totalout(j) .EQ. 0. ) then
           write(string(ipos:ipos+11),9005,ERR=7000) totalout(j)
         else
           if(totalout(j) .GT. 10000. .OR. totalout(j) .LT. 0.0001) then
               write(string(ipos:ipos+11),9006,ERR=7000) totalout(j)
           else
               write(string(ipos:ipos+11),9007,ERR=7000) totalout(j)
           endif
         endif
      enddo
      write(iounit,9008,ERR=7000) string(:strmin(string))
c
  111 continue
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
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Writing emissions totals',
     &                                         'table to message file.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',curofl(:strmin(curofl))
      goto 9999
c
c----------------------------------------------------------------------
c   Format statements:
c----------------------------------------------------------------------
c
 9001 format(2A,/,2A)
 9002 format(1X,3A,2X,1000(:A))
 9003 format(1X,2500(:,A))
 9004 format(1X,A,2X)
 9005 format(4X,F3.0,5X)
 9006 format(1E10.3,2X)
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
