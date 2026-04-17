C**** WRTTBL.F version 2.0 09/01/2007
c    
      subroutine wrttbl( ierr,
     &                  iounit, title, units, ndim, ncod, icdlen,
     &                    codnam, codtit, npol, polnam, iarray, ltotpr )
c
c----------------------------------------------------------------------
c
c    Writes a table of emissions by any character string code.  An array
c    of counts is written. The argument variables appear in the 
c    table as shown:
c
c                               title
c                               units
c
c        codtit       polnam(1)    polnam(2)    ...
c        --------------------------------------------------------------
c        codnam(1)    iarray(1,1)    iarray(1,2)  ...
c        codnam(2)    iarray(2,1)    iarray(2,2)  ...
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
c       iarray   I   two dimensional array of emissions values
c       ltotpr   L   flag to determine if totals are printed 
c                    .TRUE. = print line with totals
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
      include 'iopigems.inc'
      include 'pigemstot.inc'
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
      character*20  polnam(npol)
      integer*4     iarray(ndim,npol)
      logical       ltotpr
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
c    local parameters:
c----------------------------------------------------------------------
c
c   PAGLEN  I  number of lines per page
c   PAGWID  I  number of columns per page
c
      integer*4 PAGLEN
      integer*4 PAGWID
c
      parameter( PAGLEN = 9999999 )
      parameter( PAGWID = 9999    )
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
c
      character*(PAGWID) string, blank
      integer*4          nper, ispcln(ICDMSK), ititln, iunln, ilencd
      integer*4          ibegpl, iendpl, iwide, itotal(ICDMSK)
      integer*4          jlen, ibegcd, iendcd, ipos, i, j
      logical            ltot
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
      if( ilencd .EQ. 0 .OR. ltotpr .EQV. .FALSE. ) ltot = .FALSE.
      do i=1,npol
         ispcln(i) = strmin ( polnam(i) )
         itotal(i) = 0.
      enddo
c
c   --- loop through pollutants, setting number per page ---
c
      nper = (PAGWID - icdlen - 6)/22
  111 continue
      if( MOD(npol,nper) .LE. 2 .AND. MOD(npol,nper) .GT. 0) then
          nper = nper - 1
          goto 111
      endif
      do 10 ibegpl = 1,npol,nper
         iendpl = MIN(ibegpl+nper-1,npol)
c
c   ---- loop through codes, PAGLEN number per page ---
c
         do ibegcd=1,ncod,PAGLEN
            iendcd = MIN(ibegcd+PAGLEN-1,ncod)
            iwide = (iendpl-ibegpl+1)*22+icdlen+1
            iwide = MAX(iwide,ititln+1)
            iwide = MAX(iwide,iunln+1)
c
c   ---- write title and headings ---
c
            write(iounit,9001,ERR=7000) blank(1:(iwide-ititln)/2),
     &         title(1:ititln),blank(1:(iwide-iunln)/2), units(1:iunln)
            write(iounit,9002,ERR=7000) blank(1:(icdlen-ilencd)/2),
     &        codtit(1:ilencd),blank(1:icdlen-(icdlen-ilencd)/2-ilencd),
     &                (blank(1:(20-ispcln(i))/2),polnam(i)(1:ispcln(i)),
     &         blank(1:20-(20-ispcln(i))/2-ispcln(i)+2),i=ibegpl,iendpl)
            write(iounit,9003,ERR=7000) ('-',i=1,iwide)
c
c   ---- write emissions table ----
c
           do 20 i=ibegcd,iendcd
              string = blank
c
c   ---- if this code string is just a "$", then just write a blank line ---
c
              jlen = strmin(codnam(i) )
              if( jlen .EQ. 1 .AND. codnam(i)(1:1) .EQ. '$' ) then
                  write(iounit,9003,ERR=7000)
                  goto 20
              endif
c
              write(string(1:icdlen+3),9004,ERR=7000) codnam(i)
              do j=ibegpl,iendpl
                  ipos = (j-ibegpl)*22+icdlen+4
                  write(string(ipos:ipos+22),9005,ERR=7000) iarray(i,j)
                  itotal(j) = itotal(j) + iarray(i,j)
              enddo
              jlen = strmin ( string )
              write(iounit,9006,ERR=7000) string(1:jlen)
   20      continue
         enddo
c
c   --- write totals for each species ---
c
         write(iounit,9003,ERR=7000) ('-',i=1,iwide)
         if( .NOT. ltot ) goto 10
         string = blank
         string(1:icdlen+3) = ' Total  '
         do j=ibegpl,iendpl
            ipos = (j-ibegpl)*22+icdlen+4
            write(string(ipos:ipos+22),9005,ERR=7000) itotal(j)
         enddo
        jlen = strmin ( string )
        write(iounit,9006,ERR=7000) string(1:jlen)
c
   10 continue
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
      write(IOWSTD,'(/,2A,//)',ERR=9999) 'ERROR: Writing emissions ',
     &                                          'table to message file.'
      goto 9999
c
c----------------------------------------------------------------------
c   Format statements:
c----------------------------------------------------------------------
c
 9001 format(/,2A,/,2A)
 9002 format(/,1X,3A,2X,900(:,A))
 9003 format(1X,250(:,A))
 9004 format(1X,A,2X)
 9005 format(6X,I10,6X)
 9006 format(A)
c
c----------------------------------------------------------------------
c   Return point:
c----------------------------------------------------------------------
c
 9999 continue
      return
      end
