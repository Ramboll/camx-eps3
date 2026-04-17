C**** MSGCSV.F 
c    
      subroutine msgcsv( ierr,
     &                        iounit, title, units, ndim, ncod, icdlen,
     &                            codnam, codtit, npol, header, polnam, 
     &                                              arrayin, arrayout )
c
c----------------------------------------------------------------------
c
c    Writes a table of emissions by any character string code.  An array
c    of emissions by code and emissions species is written. This version
c    writes a CSV table that can easily be imported into a spreadsheet 
c    program.
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
c
c      character*(5*MXSTR) string, blank  changed to function of #species
      character*(MXSPEC*30+100) string
      integer*4           icnt,  jlen, ilen, ipos, i, j
c
c----------------------------------------------------------------------
c   Entry point:
c---------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c   ---- write title and headings ---
c
      write(iounit,'(2A)',ERR=7000) title(:strmin(title)),','
      write(iounit,'(2A)',ERR=7000) units(:strmin(units)),','
      write(iounit,'(500A)',ERR=7000) codtit(:strmin(codtit)),',',
     &                   (header(1,i)(:strmin(header(1,i))),' ',
     &                    polnam(1,i)(:strmin(polnam(1,i))),',',
     &                   header(2,i)(:strmin(header(2,i))),' ',
     &         polnam(2,i)(:strmin(polnam(2,i))),',',i=1,npol)
c
c   ---- write emissions table ----
c
      do 10 i=1,ncod
c
c   ---- if this code string is just skip it ---
c
          string = ' '
          jlen = strmin(codnam(i) )
          if( jlen .EQ. 1 .AND. codnam(i)(1:1) .EQ. '$' ) goto 10
c
          write(string(1:icdlen+1),'(2A)',ERR=7000) codnam(i),','
          icnt = 0
          do j=1,npol
             icnt = icnt + 1
c
c  --- first do the input array ---
c
             ipos = (icnt-1)*12+icdlen+4
             if( arrayin(i,j) .EQ. 0. ) then
                write(string(ipos:ipos+11),9000,ERR=7000) arrayin(i,j)
             else
                if( arrayin(i,j) .GT. 10000.0 .OR.
     &                                 arrayin(i,j) .LT. 0.0001 ) then
                   write(string(ipos:ipos+11),9001,ERR=7000)
     &                                                    arrayin(i,j)
                else
                   write(string(ipos:ipos+11),9003,ERR=7000)
     &                                                    arrayin(i,j)
                endif
             endif
c
c  --- next do the output array ---
c
             icnt = icnt + 1
             ipos = (icnt-1)*12+icdlen+4
             if( arrayout(i,j) .EQ. 0. ) then
                write(string(ipos:ipos+11),9000,ERR=7000) arrayout(i,j)
             else
                if( arrayout(i,j) .GT. 10000.0 .OR.
     &                                 arrayout(i,j) .LT. 0.0001 ) then
                   write(string(ipos:ipos+11),9001,ERR=7000)
     &                                                    arrayout(i,j)
                else
                   write(string(ipos:ipos+11),9003,ERR=7000)
     &                                                    arrayout(i,j)
                endif
             endif
          enddo
c
c  --- get rid of the blanks in the string ---
c
          call lftjst(string)
          ilen = strmin(string)
  222     continue
          ipos = INDEX(string(:ilen),' ')
          if( ipos .GT. 0 .AND. ilen .GT. 1 ) then
             call lftjst( string(ipos:) )            
             ilen = strmin(string)
             goto 222
          endif
c
c  --- qrite the condensed line ---
c
          write(iounit,'(A)',ERR=7000) string(:ilen)
c            
c  --- next line in output table ---
c
   10 continue
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
 9000 format(4X,F3.0,',',4X)
 9001 format(1E10.3,','1X)
 9003 format(F10.4,',',1X)
c
c----------------------------------------------------------------------
c   Return point:
c----------------------------------------------------------------------
c
 9999 continue
      return
      end
