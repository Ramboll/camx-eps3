C**** DSCTBL.F 
c
      subroutine dsctbl( ierr,
     &                         title, idxfip )
c
c----------------------------------------------------------------------
c
c    Writes a table of emissions by source category for the indicated
c    county.  The process flag for discretionary controls is also
c    written, indicating which discretionary controls have been
c    applied to the source category.
c
c    Argument Declaration.
c     Outputs:
c       ierr     I   error flag
c     Inputs:
c       title    C   title appearing above each table
c       idxfip   I   index of county for which to write tables
c                    (0 = entire domain)
c
c----------------------------------------------------------------------
c    LOG:
c----------------------------------------------------------------------
c
c----------------------------------------------------------------------
c    Include Files:
c----------------------------------------------------------------------
c
      include 'param.inc'
      include 'embrdat.inc'
      include 'cntylst.inc'
      include 'iocntlem.inc'
      include 'ctltot.inc'
c
c----------------------------------------------------------------------
c    Argument Declarations:
c----------------------------------------------------------------------
c
      integer*4     ierr
      character*80  title
      integer*4     idxfip
c
c----------------------------------------------------------------------
c    External Functions:
c----------------------------------------------------------------------
c
c   strmin    I     returns actual length of string
c
      integer*4 strmin
c
c----------------------------------------------------------------------
c    Local Variables:
c----------------------------------------------------------------------
c
c   LENDSC  I  length of space needed for discretionary labels
c   PAGLEN  I  number of lines per page
c   PAGWID  I  number of columns per page
c
      integer*4 LENDSC
      integer*4 PAGLEN
      integer*4 PAGWID
c
      parameter( LENDSC = 40 )
      parameter( PAGLEN = 99999 )
      parameter( PAGWID = 99999 )
c
c----------------------------------------------------------------------
c    Local Variables:
c----------------------------------------------------------------------
c
c   string  C    trmporary character string for each line
c   blank   C    string for writing blanks
c   nper    I    number of pollutants per line (used to avoid having
c                just one pollutant on a line
c   ititln  I    length of title string
c
      character*(PAGWID) string, blank
      character*24       srclbl
      character*8        dsclbl(4), dscstr
      character*10       name(MXCRIT)
      integer*4          icrtln(MXCRIT)
      integer*4          nper, ititln, ibegpl, iendpl, iwide
      integer*4          ncod, icod, ipos, itmp, i, j
      real*4             tottmp,tmpems
c
c----------------------------------------------------------------------
c   Data statements:
c----------------------------------------------------------------------
c
      data blank /' '/
c
c---------------------------------------------------------------------
c   Entry Point:
c----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c   --- change labels for entire domain ---
c
      if( idxfip .EQ. 0 ) then
          srclbl = '        SIC     SCC     '
          dsclbl(1) = ' P A F'
          dsclbl(2) = ' R S I'
          dsclbl(3) = ' O C P'
          dsclbl(4) = ' J   S'
      else
          srclbl = 'County  SIC     SCC     '
          dsclbl(1) = '      '
          dsclbl(2) = '      '
          dsclbl(3) = '      '
          dsclbl(4) = '      '
      endif
c
c   --- put pollutant names into arrays ---
c
      do i=1,ncrthd
         name(i) = crtnam(i)
      enddo
c
c   --- compute length of species names ---
c
      ititln = strmin ( title )
      do i=1,ncrthd
         icrtln(i) = strmin ( name(i) )
      enddo
c
c   --- loop through pollutants, setting number per page ---
c
      nper = (PAGWID - LENDSC)/24
c
c  --- the following has been commented out to fit more than
c      one species on a line since the number has now been
c      increased to 7.  ---
c
      do 10 ibegpl = 1,ncrthd,nper
         iendpl = MIN(ibegpl+nper-1,ncrthd)
         iwide = (iendpl-ibegpl+1)*24+LENDSC+1
c
c   ---- loop through codes, PAGLEN number per page ---
c
         ncod = 0
         dscstr = '        '
         do 20 itmp=1,nscsic
            icod = idxsrt(itmp)
c
c   ---- disable process flag if doing county table ---
c
            if( idxfip .NE. 0 ) ctlflg(icod) = '    '
c
c   ---- skip this code if emissions is zero ---
c
            tottmp = 0.
            do i=1,ncrthd
                tottmp = tottmp + totifp(icod,idxfip,i)
            enddo
            if( tottmp .LE. 0 ) goto 20
c
c   ---- write title and headings ---
c
            if( MOD( ncod, PAGLEN-5 ) .EQ. 0 ) then
               write(IOWMSG,'(///,A,A)',ERR=7000) 
     &                         blank(1:(iwide-ititln)/2),title(1:ititln)
               write(IOWMSG,'(1X,2A)',ERR=7000) blank(1:24),dsclbl(1)
               write(IOWMSG,'(1X,2A)',ERR=7000) blank(1:24),dsclbl(2)
               write(IOWMSG,'(1X,200(:A))',ERR=7000) blank(1:24),
     &               dsclbl(3), blank(1:5),(blank(1:(22-icrtln(i))/2),
     &                                          name(i)(1:icrtln(i)),
     &                    blank(1:22-(22-icrtln(i))/2-icrtln(i)+2),
     &                                                  i=ibegpl,iendpl)
               write(IOWMSG,'(1X,200(:A))',ERR=7000) srclbl,dsclbl(4),
     &            '    ',('    IN     ','     OUT     ',i=ibegpl,iendpl)
               write(IOWMSG,'(1X,800(:A))',ERR=7000) ('-',i=1,iwide)
            endif
c
c   ---- put process flag into temp string ---
c
            do i=1,IFLCTY
               dscstr(2*i:2*i) = ctlflg(icod)(i:i)
            enddo
c
c   ---- write emissions table ----
c
            ncod = ncod + 1
            string = blank
            if( idxfip .GT. 0 ) string(2:6) = fipcd(idxfip)
            string(8:11) = sccsic(icod)(11:14)
            string(13:22) = sccsic(icod)(1:10)
            string(25:38) = dscstr
c
            do j=ibegpl,iendpl
                ipos = (j-ibegpl)*24+LENDSC-6
c
                tmpems = totifp(icod,idxfip,j)
c
                if( tmpems .EQ. 0. ) then
                   write(string(ipos:ipos+11),'(4X,F3.0,5X)',ERR=7000)
     &                                                           tmpems
                else
                   if( tmpems .GT. 10000.0 .OR.
     &                          tmpems .LT. 0.0001 ) then
                      write(string(ipos:ipos+11),'(1E10.3,2X)',ERR=7000)
     &                                                            tmpems
                   else
                      write(string(ipos:ipos+11),'(F10.4,2X)',ERR=7000)
     &                                                           tmpems
                   endif
                endif
                ipos = ipos + 12
c
                tmpems = totofp(icod,idxfip,j)
c
                if( tmpems .EQ. 0. ) then
                   write(string(ipos:ipos+11),'(4X,F3.0,5X)',ERR=7000)
     &                                                          tmpems
                else
                   if( tmpems .GT. 10000.0 .OR.
     &                         tmpems .LT. 0.0001 ) then
                      write(string(ipos:ipos+11),'(1E10.3,2X)',ERR=7000)
     &                                                          tmpems
                   else
                      write(string(ipos:ipos+11),'(F10.4,2X)',ERR=7000)
     &                                                          tmpems
                   endif
               endif
            enddo
            write(IOWMSG,'(1X,A)',ERR=7000) string(:strmin(string))
c
c   --- get next code ---
c
   20   continue
c
c   --- write totals for each species ---
c
        if( ncod .LE. 0 ) goto 10
        write(IOWMSG,'(1X,800(:A))',ERR=7000) ('-',i=1,iwide)
        string = blank
        string(1:LENDSC) = ' Total  '
        do 70 j=ibegpl,iendpl
           ipos = (j-ibegpl)*24+LENDSC-6
c
           tmpems = totifp(0,idxfip,j)
c
           if( tmpems .EQ. 0. ) then
              write(string(ipos:ipos+11),'(4X,F3.0,5X)',ERR=7000)
     &                                                       tmpems
           else
              if( tmpems .GT. 10000.0 .OR.
     &                           tmpems .LT. 0.0001 ) then
                  write(string(ipos:ipos+11),'(1E10.3,2X)',ERR=7000)
     &                                                        tmpems
              else
                  write(string(ipos:ipos+11),'(F10.4,2X)',ERR=7000)
     &                                                        tmpems
              endif
           endif
           ipos = ipos + 12
c
           tmpems = totofp(0,idxfip,j)
c
           if( tmpems .EQ. 0. ) then
              write(string(ipos:ipos+11),'(4X,F3.0,5X)',ERR=7000)
     &                                                       tmpems
           else
              if( tmpems .GT. 10000.0 .OR.
     &                           tmpems .LT. 0.0001 ) then
                  write(string(ipos:ipos+11),'(1E10.3,2X)',ERR=7000)
     &                                                         tmpems
              else
                  write(string(ipos:ipos+11),'(F10.4,2X)',ERR=7000)
     &                                                      tmpems
              endif
           endif
   70   continue
        write(IOWMSG,'(1X,A)',ERR=7000) string(:strmin(string))
c
c  --- do next group of species ---
c
   10 continue
c
c  --- set error flag to success ---
c
      ierr = ISUCES
      goto 9999
c
c----------------------------------------------------------------------
c   Error Messages:
c----------------------------------------------------------------------
c
 7000 continue
      ioerr = IOWSTD
      write(ioerr,'(/,A)',ERR=9999)
     &          'ERROR:  Writing message file generating output tables.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',msgfl(:strmin(msgfl))
      goto 9999
c
c----------------------------------------------------------------------
c   Format Statements:
c----------------------------------------------------------------------
c
c----------------------------------------------------------------------
c   Return Point:
c----------------------------------------------------------------------
c
 9999 continue
      return
      end
