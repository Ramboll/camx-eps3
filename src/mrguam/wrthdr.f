C***** WRTHDR.F 
c
      subroutine wrthdr( ierr )
c
c-----------------------------------------------------------------------
c
c    Writes the merged header record to output emissions file.
c
c    Argument Declaration.
c     Outputs:
c       ierr  I  error flag
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
      include 'uamcntl.inc'
      include 'uamregn.inc'
      include 'iomrguam.inc'
c
c-----------------------------------------------------------------------
c    Argument Declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
c
c-----------------------------------------------------------------------
c    External functions
c-----------------------------------------------------------------------
c
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c   estr      C  string for 'EMISSIONS'
c   ifname    I  fname in integer format
c   ifnote    I  file description in integer format
c   nseg      I  always 1
c
      character*10 estr
      character*4  ifname(10), ifnote(60)
      integer*4    ispc, jerr, nseg, i, j
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- assume failure ---
c
      ierr = ISUCES
c
c   --- since some machines will get some fuzz after dividing the
c       origin variables by 1000, we read the clean values again ---
c
      call rdregn( jerr,
     &                  IORINP )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- put species list into global arrays ---
c
      do i=1,nspecs
        read(spcnam(i),'(10A1)') (intnam(i,j),j=1,10)
      enddo
c
c   --- record 1 ---
c
      estr = 'EMISSIONS'
      read(estr,'(10A1)') ifname
      read(estr,'(60A1)') ifnote
      nseg = 1
c
      write(IOWUAM,ERR=7000) ifname, ifnote, nseg, nspecs, ibgdat, 
     &                       begtim/100, iendat, endtim/100
c
c   --- record 2 ---
c
      write(IOWUAM,ERR=7000) utmx, utmy, izone, xorig*1000, 
     &                       yorig*1000, dxcell*1000, dycell*1000, 
     &                       nxcell, nycell, nzcell, nzlowr, nzuppr, 
     &                       htsur, htlowr, htuppr
c
c   --- record 3 ---
c
      write(IOWUAM,ERR=7000) seg4
c
c   --- record 4 ---
c
      write(IOWUAM,ERR=7000) ((intnam(ispc,i),i=1,10),ispc=1,nspecs)
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR:  Writing header of ',
     &                                         'output emissions file.'
      write(ioerr,'(2A,//)',ERR=9999)  'File: ',ouamfl(:strmin(ouamfl))
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
