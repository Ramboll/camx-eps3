C**** WRTCAMX.F version 2.0 09/01/2007
c
      subroutine wrtptsrc(ierr,
     &                    icurdt, emsgrd, nhours )
c
c-----------------------------------------------------------------------
c
c    This routine writes the binary point source output file that will be 
c    used by the CAMx file.
c
c    Argument Declaration.
c     Outputs:
c        ierr  I  error code
c     Inputs:
c       icurdt   I  current date and time
c       emsgrd   R  hourly speciated emissions for each stack (moles)
c       nhours   I  number of hours prcoessed in this episode
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
      include 'embrdat.inc'
      include 'iopstfir.inc'
      include 'firebin.inc'
      include 'usrpstfir.inc'
      include 'pstfirstk.inc'
c
c-----------------------------------------------------------------------
c    Argument Declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr    
      integer*4 nhours
      integer*4 icurdt(MXHOUR)
      real*4    emsgrd(MXSPEC,MXSTK,MXHOUR)
c
c-----------------------------------------------------------------------
c    External Functions:
c-----------------------------------------------------------------------
c
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
      character*4  ifile(10), inote(60), mspec(10,MXSPEC)
      character*10 cfile
      integer*4    icell(MXSTK), jcell(MXSTK), ione, izero, ihr
      integer*4    ispc, ipt, i, j
      real*4       rone, rzero
      real*4       timbeg, timend
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
      data cfile /'PTSOURCE'/
      data ione /1/
      data izero /0/
      data rone /1.0/
      data rzero /0.0/
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c  --- set the error flag ---
c
      ierr = IFAIL
c
c  --- write the header ---
c
      read(cfile,'(10A1)') (ifile(i),i=1,10)
      read(fnote,'(60A1)') (inote(i),i=1,60)
      do i=1,nspecs
         read(spcnam(i),'(10A1)') (mspec(j,i),j=1,10)
      enddo
      write(IOWPTS,ERR=7000) ifile, inote, ione, nspecs, 
     &                    ibgdat, begtim/100., iendat, endtim/100.
      write(IOWPTS,ERR=7000) utmx*1000., utmy*1000., izone, xorig*1000., 
     &          yorig*1000., dxcell*1000., dycell*1000., nxcell, nycell, 
     &                         ione, izero, izero, rzero, rzero, rzero
      write(IOWPTS,ERR=7000) ione, ione, nxcell, nycell
      write(IOWPTS,ERR=7000) ((mspec(j,i),j=1,10),i=1,nspecs)
      write(IOWPTS,ERR=7000) ione, nstack
      write(IOWPTS,ERR=7000) (xloc(i)*1000., yloc(i)*1000., rone, 
     &                 rone, rone, rone,i=1,nstack)
c
c  --- calculate the stack i,j cell ---
c
      do i=1,nstack
        icell(i) = INT( (xloc(i) - xorig) / dxcell ) + 1
        jcell(i) = INT( (yloc(i) - yorig) / dycell ) + 1
      enddo
c
c  --- write the emissions at each hour ---
c
      do ihr=1,MXHOUR
c
c  --- lookup the plume top and bottom for each stack ---
c
         timbeg = FLOAT( ihr ) - 1.0
         timend = FLOAT( ihr )
         write(IOWPTS,ERR=7000) ibgdat, timbeg, ibgdat, timend
         write(IOWPTS,ERR=7000) ione, nstack

         write(IOWPTS,ERR=7000) (icell(i), jcell(i), ione, 
     &                -1.*stkbot(i,ihr), -1.*stktop(i,ihr),i=1,nstack)
         do ispc=1,nspecs
           write(IOWPTS,ERR=7000) ione,(mspec(i,ispc),i=1,10),
     &                    (emsgrd(ispc,ipt,ihr),ipt=1,nstack)
         enddo
      enddo
c
c  --- set error flag and return ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Warning messages:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,A)',ERR=9999)
     &                'ERROR:  Writing output CAMx point source file.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',
     &                                         optsfl(:strmin(optsfl))
      goto 9999
c
 7001 continue
      write(ioerr,'(/,3A)',ERR=9999) 'ERROR: Reading the master stack ',
     &                                                      'list file.'
      write(ioerr,'(A)',ERR=9999) 'Number of stacks exceeds max.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maxumium allowed : ',MXSTK
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter ',
     &                                        'MXSTK and recompile.'
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
 9000 format(4(A10,1X),F10.1,1X,F10.3,1X,4(F10.0,1X))
 9001 format(4(A10,1X),F10.1,1X,F10.3,1X,2(F10.0,1X),2(F10.4,1X))
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
