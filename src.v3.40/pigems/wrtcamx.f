C**** WRTCAMX.F version 2.0 09/01/2007
c
      subroutine wrtcamx(ierr)
c
c-----------------------------------------------------------------------
c
c    This routine writes the binary output file that will be 
c    used by the CAMx file.
c
c    Argument Declaration.
c     Outputs:
c        ierr  I  error code
c     Inputs:
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c    june 2015 mjimenez Updated to support the plume rise
c                       distribution override feature of CAMx
c
c-----------------------------------------------------------------------
c    Include Files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'uamcntl.inc'
      include 'uamregn.inc'
      include 'embrdat.inc'
      include 'iopigems.inc'
      include 'usrpigems.inc'
      include 'stkpigems.inc'
c
c-----------------------------------------------------------------------
c    Argument Declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr    
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
      integer*4    icell(MXSTKLST), jcell(MXSTKLST), ione, izero, ihr
      integer*4    ispc, ipt, i, j
      real*4       rone, rzero, flowrate(MXSTKLST), timbeg, timend
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
      write(IOWEMS,ERR=7000) ifile, inote, ione, nspecs, 
     &                    ibgdat, begtim/100., iendat, endtim/100.
      write(IOWEMS,ERR=7000) utmx*1000., utmy*1000., izone, xorig*1000., 
     &          yorig*1000., dxcell*1000., dycell*1000., nxcell, nycell, 
     &                         ione, izero, izero, rzero, rzero, rzero
      write(IOWEMS,ERR=7000) ione, ione, nxcell, nycell
      write(IOWEMS,ERR=7000) ((mspec(j,i),j=1,10),i=1,nspecs)
      write(IOWEMS,ERR=7000) ione, nstklist
      write(IOWEMS,ERR=7000) (xloclist(i), yloclist(i), hgtlist(i), 
     &                 dialist(i), tptlist(i), vellist(i),i=1,nstklist)
c
c  --- calculate the hourly flowrate ---
c
      do i=1,nstklist
        if( .NOT. lprdo(i) ) then
           flowrate(i) = 3.1415927 * vellist(i) * (dialist(i)/2.)**2
        else
           flowrate(i) = vellist(i)
        endif
        icell(i) = INT( (xloclist(i)/1000. - xorig) / dxcell ) + 1
        jcell(i) = INT( (yloclist(i)/1000. - yorig) / dycell ) + 1
      enddo
c
c  --- write the emissions at each hour ---
c
      do ihr=1,MXHOUR
         timbeg = FLOAT( ihr ) - 1.0
         timend = FLOAT( ihr )
         write(IOWEMS,ERR=7000) ibgdat, timbeg, ibgdat, timend
         write(IOWEMS,ERR=7000) ione, nstklist
         write(IOWEMS,ERR=7000) (icell(i), jcell(i), ione, flowrate(i), 
     &                                         hgtlist(i),i=1,nstklist)
         do ispc=1,nspecs
           write(IOWEMS,ERR=7000) ione,(mspec(i,ispc),i=1,10),
     &                    (emsstk(ipt,ispc,ihr),ipt=1,nstklist)
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
     &                                         ostkfl(:strmin(ostkfl))
      goto 9999
c
 7001 continue
      write(ioerr,'(/,3A)',ERR=9999) 'ERROR: Reading the master stack ',
     &                                                      'list file.'
      write(ioerr,'(A)',ERR=9999) 'Number of stacks exceeds max.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maxumium allowed : ',MXSTKLST
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter ',
     &                                        'MXSTKLST and recompile.'
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
