C**** RDCAMX.F version 2.0 09/01/2007
c
      subroutine rdcamx(ierr)
c
c-----------------------------------------------------------------------
c
c    This routine reads the binary point source file that contains
c    the static sources.
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
      include 'pigemstot.inc'
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
c   Local parameters:
c----------------------------------------------------------------------
c
c   BFUZZ   R   fuzz for handling roundoff od coordinates
c
      real*4 BFUZZ
c
      parameter( BFUZZ = 0.005     )
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
      character*60  fnotein
      character*10  cfile, spname(10*MXSPEC)
      character*4   ifile(10), inote(60), mspec(10,10*MXSPEC)
      integer*4     ione, nspecin, ibgdatin, iendatin, i, j
      integer*4     nxcellin, nycellin, ihr, icell, jcell, ispc, ipt
      real*4        begtimin, endtimin, utmxin, utmyin, xorigin, yorigin
      real*4        dxcellin, dycellin, flowrate
      logical*4     lcheck
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c  --- set the error flag ---
c
      ierr = IFAIL
c
c  --- read the header, checking that it is compatable with
c      USERIN parameters ---
c
      read(IORSTAT,ERR=7000,END=7001) ifile, inote, ione, nspecin, 
     &                       ibgdatin, begtimin, iendatin, endtimin
      write(fnotein,'(60A1)') (inote(i),i=1,60)
      begtimin = begtimin * 100.
      endtimin = endtimin * 100.
      lcheck = .TRUE.
      if( ibgdatin .NE. ibgdat ) lcheck = .FALSE.
      if( ABS(begtimin- begtim) .GT. BFUZZ ) lcheck = .FALSE.
      if( iendatin .NE. iendat ) lcheck = .FALSE.
      if( ABS(endtimin-endtim) .GT. BFUZZ ) lcheck = .FALSE.
      if( .NOT. lcheck ) goto 6000
c
 111  continue
      lcheck = .TRUE.
      read(IORSTAT,ERR=7000,END=7001) utmxin, utmyin, izone, xorigin, 
     &          yorigin, dxcellin, dycellin, nxcellin, nycellin
      utmxin = utmxin / 1000.
      utmyin = utmyin / 1000.
      xorigin = xorigin / 1000.
      yorigin = yorigin / 1000.
      dxcellin = dxcellin / 1000.
      dycellin = dycellin / 1000.
      lcheck = .TRUE.
      if( ABS(utmxin-utmx) .GT. BFUZZ ) lcheck = .FALSE.
      if( ABS(utmyin-utmy) .GT. BFUZZ ) lcheck = .FALSE.
      if( ABS(xorigin-xorig) .GT. BFUZZ ) lcheck = .FALSE.
      if( ABS(yorigin-yorig) .GT. BFUZZ ) lcheck = .FALSE.
      if( ABS(dxcellin-dxcell) .GT. BFUZZ ) lcheck = .FALSE.
      if( ABS(dycellin-dycell) .GT. BFUZZ ) lcheck = .FALSE.
      if( nxcellin .NE. nxcell ) lcheck = .FALSE.
      if( nycellin .NE. nycell ) lcheck = .FALSE.
      if( .NOT. lcheck ) goto 6001
c
 222  continue
      read(IORSTAT,ERR=7000,END=7001) 
      read(IORSTAT,ERR=7000,END=7001) ((mspec(j,i),j=1,10),i=1,nspecin)
      lcheck = .TRUE.
      if( nspecin .NE. nspecs ) lcheck = .FALSE.
      do i=1,nspecin
        write(spname(i),'(10A1)') (mspec(j,i),j=1,10)
        if( spname(i) .NE. spcnam(i) ) lcheck = .FALSE.
      enddo
      if( .NOT. lcheck ) goto 7003
c
c   --- initialize the stack array with sources in this file ---
c
      read(IORSTAT,ERR=7000,END=7001) ione, nstklist
      if( nstklist .GT. MXSTKLST ) goto 7002
      read(IORSTAT,ERR=7000,END=7001) (xloclist(i), yloclist(i), 
     &                         hgtlist(i), dialist(i), tptlist(i), 
     &                                       vellist(i),i=1,nstklist)
c
c  --- read the emissions at each hour ---
c
      ihr = 0
  333 continue
      ihr = ihr + 1
      read(IORSTAT,ERR=7000,END=444) ibgdatin, begtimin, 
     &                                         iendatin, endtimin
      read(IORSTAT,ERR=7000,END=7001) ione, nstklist
      read(IORSTAT,ERR=7000,END=7001) (icell, jcell, ione, 
     &                           flowrate, hgtlist(i),i=1,nstklist)
      do ispc=1,nspecs
        read(IORSTAT,ERR=7000,END=7001) ione,(mspec(i,ispc),i=1,10),
     &                            (emsstk(ipt,ispc,ihr),ipt=1,nstklist)
      enddo
c
c  --- add the emissions for thes sources to the total ---
c
      do ipt=1,nstklist
         lpiglist(ipt) = .FALSE.
         if( dialist(ipt) .LT. 0 ) lpiglist(ipt) = .TRUE.
         do ispc=1,nspecs
            if( ispcrt(ispc) .GT. 0 ) then
               emstot(nemsfl+1,ispcrt(ispc)) =
     &                    emstot(nemsfl+1,ispcrt(ispc)) +
     &                      emsstk(ipt,ispc,ihr) * molwt(ispc) * CVTTON
            endif
         enddo
      enddo

      goto 333
c
c  --- set error flag and return ---
c
 444  continue
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Warning messages:
c-----------------------------------------------------------------------
c
 6000 continue
      write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
      write(ioerr,'(2A)',ERR=9999) 'WARNING: Header read from static',
     &          ' sources file is not consistent with /DATE/ packet.'
      write(ioerr,'(2A)',ERR=9999) 'File: ',staticfl(:strmin(staticfl))
      write(ioerr,'(A)',ERR=9999) 'Input PTSOURCE header:'
      write(ioerr,9003,ERR=9999) 'File note',fnotein(:strmin(fnotein))
      write(ioerr,9001,ERR=9999) 'Beginning date',ibgdatin
      write(ioerr,9004,ERR=9999) 'Beginning time',begtimin
      write(ioerr,9001,ERR=9999) 'Ending date',iendatin
      write(ioerr,9004,ERR=9999) 'Ending time',endtimin
c
      write(ioerr,'(/A)',ERR=9999) '/DATE/ header:'
      write(ioerr,9005,ERR=9999) 'Beginning date (Calander)',ibgcal
      write(ioerr,9001,ERR=9999) 'Beginning date (Julian)',ibgdat
      write(ioerr,9004,ERR=9999) 'Beginning time',begtim
      write(ioerr,9005,ERR=9999) 'Ending date (Calander)',iencal
      write(ioerr,9001,ERR=9999) 'Ending date (Julian)',iendat
      write(ioerr,9004,ERR=9999) 'Ending time',endtim
      write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
      nwarn = nwarn + 1
      goto 111
c
 6001 continue
      write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
      write(ioerr,'(2A)',ERR=9999) 'WARNING: Header read from static',
     &          ' sources file is not consistent with /REGION/ packet.'
      write(ioerr,'(2A)',ERR=9999) 'File: ',staticfl(:strmin(staticfl))
      write(ioerr,'(A)',ERR=9999) 'Input EMISSIONS header:'
      write(ioerr,9000,ERR=9999) 'UTM origin (km)',utmxin,utmyin
      write(ioerr,9000,ERR=9999) 'Grid origin (km)',xorigin,yorigin
      write(ioerr,9000,ERR=9999) 'Grid cell width (km)',
     &                                             dxcellin,dycellin
      write(ioerr,9002,ERR=9999) 'Number of cells',nxcellin,nycellin
      write(ioerr,'(/,A)',ERR=9999) '/REGION/ packet:'
      write(ioerr,9000,ERR=9999) 'UTM origin (km)',utmx,utmy
      write(ioerr,9000,ERR=9999) 'Grid origin (km)',xorig,yorig
      write(ioerr,9000,ERR=9999) 'Grid cell width (km)',
     &                                             dxcell,dycell
      write(ioerr,9002,ERR=9999) 'Number of cells',nxcell,nycell
      write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
      nwarn = nwarn + 1
      goto 222
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,A)',ERR=9999)
     &                'ERROR:  Reading input CAMx point source file.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',
     &                                       staticfl(:strmin(staticfl))
      goto 9999
c
 7001 continue
      write(ioerr,'(/,A,I8)',ERR=9999)
     &                'ERROR:  Reading input CAMx point source file.'
      write(ioerr,'(A)',ERR=9999) 'Premature end-of-file encountered.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',
     &                                       staticfl(:strmin(staticfl))
      goto 9999
c
 7002 continue
      write(ioerr,'(/,3A)',ERR=9999) 'ERROR: Reading the input CAMx ',
     &                                                  'PTSOURCE file.'
      write(ioerr,'(A)',ERR=9999) 'Number of stacks exceeds max.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Number of stacks : ',nstklist
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maxumium allowed : ',MXSTKLST
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter ',
     &                                        'MXSTKLST and recompile.'
      goto 9999
c
 7003 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR:  Species list in input',
     &        ' CAMx file is not consistent with /SPECIES LIST/ packet.'
      write(ioerr,'(10X,A)',ERR=9999) 'Species in PTSOURCE file:'
      write(ioerr,9001,ERR=9999) '        Number of species',nspecin
      do i=1,nspecin
        write(ioerr,'(20X,A)',ERR=9999) spname(i)
      enddo
      write(ioerr,'(/,10X,A)',ERR=9999) 'Species in USERIN file:'
      write(ioerr,9001,ERR=9999) '        Number of species',nspecs
      do i=1,nspecs
        write(ioerr,'(20X,A)',ERR=9999) spcnam(i)
      enddo
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
 9000 format(T10,A,T40,:,': (',F10.3,',',F10.3,')')
 9001 format(T10,A,T40,:,':',I5)
 9002 format(T10,A,T40,:,': (',I5,',',I5,')')
 9003 format(T10,A,T40,:,':',A)
 9004 format(T10,A,T40,:,':',F7.2)
 9005 format(T10,A,T40,:,':',I6)
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
