C**** WRTELEFIR.F
c
      subroutine wrtelefir( ierr,
     &                         icurdt, emscrt, emsgrd, nhours )
c
c-----------------------------------------------------------------------
c
c    Write the fire emissions data to the elevated file and header file.
c
c    The facility ID is assigned "F" with sequential number.
c    The stack ID is assigned "SL" with sequential number by facility.
c    The stack height is assigned the negative (-) of the layer midpoint.
c
c    Argument Declaration.
c     Outputs:
c       ierr     I  error flag
c     Inputs:
c       icurdt   I  current date and time
c       emscrt   R  daily criteria emissions for each stack (tons)
c       emsgrd   R  hourly speciated emissions for each stack (moles)
c       nhours   I  number of hours prcoessed in this episode
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c    16 sep 2014  mjimenez modify daily total format to scientific
c    02 jun 2015  mjimenez modified to support plume rise override feature
c
c-----------------------------------------------------------------------
c    Include Files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'uamregn.inc'
      include 'embrdat.inc'
      include 'uamcntl.inc'
      include 'firebin.inc'
      include 'layers.inc'
      include 'iopstfir.inc'
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
      real*4    emscrt(MXCRIT,MXSTK)
      real*4    emsgrd(MXSPEC,MXSTK,MXHOUR)
c
c-----------------------------------------------------------------------
c    External Functions:
c-----------------------------------------------------------------------
c
c    julcal  I   converts julian date to calender date
c    strmin  I   returns the true length of a string
c
      integer*4 julcal
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
      character*(12*MXHOUR) emsstr
      character*10          fiptmp, oplnt, ostck,  keypts
      integer*4             idxhr, i, j, ipos, idate, nl, ii
      real*4                xcoord, ycoord, tot, plumemax, plumemin
      logical*4             lflag(MXLAY)
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
      data keypts /'ORIGINAL '/    
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- initialize error code ---
c
      ierr = IFAIL

         idate = julcal( icurdt(1)/100 )
         nl = 0
c
c     ---for each stack; fips and x,y loc  and additional 'surface' stacks ---  -------------------
c
      do 200 i=1,nstack
          plumemax = 0.0
          plumemin = 9999.
          nl = 1
c
          do 300 j=1,nspecs
            tot = 0.0
            emsstr = ' '
c
c            ---  write hourly emissions to temporary string ---
            do 400 idxhr=1,nhours
               ipos = (idxhr-1)*12 + 1
               if( emsgrd(j,i,idxhr) .GT. 0. ) then
                   tot = tot + emsgrd(j,i,idxhr)
                   if( emsgrd(j,i,idxhr) .GT. 10.0**6 ) then
                       write(emsstr(ipos:ipos+11),'(F10.0)',ERR=7002) 
     &                                             emsgrd(j,i,idxhr) 
                   else if( emsgrd(j,i,idxhr) .LT. 1.0 ) then
                        write(emsstr(ipos:ipos+11),'(F10.8)',ERR=7002) 
     &                                             emsgrd(j,i,idxhr) 
                   else
                       write(emsstr(ipos:ipos+11),'(F10.3)',ERR=7002) 
     &                                             emsgrd(j,i,idxhr) 
                   endif
               else
                       write(emsstr(ipos:ipos+11),'(F10.0)',ERR=7002)
     &                                                 0.0
               endif

c           --- next hour ---
  400       continue

            if( plumemax .EQ. 0.0 ) then
            	do 410 idxhr=1,nhours
                    plumemax = MAX(plumemax,stktop(i,idxhr))
                    plumemin = MIN(plumemin,stkbot(i,idxhr))
  410       	continue
            endif
c
c           --- write non-zero emissions estimates ---
            if ( tot .GT. 0.0 ) then
              fiptmp = infips(i)
              call lftjst( fiptmp )
              write(oplnt,9002) 'F', i
              write(ostck,9003) 'SL', nl
              if( izone .GE. 0 ) then
                  xcoord = xloc(i) * 1000
                  ycoord = yloc(i) * 1000
                  write(IOWELV,9000,ERR=7000) fiptmp, oplnt,
     &                 ostck, -1.*plumemax, 1.0, 1.0, 
     &                -1.*plumemin, xcoord, ycoord, idate, spcrit(j),
     &                               emscrt(ispcrt(j),i),spclst(j), 
     &                                        emsstr(:strmin(emsstr))
              else    
                  xcoord = xloc(i) 
                  ycoord = yloc(i) 
                  write(IOWELV,9001,ERR=7000) fiptmp, oplnt, 
     &                 ostck, -1.*plumemax, 1.0, 1.0, 
     &                -1.*plumemin, xcoord, ycoord, idate, spcrit(j),
     &                                emscrt(ispcrt(j),i),spclst(j), 
     &                                        emsstr(:strmin(emsstr))
              endif
            endif
c
  300     continue
c
c         --- write header records ---
            if( izone .GE. 0 ) then
               xcoord = xloc(i) * 1000
               ycoord = yloc(i) * 1000
               write(IOWHDR,9004,ERR=7001) keypts, fiptmp, oplnt, 
     &                     ostck,-1.*plumemax, 1.0, 1.0,
     &                                -1.*plumemin, xcoord, ycoord
            else
               xcoord = xloc(i)
               ycoord = yloc(i)
               write(IOWHDR,9005,ERR=7001) keypts, fiptmp, oplnt, 
     &                     ostck,-1.*plumemax, 1.0, 1.0,
     &                                -1.*plumemin, xcoord, ycoord
            endif

c     --- next stack ---
  200 continue
c
c     --- set error flag to sucess ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,A)',ERR=9999)
     &                       'ERROR:  Writing output emissions file.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',oelvfl(:strmin(oelvfl))
      goto 9999
c
 7001 continue
      write(ioerr,'(/,A)',ERR=9999)
     &                       'ERROR:  Writing output header file.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',ohdrfl(:strmin(ohdrfl))
      goto 9999
c
 7002 continue
      write(ioerr,'(/,A)',ERR=9999)
     &                       'ERROR:  Writing internal string.'
      write(ioerr,'(2A,I5,//)',ERR=9999) 'Module: wrtelvfir.f at ',ipos
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
 9000 format(3(A10,1X),F10.1,1X,F10.3,1X,
     &            F10.0,1X,F10.3,1X,2(F10.0,1X),I6,1X,A10,1X,
     &                                     e10.4,1X,A10,1X,A)
 9001 format(3(A10,1X),F10.1,1X,F10.3,1X,
     &            F10.0,1X,F10.2,1X,2(F10.4,1X),I6,1X,
     &                                        A10,1X,e10.4,1X,A10,1X,A)
c    &                                        A10,1X,F10.3,1X,A10,1X,A)
 9002 format(A1,I8.8)
 9003 format(A2,I3.3)
 9004 format(4(A10,1X),F10.1,1X,F10.3,1X,F10.0,1X,F10.2,1X,2(F10.0,1X))
 9005 format(4(A10,1X),F10.1,1X,F10.3,1X,F10.0,1X,F10.2,1X,2(F10.4,1X))
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
