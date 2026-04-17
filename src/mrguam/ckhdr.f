C**** CKHDR.F 
c
      subroutine ckhdr( ierr,
     &                        ifile)
c
c-----------------------------------------------------------------------
c
c    Compare the header at the iounit to the userin info in the
c    common block.
c    Set up the index between species names in input files
c    and species names from the uamcntl.inc
c    Called from header.
c
c    Argument declaration.
c     Outputs:
c       ierr  I  error flag
c     Inputs:
c       ifile I  which file do we have
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'uamcntl.inc'
      include 'uamregn.inc'
      include 'mrguhdr.inc'
      include 'iomrguam.inc'
      include 'usrmrguam.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
      integer*4 ifile
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
c   strmin  I  returns the actual length of a string
c   fndchr  I  finds the index of a string in a list
c
      integer*4 strmin
      integer*4 fndchr
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
c    Local variables:
c-----------------------------------------------------------------------
c
c     inew    I  loop variable for reading this file's species names
c     rdiff   R  real difference
c
      integer*4    inew, idatmp, idareg, i
      real*4       rdiff, timtmp, timreg
      logical      lregn, lepis
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- assume failure ---
c
      ierr = IFAIL
      lregn = .TRUE.
      lepis = .TRUE.
c
c   --- compare region ---
c
      rdiff = ABS(utmx - tutmx)
      if(rdiff .GT. BFUZZ) lregn = .FALSE.
c
      rdiff = ABS(utmy - tutmy)
      if(rdiff .GT. BFUZZ) lregn = .FALSE.
c
      if(izone .NE. izonet) lregn = .FALSE.
c
      rdiff = ABS(xorig - txorig)
      if(rdiff .GT. BFUZZ) lregn = .FALSE.
c
      rdiff = ABS(yorig - tyorig)
      if(rdiff .GT. BFUZZ) lregn = .FALSE.
c
      if(nxcell .NE. nxcelt) lregn = .FALSE.
      if(nycell .NE. nycelt) lregn = .FALSE.
c
c   --- compare time, if beginning hour is 24, 
c       change day and set date to next day ---
c
      idatmp = idateb
      timtmp = tbegti
      if( timtmp .EQ. 24. ) then
          idatmp = idatmp + 1
          timtmp = 0.
      endif
c
      idareg = ibgdat
      timreg = begtim/100
      if( timreg .EQ. 24. ) then
          idareg = idareg + 1
          timreg = 0.
      endif
c
      if(idatmp .NE. idareg) lepis = .FALSE.
      rdiff = ABS(timtmp - timreg)
      if(rdiff .GT. BFUZZ) lepis = .FALSE.
c
c   --- if ending hour is 0, change day and set date to previous
c       day ---
c
      idatmp = idatee
      timtmp = tendti
      if( timtmp .EQ. 0. ) then
          idatmp = idatmp - 1
          timtmp = 24.
      endif
c
      idareg = iendat
      timreg = endtim/100
      if( timreg .EQ. 0. ) then
          idareg = idareg - 1
          timreg = 24.
      endif
c
      if(idatmp .NE. idareg) lepis = .FALSE.
      rdiff = ABS(timtmp - timreg)
      if(rdiff .GT. BFUZZ) lepis = .FALSE.
c
c   --- set up index for species names ---
c
      do inew=1,iscnt(ifile)
          idxspc(ifile,inew) = 
     &           fndchr( table(ifile,inew), 10, spcnam, nspecs )
      enddo
c
c   --- if the /REGION/ packet does not match,  write the
c       appropriate message ----
c
      if( .NOT. lregn ) then
c
c   --- if the check header flag is turned on this is an error ---
c
          if( lchkhd ) then
             write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Header read from ',
     &             'input file is not consistent with /REGION/ packet.'
             write(ioerr,'(2A)',ERR=9999) 'File: ',
     &                               innm(ifile)(:strmin(innm(ifile)))
c
c   --- if the check header flag is turned off 
c       this is just a warning ----
c
          else 
             write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
             write(ioerr,'(2A)',ERR=9999) 'WARNING: Header read ',
     &        'from input file is not consistent with /REGION/ packet.'
             write(ioerr,'(2A)',ERR=9999) 'File: ',
     &                               innm(ifile)(:strmin(innm(ifile)))
             nwarn = nwarn + 1
          endif
c 
c   --- echo the packet and header data ----
c
          write(ioerr,'(A)',ERR=9999) 'Input EMISSIONS header:'
          write(ioerr,9000,ERR=9999) 'UTM origin (km)',tutmx,tutmy
          write(ioerr,9001,ERR=9999) 'UTM zone',izonet
          write(ioerr,9000,ERR=9999) 'Grid origin (km)',txorig,tyorig
          write(ioerr,9000,ERR=9999) 'Grid cell width (km)',
     &                                                   tdxcel,tdycel
          write(ioerr,9002,ERR=9999) 'Number of cells',nxcelt,nycelt
          write(ioerr,'(/,A)',ERR=9999) '/REGION/ header:'
          write(ioerr,9000,ERR=9999) 'UTM origin (km)',utmx,utmy
          write(ioerr,9001,ERR=9999) 'UTM zone',izone
          write(ioerr,9000,ERR=9999) 'Grid origin (km)',xorig,yorig
          write(ioerr,9000,ERR=9999) 'Grid cell width (km)',
     &                                                 dxcell,dycell
          write(ioerr,9002,ERR=9999) 'Number of cells',nxcell,nycell
c
c   --- if flag for checking headers is turned on, then exit with
c       an error, otherwise continue processing ---
c
          if( lchkhd ) then
              goto 9999
          else
              write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
          endif
      endif
c
c   --- if the /DATE/ packet does not match, write the 
c       appropriate message ----
c
      if( .NOT. lepis ) then 
c
c   --- if the check header flag is turned on, this is an error ---
c
         if( lchkhd ) then 
             write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Header read from ',
     &             'input file is not consistent with /DATE/ packet.'
             write(ioerr,'(2A)',ERR=9999) 'File: ',
     &                               innm(ifile)(:strmin(innm(ifile)))
c
c   --- if the check header flag is turned off, this is only
c       a warning ---
c
         else
             write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
             write(ioerr,'(2A)',ERR=9999) 'WARNING: Header read ',
     &        'from input file is not consistent with /DATE/ packet.'
             write(ioerr,'(2A)',ERR=9999) 'File: ',
     &                               innm(ifile)(:strmin(innm(ifile)))
             nwarn = nwarn + 1
         endif
c
c   --- echo the packet data and header data ---
c
         write(ioerr,'(A)',ERR=9999) 'Input EMISSIONS header:'
         write(ioerr,9003,ERR=9999) 'File note',fnote(:strmin(fnote))
         write(ioerr,9001,ERR=9999) 'Beginning date',idateb
         write(ioerr,9004,ERR=9999) 'Beginning time',tbegti
         write(ioerr,9001,ERR=9999) 'Ending date',idatee
         write(ioerr,9004,ERR=9999) 'Ending time',tendti
c
         write(ioerr,'(/A)',ERR=9999) '/DATE/ header:'
         write(ioerr,9005,ERR=9999) 'Beginning date (Calander)',ibgcal
         write(ioerr,9001,ERR=9999) 'Beginning date (Julian)',ibgdat
         write(ioerr,9004,ERR=9999) 'Beginning time',begtim/100
         write(ioerr,9005,ERR=9999) 'Ending date (Calander)',iencal
         write(ioerr,9001,ERR=9999) 'Ending date (Julian)',iendat
         write(ioerr,9004,ERR=9999) 'Ending time',endtim/100
c
c   --- if flag for checking headers is turned on, then exit with
c       an error, otherwise continue processing ---
c
         if( lchkhd ) then
             goto 9999
         else
             write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         endif
       endif
c
c   --- set error flag to sucess ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Format statements:
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
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
