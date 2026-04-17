C**** RDEHDR.F 
c
      subroutine rdehdr( ierr,
     &                       iounit, ifile)
c
c-----------------------------------------------------------------------
c
c    Reads the header from a UAM emissions file.
c
c    Argument Declaration.
c     Outputs:
c       ierr    I  error indicator
c     Inputs:
c       iounit  I  unit number for reading file
c       ifile   I  number of file
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
      include 'iomrguam.inc'
      include 'mrguhdr.inc'
c
c-----------------------------------------------------------------------
c    Argument Declarations:
c-----------------------------------------------------------------------
c
      integer*4  ierr
      integer*4  iounit
      integer*4  ifile
c
c-----------------------------------------------------------------------
c   External functions:
c-----------------------------------------------------------------------
c
      integer*4 strmin
c
c-----------------------------------------------------------------------
c   Local Variables:
c-----------------------------------------------------------------------
c
      character*(MXSTR) action
      character*10      cname
      character*4       ifname(10), ifnote(60), iname(10,MXSPEC)
      integer*4         nseg, nspect, ichar, ispc, ihours, i
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- assume failure ---
c
      ierr = IFAIL
c
c   --- read the first four records ---
c
      action = 'Reading record with episode period.'
      rewind(iounit)
      read(iounit,ERR=7000) ifname, ifnote, nseg, nspect, idateb,
     &                      tbegti, idatee, tendti
      if( nspect .GT. MXSPEC) goto 7002
c
c   --- check that this inventory does not span more than a day
c
      if( tendti .EQ. 0 ) then
         idatee = idatee - 1
         tendti = 24.0
      endif
      ihours = (idatee-idateb)*24 + INT( (tendti-tbegti) )
      if( ihours .GT. MXHOUR ) goto 7001
c
c   --- set the species count ---
c
      iscnt(ifile) = nspect
c
c   --- record 2 ---
c
      action = 'Reading record with the domain definition.'
      read(iounit,ERR=7000) tutmx, tutmy, izonet, txorig, tyorig,
     &                      tdxcel, tdycel, nxcelt, nycelt, nzcelt,
     &                      nzlowt, nzuppt, thtsur, thtlow, thtupp
c
c  --- convert corrdinates to kilometers, for comparison to ASCII ---
c
      tutmx = tutmx / 1000.
      tutmy = tutmy / 1000.
      txorig = txorig / 1000.
      tyorig = tyorig / 1000.
      tdxcel = tdxcel / 1000.
      tdycel = tdycel / 1000.
c
c  --- record 3 ---
c
      action = 'Reading record with the segment identification.'
      read (iounit,ERR=7000) seg4
c
c  --- record 4 ---
c
      action = 'Reading record with the species list.'
      read (iounit,ERR=7000)
     &        ((iname(ichar,ispc),ichar=1,10),ispc=1,nspect)
c
c  --- convert species name from integer to character
c
      do ispc=1,nspect
          write(cname,'(10A1)') (iname(i,ispc),i=1,10)
          table(ifile,ispc) = cname
      enddo
c
      ierr=ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR:  Reading header of ',
     &                                        'input emissions file.'
      write(ioerr,'(A)',ERR=9999) action(:strmin(action))
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',
     &                              innm(ifile)(:strmin(innm(ifile)))
      goto 9999
c
 7001 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR:  Reading USERIN file.'
      write(ioerr,'(A)',ERR=9999) 'Number of hours exceeds max.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',
     &                              innm(ifile)(:strmin(innm(ifile)))
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Hours in episode: ',ihours
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maxumium allowed: ',MXHOUR
      write(ioerr,'(A,//)',ERR=9999)
     &                  'EPS does allows only a single day episode.'
      goto 9999
c
 7002 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR:  Reading header of ',
     &                                        'input emissions file.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',
     &                              innm(ifile)(:strmin(innm(ifile)))
      write(ioerr,'(A)',ERR=9999) 'Number of species exceeds max.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Species in file  : ',nspect
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maxumium allowed : ',MXSPEC
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXSPEC ',
     &                                                  'and recompile.'
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
