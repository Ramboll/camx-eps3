C**** CMPSPC.F 
c

      subroutine cmpspc( ierr )
c
c-----------------------------------------------------------------------
c
c    Compares the species list in the splits file with the
c    species list in the USERIN file.  Will also check the
c    species in the /DEFAULT SPLITS/ packet for matches.
c
c    Argument Declaration.
c     Inputs:
c     Outputs:
c       ierr  I error flag
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
      include 'embrdat.inc'
      include 'sccxref.inc'
      include 'splits.inc'
      include 'chmspl.inc'
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
c   fndchr   I  returns the index of a string in an array
c   strmin   I  returns the actual lenght of a string (min of 1)
c
      integer*4 fndchr
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
      integer*4 idx, i, j
      logical*4 lwarn
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      lwarn = .FALSE.
c
c   --- initialize the arrays ----
c
      do i=1,MXSPEC
         indspl(i) = 0
      enddo
c
c   --- first check that every species in the /SPECIES LIST/ packet
c       is in the splitting factors
c
      do i=1,nspecs
          idx = fndchr( spcnam(i),10,splnam,nsplit )
c
c   --- if not found, check for species in /DEFAULT SPLITS/ packet ---
c
          if( idx .LE. 0 ) then
             idx = fndchr( spcnam(i),10,dftlst,ndftspl )
             if( idx .LE. 0 ) goto 7000
          else
             indspl(i) = idx
          endif
      enddo
c
c   --- now check that every species in the splits file
c       is in the /SPECIES LIST/ packet.  The handling of this error
c       is determined by the overide variable from chmspl.inc ---
c
      do i=1,nsplit
          idx = fndchr ( splnam(i),10,spcnam,nspecs )
          if( idx .LE. 0 ) then
c
c   --- split species not in control packet, could be an error ---
c
             if( .NOT. lovrid ) goto 7001
c
c   --- or could be a warning ---
c
             if( .NOT. lwarn ) then
                write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',j=1,40)
                write(ioerr,'(3A)',ERR=9999) 'WARNING:  Some ',
     &                        'species in splits file are not found ',
     &                                      'in /SPECIES LIST/ packet.'
                write(ioerr,'(10X,2A)',ERR=9999) 'Species: ',
     &                                  splnam(i)(:strmin(splnam(i)))
                write(ioerr,'(A)',ERR=9999) 
     &                 'A loss in total mass of emissions may result.'
                write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',j=1,40)
                nwarn = nwarn + 1
             endif
             lwarn = .TRUE.
          endif
      enddo
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,3A)',ERR=9999) 
     &                          'ERROR:  Species in /SPECIES LIST/ ',
     &                      'packet is not found in splits file or ',
     &                                      '/DEFAULT SPLITS/ packet.'
      write(ioerr,'(10X,2A)',ERR=9999) 'Species: ',
     &                                  spcnam(i)(:strmin(spcnam(i)))
      write(ioerr,'(2A,//)',ERR=9999) 'Either provide splits data ',
     &     'for this species or remove it from /SPECIES LIST/ packet.'
      goto 9999
c
 7001 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR:  Species in splits ',
     &                   'file is not found in /SPECIES LIST/ packet.'
      write(ioerr,'(10X,2A)',ERR=9999) 'Species: ',
     &                                  splnam(i)(:strmin(splnam(i)))
      write(ioerr,'(2A)',ERR=9999) 
     &                    'Either add species to /SPECIES LIST/ ',
     &                     'packet or remove it from the splits file.'
      write(ioerr,'(2A,//)',ERR=9999) 'Alternatively, set the flag ',
     &    'for using inconsistent species list in the /CHMSPL/ packet.'
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
