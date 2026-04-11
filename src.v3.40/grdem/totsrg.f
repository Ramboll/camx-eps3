C**** TOTSRG.F 
c
      subroutine totsrg( ierr, 
     &                         idxtot, label, hash_label, emsarr )
c
c-----------------------------------------------------------------------
c
c     This routine adds the emissions to a the surrogate totals array at 
c     index given by the input arguments.  
c
c   Arguments:
c
c     Inputs:
c       ierr        I   error code
c       idxtot      I   code indicating input (=1) or output (=2)
c       label       C   label: fips//scc/srgcd/flag
c       hash_label  C   label: fips//scc/srgcd/flag (without spaces)
c       emsarr      R   array of criteria emissions to add to totals
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Include Files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'iogrdem.inc'
      include 'embrdat.inc'
      include 'grdsrgrpt.inc'
c
c-----------------------------------------------------------------------
c   Argument Declaration:
c-----------------------------------------------------------------------
c
      integer*4    ierr
      integer*4    idxtot
      real*4       emsarr(MXCRIT)
      character*22 label
      character*19 hash_label
c
c-----------------------------------------------------------------------
c    External Functions:
c-----------------------------------------------------------------------
c
c   fndhash_fssf   I   returns the index of the FSSF code in the hash table
c                      
      integer*4 fndhash_fssf
c
c-----------------------------------------------------------------------
c   Local Variables:
c-----------------------------------------------------------------------
c
      integer*4 idxs, jerr, i
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   ---- initialize the error code ---
c
      ierr = IFAIL
c
c   --- determine index to surrogate total arrays ---
c
      if( last_fssf .NE. label ) then
          idxs = fndhash_fssf( jerr, label, hash_label )
          if( jerr .NE. ISUCES ) goto 9999
          idx_last_fssf = idxs
          last_fssf = label
      else
         idxs = idx_last_fssf
      endif

c
c   ---- add emissions to criteria totals ---
c
      if ( idxtot .EQ. 1 )  then
        do i=1,ncrthd
         if( emsarr(i) .GT. 0. ) emsifssf(idxs,i) = emsifssf(idxs,i) +
     &                                                  DBLE(emsarr(i))
        enddo
      endif
c
      if ( idxtot .EQ. 2 )  then
        do i=1,ncrthd
         if( emsarr(i) .GT. 0. ) emsofssf(idxs,i) = emsofssf(idxs,i) +
     &                                                  DBLE(emsarr(i))
        enddo
      endif
c
c  --- set the error flag to sucess ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
