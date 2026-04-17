C**** MKHASH_ASC.F 
c
      integer*4 function mkhash_asc( inasc )
c
c-----------------------------------------------------------------------
c
c   Description:
c
c     This routine creates the hash value for the ASC code passed in 
c    he argument list. 
c
c   Arguments:
c     Inputs:
c       inasc    C   ASC code
c     Return value:
c                I   the hash value
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Argument declaration:
c-----------------------------------------------------------------------
c
      character*(10) inasc
c
c-----------------------------------------------------------------------
c   Include files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'hash.inc'
c
c-----------------------------------------------------------------------
c   Local variables:
c-----------------------------------------------------------------------
c
      character*62 alphanum
      integer*4    i
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
      alphanum = '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
c
c  --- loop over the characters in the string ---
c
      mkhash_asc = 0
      do i=2,10
        mkhash_asc = mkhash_asc +
     &       (INDEX(alphanum,inasc(i:i))*(i-1)+
     &                            INDEX(alphanum,inasc(i-1:i-1)))*i
      enddo
c
c  --- make sure it is not out of bounds ---
c
      mkhash_asc = MIN( mkhash_asc, MXHASH_ASC )
      mkhash_asc = MAX( mkhash_asc, 0 )
c
c-----------------------------------------------------------------------
c   Format statements:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
