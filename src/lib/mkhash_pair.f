C**** MKHASH_PAIR.F 
c
      integer*4 function mkhash_pair( infips, inasc )
c
c-----------------------------------------------------------------------
c
c   Description:
c
c     This routine creates the hash value for the FIPS code/ASC code
c     pair passed in the argument list. 
c
c   Arguments:
c     Inputs:
c       infips   C   FIPS code
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
      character*(5)  infips
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
      character*15 string
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
c  --- piece together the string ---
c
      string = infips // inasc
c
c  --- loop over the characters in the string ---
c
      mkhash_pair = 0
      do i=2,15
        mkhash_pair = mkhash_pair +
     &       (INDEX(alphanum,string(i:i))*(i-1)+
     &                            INDEX(alphanum,string(i-1:i-1)))*i
      enddo
c
c  --- make sure it is not out of bounds ---
c
      mkhash_pair = MIN( mkhash_pair, MXHASH_PAIR )
      mkhash_pair = MAX( mkhash_pair, 0 )
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
