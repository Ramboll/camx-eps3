C**** MKHASH_FSSF.F 
c
      integer*4 function mkhash_fssf( fssfcd )
c
c-----------------------------------------------------------------------
c
c   Description:
c
c     This routine creates the hash value for the combinnation code
c     passed in the argument list. The code is a concatonated string
c     of FIPS code(F), Source category code(S), Surrogate code(S) 
c     and surrogate flag(F).
c
c   Arguments:
c     Inputs:
c       fssfcd    C   FSSF code
c     Return value:
c                 I   the hash value
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Argument declaration:
c-----------------------------------------------------------------------
c
      character*(19) fssfcd
c
c-----------------------------------------------------------------------
c   Include files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'grdsrgrpt.inc'
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
      data alphanum /'0123456789abcdefghijklmnopqrstuvwxyz
     &ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c  --- loop over the characters in the string ---
c
      do i=2,19
        mkhash_fssf = mkhash_fssf +
     &       (INDEX(alphanum,fssfcd(i:i))*(i-1)+
     &                            INDEX(alphanum,fssfcd(i-1:i-1)))*i
      enddo
c
c  --- make sure it is not out of bounds ---
c
      mkhash_fssf = MIN( mkhash_fssf, MXHASH_FSSF )
      mkhash_fssf = MAX( mkhash_fssf, 0 )
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
