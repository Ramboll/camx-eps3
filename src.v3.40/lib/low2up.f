C**** LOW2UP.F 
c
      subroutine low2up( string )
c
c-----------------------------------------------------------------------
c
c     This routine converts a string from lower case to upper case.
c
c   Arguments:
c
c     Inputs/Outputs: (serves as both input and output)
c       string   C   string to convert
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Argument declaration:
c-----------------------------------------------------------------------
c
      character*(*) string
c
c-----------------------------------------------------------------------
c   Local variables:
c-----------------------------------------------------------------------
c
      integer*4 idiff, i
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   ---- check from beninng of string to end for non-blank ----
c
      do i=1,LEN(string)
        idiff = ichar( string(i:i) ) - ichar( 'a' )
        if( idiff .GE. 0 .AND. idiff .LE. 26 ) then
            string(i:i) = char( ichar( 'A' ) + idiff )
        endif
c
      enddo
c
      goto 9999
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
