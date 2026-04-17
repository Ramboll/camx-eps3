      subroutine beh072(hf,hx,hmw,f,delhf,distf,delhx,hp,ts,vs,d,vf,
     &                        kst,u,x,dthdz,t,p)
c
c-----------------------------------------------------------------------
c
c        beh072   (briggs effective height)   september 79
c         this differs from the oct. 1972 version in statement 24 + 1:
c          the constant 2.6 previously was 2.4.
c
c        d. b. turner, research meteorologist* model development branch,
c         division of meteorology, environmental protection agency.
c           room 314b, nchs building, rtp. phone (919) 549-8411 ext 4564
c          mailing address- dm, epa, research triangle park, nc 27711
c         * on assignment from national oceanic and atmospheric
c               administration, department of commerce.
c         from a single source is based on:
c        this version of briggs effective height to calculate plume rise
c           1) briggs,gary a., 1971: some recent analyses of plume rise
c               observation. pp 1029 - 1032 in proceedings of the second
c               international clean air congress, edited by h. m. englun
c               and w. t. beery. academic press, new york.
c           2) briggs, gary a.,1972: discussion on chimney plumes in
c               neutral and stable surroundings. atmos. environ. 6, 507
c               - 510. (jul 72).
c      argument description:
c         outputs:
c           hf    final effective plume height (meters)
c           hx    effective plume height for distance x (meters)
c           hmw   heat output of source (mw)
c           f     buoyancy flux (m**4/sec**3)
c           delhf final plume rise (meters)
c           distf distance of final plume rise from source (km)
c           delhx plume rise at distance x (meters)
c         inputs:
c           hp    physical stack height (meters)
c           ts    stack gas temperature (deg k)
c           vs    stack gas exit velocity (m/sec)
c           d     inside stack diameter (meters)
c           vf    stack gas volumetric flow rate (m**3/sec)
c           kst   stability (class), see page 209 of pasquill,
c                  atmospheric dispersion.  classes defined by...
c                    1 is pasquill stability class a
c                    2 is pasquill stability class b
c                    3 is pasquill stability class c
c                    4 is pasquill stability class d
c                    5 is pasquill stability class e
c                    6 is pasquill stability class f
c           u     wind speed (m/sec)
c           x     downwind distance (km)
c           dthdz potential temperature lapse rate (deg k/meter)
c           t     ambient air temperature (deg k)
c           p     ambient air pressure (mb)
c        thanks to dale coventry for his helpful discussion on
c         programming plume rise, to roger thompson for the comment
c         cards, and to russ lee who revised this according to reference
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Argument Declarations:
c-----------------------------------------------------------------------
c
      real*4    hf
      real*4    hx
      real*4    hmw
      real*4    f
      real*4    delhf
      real*4    distf
      real*4    delhx
      real*4    hp
      real*4    ts
      real*4    vs
      real*4    d
      real*4    vf
      integer*4 kst
      real*4    u
      real*4    x
      real*4    dthdz
      real*4    t
      real*4    p
c
c-----------------------------------------------------------------------
c   Local Variables:
c-----------------------------------------------------------------------
c
      real*4  xst, xm, s, dha
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
      if( t ) 1,1,2
c
c   ---- t = 0. means no ambient temperature given.  use t = 293. ----
c
    1 t = 293.
    2 if( p ) 3,3,4
c
c   ---- p = 0. means no ambient air pressure given.  use p = 960. ----
c
    3 p = 960.
c
c   ---- if vf is not given, calculate it from stack data. ----
c
    4 if( vf ) 5,5,6
    5 vf = 0.785398*vs*d*d
c
c   ---- the constant 0.785398 = pi/4 ----
c
    6 f = 3.12139*vf*(ts-t)/ts
c
c   ---- the constant 3.12139 is the acceleration due to gravity / pi.
c
      hmw = 0.00011217*f*p
c
c   ---- the constant 0.00011217 = pi times the specific heat of ----
c        air at constant pressure (0.24 cal/gm*deg k) times
c        molecular weight of air (28.966 gm/gm.mole) divided by
c        ideal gas constant (0.0831 mb*m**3/gm.mole*deg k) and
c        acceleration due to gravity (9.80616 m/sec*sec) and
c        then multiplied by (4.1855e-06 mw/cal per sec) to convert
c        the answer to megawatts. go to appropriate branch for
c        stability condition given if unstable or neutral go to 7,
c        if stable go to 20. ----
c
      goto (7,7,7,7,20,20),kst
c
c   ---- determine appropriate formula for calculating xst,
c        distance at which turbulence begins to dominate.  the
c        formula used depends upon buoyancy flux. statements 8
c        and 9 are equation (7). ----
c
7     if( f-55. ) 8,9,9
8     xst=14.*f**.625
      goto 10
9     xst=34.*f**.4
10    distf=3.5*xst
      delhf=1.6*f**0.333333*distf**0.666667/u
      if( x ) 29,29,32
c
c   ---- if x = 0.0, calculate final rise only, if x is greater
c        than 0.0, calculate rise for distance = x also. ----
c
   32 xm = 1000.* x
c
c   ---- xm is x in meters.
c        statement 14 is equation (6), reference 1. ----
c
   14 delhx = 1.6*f**0.333333*xm**0.666667/u
      if( delhx .GT. delhf ) delhx=delhf
      goto 30
   20 if( dthdz ) 21,21,24
c
c   ---- if dthdz is negative or zero assign to it a value of
c        0.02 or 0.035 if stability is slightly stable or stable,
c        respectively. ----
c
   21 go to (7,7,7,7,22,23),kst
   22 dthdz = 0.02
      go to 24
   23 dthdz = 0.035
   24 s = 9.80616*dthdz/t
c
c   ---- the constant 9.80616 is the acceleration due to gravity.
c        s is a stability parameter.
c        calculate plume rise according to equation (4), reference 1. ----
c
      dha = 2.6*(f/(u*s))**0.333333
c
c   ---- calculate plume rise by equation (5), reference 1 for light
c        wind conditions according to morton, taylor, and turner. ----
c
      delhf = 5.0*f**0.25/s**0.375
      if(dha-delhf) 25,25,27
   25 delhf = dha
c
c   ---- distance to final plume rise is given by the following ----
c
   27 distf = 3.14159*u/s**0.5
c
c   ---- if x = 0.0, calculate final rise only, if x is greater than
c         0.0, calculate rise for distance = x also.
c        if x is zero or less, go to 29 and set plume rise and dist. ----
c        to maximum plume rise equal to zero. ----
c
      if( x ) 29,29,33
   33 xm = 1000.*x
c
c   ---- xm is x in meters.
c        if xm is greater than the distance to the point of final
c        plume rise, set plume rise equal to final plume rise,
c        otherwise calculate plume rise from equation (6),reference 1.
c
      if( xm-distf ) 14,14,28
   28 delhx = delhf
      go to 30
   29 delhx = 0.
      hx = 0.
      go to 31
c
c   ---- calculate effective height at distance x. ----
c
   30 hx = hp + delhx
c
c   ---- calculate final effective height. ----
c
   31 hf = hp + delhf
      distf = distf/1000.
c
c   ---- return point ----
c
      return
      end
