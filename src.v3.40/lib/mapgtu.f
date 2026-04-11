      subroutine mapgtu (lon,lat,z0,z,x,y)

*     *** where
*           lon   e longitude in decimal degrees
*           lat   n latitude in decimal degrees
*           z0    utm zone override if not zero
*           z     returned utm zone
*           x     returned utm easting in kilometers
*           y     returned utm northing in kilometers

      real*4 lon,lat,x,y
      integer*4 z0,z

      real*4 k0,a,e2,ep2,false_e,dtr
      parameter (k0=0.9996)
      parameter (a=6378137.0)
      parameter (e2=0.00676866)
      parameter (ep2=0.0068148)
      parameter (false_e=500000.0)
      parameter (dtr=3.141592654/180.0)

      real*4 dl,p,n,t,c,a1,m
      real*4 sinp,tanp,cosp
      real*4 a2,a3,a4,a5,a6,t2

*     ---------------------------------------------------------------------

      if (z0 .EQ. 0) then
          z = INT((180.0+lon)/6.0) + 1
      else
          z = z0
      endif

      dl = dtr*(lon - (6.0*z-183.0))

      p = dtr*lat

      sinp = sin(p)
      n = a/sqrt(1.0-e2*sinp*sinp)
      tanp = tan(p)
      t = tanp*tanp
      cosp = cos(p)
      C = ep2*cosp*cosp
      a1 = dl*cosp
      m = 111132.0894*lat - 16216.94*sin(2.0*p) + 17.21*sin(4.0*p)
     &  - 0.02*sin(6.0*p)

      a2 = a1*a1
      a3 = a2*a1
      a4 = a2*a2
      a5 = a4*a1
      a6 = a4*a2
      t2 = t*t

      x = 0.001*(k0*n*(a1+(1.0-t+c)*a3/6.0
     &  + (5.0-18.0*t+t2+72.0*c-58.0*ep2)*a5/120.0)
     &  + false_e)
      y = 0.001*k0*(m+n*tanp * (a2/2.0
     &  + (5.0-t+9.0*c+4.0*c*c)*a4/24.0
     &  + (61.0-58.0*t+t2+600.0*c-330.0*ep2)*a6/720.0))

      if (lat.lt.0) y = y - 1.D4
      return
      end
