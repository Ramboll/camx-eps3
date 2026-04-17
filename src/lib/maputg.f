      subroutine maputg (x,y,z,lon,lat)

*     *** where
*           z     utm zone
*           x     utm easting in kilometers
*           y     utm northing in kilometers
*           lon   east longitude in decimal degrees
*           lat   north latitude in decimal degrees

      integer*4 z
      real*4 x,y,lon,lat

      real*4 k0,a,e1,e11,e12,e13,e14,e2,e4,e6,ep2,false_e,rtd
      parameter (k0=0.9996)
      parameter (a=6378137.0)      
      parameter (e1=0.001697916)
      parameter (e11=3.0*e1/2.0 - 27.0*e1*e1*e1/32.0)
      parameter (e12=21.0*e1*e1/16.0 - 55.0*e1*e1*e1*e1/32.0)
      parameter (e13=151.0*e1*e1*e1/96.0)
      parameter (e14=1097.0*e1*e1*e1*e1/512.0)
      parameter (e2=0.00676866)
      parameter (e4=e2*e2)
      parameter (e6=e2*e4)
      parameter (ep2=0.0068148)
      parameter (false_e=500000.0)
      parameter (rtd=180.0/3.141592654)

      real*4 xm,ym,m,u,p1,c1,t1,n1,r1,d
      real*4 cosp1,tanp1,sinp1,sin2p1
      real*4 t2,c2,r0,d2,d3,d4,d5,d6
      real*4 lon0,p,l

*     ---------------------------------------------------------------------

      lon0 = z*6.0 - 183.0

      xm = 1000.0*x - false_e
      ym = 1000.0*y

      if (ym.lt.1.d7) ym = ym + 1.d7
      m = ym/k0
      u = m/(a*(1.0-e2/4.0 - 3.0*e4/64.0 - 5.0*e6/256.0))
      p1 = u + e11*sin(2.0*u) + e12*sin(4.0*u) + e13*sin(6.0*u) +
     &         e14*sin(8.0*u)
      cosp1 = cos(p1)
      c1 = ep2*cosp1*cosp1
      c2 = c1*c1
      tanp1 = tan(p1)
      t1 = tanp1*tanp1
      t2 = t1*t1
      sinp1 = sin(p1)
      sin2p1 = sinp1*sinp1
      n1 = a/sqrt(1.0-e2*sin2p1)
      r0 = 1.0-e2*sin2p1
      r1 = a*(1.0-e2)/sqrt(r0*r0*r0)

      d = xm/(n1*k0)
      d2=d*d
      d3=d*d2
      d4=d*d3
      d5=d*d4
      d6=d*d5

      p = p1 - (n1*tanp1/r1) * (d2/2.0
     &       - (5.0+3.0*t1+10.0*c1-4.0*c2-9.0*ep2)*d4/24.0
     &       + (61.0+90.0*t1+298.0*c1+45.0*t2-252*ep2-3.0*c2)*d6/720.0)
      lat = rtd*p
      L = (d - (1.0+2.0*t1+c1)*d3/6.0
     &       + (5.0-2.0*c1+28*t1-3.0*c2+8.0*ep2+24.0*t2)*d5/120.0)/cosp1
      lon = rtd*l + lon0

      return
      end
