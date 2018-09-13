! Copyright 2004, Magnus Hagdorn
! 
! This file is part of proj4.
! 
! proj4 is free software; you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation; either version 2 of the License, or
! (at your option) any later version.
! 
! proj4 is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
! 
! You should have received a copy of the GNU General Public License
! along with proj4; if not, write to the Free Software
! Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

program testproj
  use proj4
  implicit none

  integer :: status
  type(prj90_projection) :: proj
  character(len=256) :: params
  real(kind=kind(1.0d0)) :: lam0,phi0,x0,y0
  real(kind=kind(1.0d0)) :: lam1(2),phi1(2),x1(2),y1(2)
  type(prj90_projection) :: srcdefn, dstdefn

  params = '+proj=aea '//&
           '+ellps=WGS84 '//&
           '+lat_1=52.8333320617676 '//&
           '+lat_2=68.1666641235352 '//&
           '+lon_0=33.5'//&
           '+lat_0=60.5 '//&
           '+x_0=1903970.98145531 '//&
           '+y_0=898179.31322811'

  status=prj90_init(proj,params)
  if (status.ne.PRJ90_NOERR) then
     print*, prj90_strerrno(status)
     stop
  end if

  lam0 = 7.0
  phi0 = 49.0
  x0 = 0
  y0 = 0
  status = prj90_fwd_pt(proj, lam0, phi0, x0, y0)
  if (status.ne.PRJ90_NOERR) then
     print*, prj90_strerrno(status)
     stop
  end if  
  print*, lam0, phi0, x0, y0
  lam0 = 0
  phi0 = 0
  status = prj90_inv(proj, x0, y0, lam0, phi0)
  if (status.ne.PRJ90_NOERR) then
     print*, prj90_strerrno(status)
     stop
  end if    
  print*, x0, y0, lam0, phi0

  lam1 = [59.92093, 60.92093]
  phi1 = [71.9509, 72.9509]
  x1 = [0, 0]
  y1 = [0, 0] 
  status = prj90_fwd(proj, lam1, phi1, x1, y1)
  if (status.ne.PRJ90_NOERR) then
     print*, prj90_strerrno(status)
     stop
  end if  
  print*, lam1, phi1, x1, y1
  lam1 = [0, 0]
  phi1 = [0, 0]

  srcdefn = proj
  status=prj90_init(dstdefn,"+proj=latlong +ellps=WGS84 +datum=WGS84")
  if (status.ne.PRJ90_NOERR) then
     print*, prj90_strerrno(status)
     stop
  end if

  status = prj90_transform_array(srcdefn, dstdefn ,x1, y1, lam1, phi1)
  if (status.ne.PRJ90_NOERR) then
     print*, prj90_strerrno(status)
     stop
  end if    
  print*, x1,y1,lam1*RAD2DEG,phi1*RAD2DEG

  status = prj90_free(proj)

end program testproj
