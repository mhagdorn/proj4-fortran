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

  integer status
  type(prj90_projection) :: proj
  character(len=256) :: params
  real(kind=kind(1.0d0)) :: lam0,phi0,x0,y0,lam1(2),phi1(2),x1(2),y1(2)

  params = '+proj=aea '//&
           '+ellps=WGS84 '//&
           '+lat_1=52.8333320617676 '//&
           '+lat_2=68.1666641235352 '//&
           '+lon_0=33.5'//&
           '+lat_0=60.5 '//&
           '+x_0=1903970.98145531 '//&
           '+y_0=898179.31322811 '

  status=prj90_init(proj,params)
  if (status.ne.PRJ90_NOERR) then
     write(*,*) prj90_strerrno(status)
     stop
  end if

  lam0 = 7.0
  phi0 = 49.0
  status = prj90_fwd(proj,lam0,phi0,x0,y0)
  if (status.ne.PRJ90_NOERR) then
     write(*,*) prj90_strerrno(status)
     stop
  end if  
  write(*,*) lam0,phi0,x0,y0
  status = prj90_inv(proj,x0,y0,lam0,phi0)
  if (status.ne.PRJ90_NOERR) then
     write(*,*) prj90_strerrno(status)
     stop
  end if    
  write(*,*) x0,y0,lam0,phi0

  lam1 = [59.92093, 60.92093]
  phi1 = [71.9509, 72.9509]
  status = prj90_fwd(proj,lam1,phi1,x1,y1)
  if (status.ne.PRJ90_NOERR) then
     write(*,*) prj90_strerrno(status)
     stop
  end if  
  write(*,*) lam1,phi1,x1,y1
  status = prj90_inv(proj,x1,y1,lam1,phi1)
  if (status.ne.PRJ90_NOERR) then
     write(*,*) prj90_strerrno(status)
     stop
  end if    
  write(*,*) x1,y1,lam1,phi1

  status = prj90_free(proj)

end program testproj
