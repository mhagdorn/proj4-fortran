/*
 Copyright 2004, Magnus Hagdorn
 
 This file is part of proj4.
 
 proj4 is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2 of the License, or
 (at your option) any later version.
 
 proj4 is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.
 
 You should have received a copy of the GNU General Public License
 along with proj4; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#include <projects.h>
#include "cfortran.h"

/*
 * error string
 */
FCALLSCFUN1(STRING,pj_strerrno,PRJF_STRERRNO,prjf_strerrno,INT);

/*
 * initialise projection structure
 */
#define prjf_init_STRV_A4 NUM_ELEM_ARG(2)
int cfort_pj_init(long *prj, const char *args)
{
  *prj = (long) pj_init_plus(args);
  if (!*prj)
    return pj_errno;
  else
    return 0;
}
FCALLSCFUN2(INT,cfort_pj_init,PRJF_INIT,prjf_init, PLONG, STRING);

/*
 * free projection structure
 */
int cfort_pj_free(long *prj)
{
  pj_free(*(projPJ *) prj);
  return 0;
}
FCALLSCFUN1(INT,cfort_pj_free, PRJF_FREE, prjf_free, PLONG);

/*
 * forward transform
 */
int cfort_pj_fwd(long *prj, double lam, double phi, double *x, double *y)
{
  int status;
  double *z;
  projPJ geographic_latlon;

  geographic_latlon = pj_init_plus("+proj=latlong +ellps=WGS84 +datum=WGS84");

  *x = lam * DEG_TO_RAD;
  *y = phi * DEG_TO_RAD;
  *z = 0;
  /*
  printf("%s -> %s\n", geographic_latlon->params->param,
                       (*(projPJ *) prj)->params->param);
  */
  status = pj_transform(geographic_latlon, *(projPJ *) prj, 1, 1, x, y, z);

  pj_free(geographic_latlon);

  if (status != 0 || (*x==HUGE_VAL && *y==HUGE_VAL))
    return  pj_errno;
  else
    return 0;
}
FCALLSCFUN5(INT,cfort_pj_fwd,PRJF_FWD,prjf_fwd,PLONG,DOUBLE,DOUBLE,PDOUBLE,PDOUBLE);

/*
 * inverse transform
 */
int cfort_pj_inv(long *prj, double x, double y, double *lam, double *phi)
{
  int status;
  double *x_tmp;
  double *y_tmp;
  double *z;
  projPJ geographic_latlon;

  geographic_latlon = pj_init_plus("+proj=latlong +ellps=WGS84 +datum=WGS84");
 
  *lam = x;
  *phi = y;
  *z = 0;
  /*
  printf("%s -> %s\n", (*(projPJ *) prj)->params->param,
                        geographic_latlon->params->param);
  */
  status = pj_transform(*(projPJ *) prj, geographic_latlon, 1, 1, lam, phi, z);

  pj_free(geographic_latlon);

  *lam *= RAD_TO_DEG;
  *phi *= RAD_TO_DEG;

  if (status != 0 || (*lam==HUGE_VAL && *phi==HUGE_VAL))
    return  pj_errno;
  else
    return 0;
}
FCALLSCFUN5(INT,cfort_pj_inv,PRJF_INV,prjf_inv,PLONG,DOUBLE,DOUBLE,PDOUBLE,PDOUBLE);
