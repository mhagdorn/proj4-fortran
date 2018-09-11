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

projPJ cfort_pj_init_plus(char *args)
{
    projPJ prj;
    printf("Get projection defenition...\n");
    printf("%s\n", args);
    prj = pj_init_plus(args);
    printf("%s\n", prj->params->param);
    return prj;
}

int cfort_pj_transform_2(projPJ srcdefn, projPJ dstdefn,
                         long point_count, int point_offset,
                         double *x, double *y, double *z)
{
    int stat;
    printf("hello!\npoint_count: %ld\n", point_count);
    //printf("%s -> %s\n", srcdefn->params->param,
    //       dstdefn->params->param);
    //for (int i=0; i<point_count; i++) {
    //    printf("x: %f; y: %f; z: %f", x[i], y[i], z[i]);
    //}
    stat = pj_transform(srcdefn, dstdefn, point_count,
                        point_offset, x, y, z);
    return stat;
}

/*
 * error string
 */
FCALLSCFUN1(STRING,pj_strerrno,PRJF_STRERRNO,prjf_strerrno,INT);

/*
 * initialise projection structure
 */
#define prjf_init_STRV_A4 NUM_ELEM_ARG(2)
int cfort_pj_init(long *prj, char *args)
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
 * transform projection
 */
int cfort_pj_transform(long *prj_in,long *prj_out, double *x, double *y, int size)
{
  int status;
  double z[size];
  memset( z, 0, size*sizeof(double));
  //for (int i=0; i<size; i++){
  //  printf("lam: %f, phi: %f, z: %f\n", lam[i], phi[i], z[i]);
  //}

  //printf("%s -> %s\n", (*(projPJ *) prj_in)->params->param,
  //                     (*(projPJ *) prj_out)->params->param);

  status = pj_transform(*(projPJ *) prj_in, *(projPJ *) prj_out, size, 1, x, y, z);

  if (status != 0)
    return  pj_errno;
  else
    return 0;
}
FCALLSCFUN5(INT,cfort_pj_transform,PRJF_TRANSFORM,prjf_transform,
            PLONG,PLONG,PDOUBLE,PDOUBLE, INT);
