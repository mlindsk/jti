#ifndef SPTAB_H
#define SPTAB_H

#include "jti_types.h"

VS  matpr(Rcpp::CharacterMatrix A);
RIV sptab_(RCM & A);
RE  sptab_env_(RCM & A);
RE  count_unique_env(VS x);

#endif
