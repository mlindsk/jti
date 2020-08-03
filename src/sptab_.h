#ifndef SPTAB_H
#define SPTAB_H

#include "jti_types.h"

VS  matpr(Rcpp::CharacterMatrix A, bool validate);
RE  sptab_(RCM & A, bool validate);
RE  count_unique(VS x);

#endif
