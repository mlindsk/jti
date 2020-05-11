#ifndef JTI_TYPES_H
#define JTI_TYPES_H

#include <numeric>       // For: 
#include <vector>        // For: 
#include <string>        // For:
#include <algorithm>     // For: sort, std set operations etc. 
#include <math.h>        // For: log
#include <map>           // For: count_unique
#include <unordered_map> // For: dfs, mcs, count_unique
#include <Rcpp.h>
// [[Rcpp::plugins(cpp11)]]
using namespace Rcpp;

using VI  = std::vector<int>;
using VS  = std::vector<std::string>;
using VVS = std::vector<std::vector<std::string>>;
using RL  = Rcpp::List;
using RIV = Rcpp::IntegerVector;
using RCV = Rcpp::CharacterVector;
using RCM = Rcpp::CharacterMatrix;

#endif
