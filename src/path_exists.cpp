// #include "jti_types.h"

// // [[Rcpp::export]]
// bool dfs_path_exists(Rcpp::IntegerMatrix g, int start, int end) {

//   // TODO: HOW TO TEST THE NEIBORS FIRST!?
//   // Compensate for the R side.
//   start -= 1;
//   end   -= 1; 
  
//   int N = g.ncol();
//   std::vector<int> nodes(N);
//   std::iota(std::begin(nodes), std::end(nodes), 0); // Fill with 0, 1, ..., N-1.

  
//   int n = nodes.size();
//   std::unordered_map<int, int> visited;
  
//   for (int i = 0; i < N; i++) {
//     visited.emplace(nodes[i], false); 
//   }

//   std::stack<int> S;
//   S.push(start);
  
//   while (!S.empty()) {
//     int u = S.top();
//     S.pop();
//     if (!visited[u]) {
//       visited[u] = true;
//       if (u == end) return true;

//       std::vector<int> adj_u;
//       auto u_col = g(_, u);
//       for (int i = 0; i < N; i++) {
// 	if (u_col[i]) adj_u.push_back(i);
//       }
      
//       for (auto & w : adj_u) {
//       	if (!visited[w]) {
//       	  S.push(w);
//       	}
//       }
//     }
//   }
//   return false;
// }


// // [[Rcpp::export]]
// bool bfs_path_exists(Rcpp::IntegerMatrix g, int start, int end) {

//   start -= 1;
//   end   -= 1; 
  
//   int N = g.ncol();
//   std::vector<int> nodes(N);
//   std::iota(std::begin(nodes), std::end(nodes), 0); // Fill with 0, 1, ..., N-1.

  
//   int n = nodes.size();
//   std::unordered_map<int, bool> visited;
  
//   for (int i = 0; i < N; i++) {
//     visited.emplace(nodes[i], false); 
//   }

//   std::queue<int> Q;
//   Q.push(start);
//   visited[start] = true;
  
//   while (!Q.empty()) {
//     int u = Q.front();
//     Q.pop();
//     if (u == end) return true;

//     std::vector<int> adj_u;
//     auto u_col = g(_, u);
//     for (int i = 0; i < N; i++) {
//       if (u_col[i]) adj_u.push_back(i);
//     }

//     for (auto & w : adj_u) {
//       if (!visited[w]) {
// 	visited[w] = true;
// 	Q.push(w);
//       }
//     }
//   }
//   return false;
// }
