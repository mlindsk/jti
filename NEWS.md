## jti 0.7.0.9999 (xxxx-yy-zz)

 * A small bug fixed for working with undirected graphs
 * =new_mpd= renamed to =mpd=. Works on =cpt_list= objects now.
 * =triangulate= method for =cpt_list= objects.
 * A new heuristic, =evidence=, for triangulation.
 * Better performance when inserting evidence in general since redundant information is now removed.

## jti 0.7.0 (2021-04-03)

 * Evidence can now be entered in the compilation process.
 * Optimized propagation by avoiding all those unity tables. That is, when a message is sent to a unity table no multiplication is performed; the updated potential just becomes the message.
 * A procedure, =mpd= for finding maximal prime decompositions is now include included
 * It is now possible to =triangulate= a graph before compilation in order to investigate the size of the cliques etc.
 * Triangulation is orders of magnitudes faster now.

## jti v0.6.0 (2020-12-16)

 * There was a bug in the creation of the junction tree when calling Kruskals algorithm.
 
 * It is now possible to specify variables of interest in advance, such that we are 
 guaranteed to be able to query the joint pmf of these variables.
 * Some refactoring making compilation much faster. When potentials is assigned to
 a clique we no longer start by creating a unity table and then multiply. This was killing
 the advantage of the sparsity.
 
## jti v0.5.2 (2020-11-24)

 * A new way of entering evidence that is much more robust
 
## jti v0.5.1 (2020-11-13)

 * A more optimal triangulation method has been implemented, which in general leads to faster run time of the **jt** function.
 * Some mis-spelled words fixed

## jti v0.5.0 (2020-11-09)

 * Initial version
