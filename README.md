jti: Junction Tree Inference
================

The **jti** package (pronounced 'yeti') is a fast implementaion of the junction tree algorithm (JTA) using the Lauritzen-Spiegelhalter scheme. Why is it fast? Because we use a sparse representation for the potentials which enable us to handle large and complex graphs where the variables can have an arbitrary large number of levels.
