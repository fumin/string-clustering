with Expectation_Maximization;
package String_Clustering is
  subtype Str_Vector is Expectation_Maximization.Str_Vector;

  procedure Run( Filename              : in String;
                 K                     : in Positive;
                 Recompute_Dist_Matrix : in Boolean );

end String_Clustering;
