with Expectation_Maximization;
package String_Clustering is
  subtype Str_Vector is Expectation_Maximization.Str_Vector;

  procedure Run( Filename : in String; K : in Positive );

end String_Clustering;
