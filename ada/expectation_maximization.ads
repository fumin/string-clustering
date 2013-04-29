with Ada.Strings.Unbounded;
with Thread_Safe_Vector;
package Expectation_Maximization is
  type Distance_Type is range 0..255;
  package Dist_Vector_Pack is new Thread_Safe_Vector(Element_Type => Distance_Type);
  subtype Dist_Vector is Dist_Vector_Pack.Vector;
  package Distance_Matrix_Pack is new Thread_Safe_Vector(Element_Type => Dist_Vector);
  type Distance_Matrix is new Distance_Matrix_Pack.Vector with null record;
  procedure Set( V : in out Distance_Matrix;
                 X : in Positive;
                 Y : in Positive;
                 N : in Distance_Type );
  function Get(  V : in Distance_Matrix;
                 X : in Positive;
                 Y : in Positive) return Distance_Type;

  package Positive_Vector_Pack is new Thread_Safe_Vector(Element_Type => Positive);
  subtype Positive_Vector is Positive_Vector_Pack.Vector;

  -- Vector of Strings Type
  package Str_Vector_Pack is new Thread_Safe_Vector(
                                   Element_Type => Ada.Strings.Unbounded.Unbounded_String);
  type Str_Vector is new Str_Vector_Pack.Vector with null record;
  procedure Set( V : in out Str_Vector; Idx : in Positive; S : in String );
  function Get( V : in Str_Vector; Idx : in Positive ) return String;

  procedure Read_File_Lines( Filename : in String;
                             Data     : out Str_Vector );

  -- Here Dist_Matrix is expected to be a vector of vectors of length N,
  -- where N is the size of the data set. For each row I of Dist_Matrix,
  -- the size is (I-1). For example, for the size of the first row is 0,
  -- and the size of the last row is (N-1).
  -- Conceptually speaking, Dist_Matrix is a triangular matrix with its
  -- upper part, including diagonals, zero.
  -- The function `Read_Distance_Matrix` is provided here to make querying the
  -- Dist_Matrix easier.
  --
  -- Example:
  -- If the data set is (0, 0), (3, 4), (3, 0),
  -- then the Distance_Matrix would be
  -- [[],
  --  [5],
  --  [3, 4]].
  --
  function Read_Distance_Matrix( A_Idx : in Positive;
                                 B_Idx : in Positive;
                                 Dists : in Distance_Matrix) return Distance_Type;
  procedure Dump_Distance_Matrix_To_Disk( Filename : in String;
                                          Dists    : in Distance_Matrix);
  procedure Load_Distance_Matrix_From_Disk( Filename : in String;
                                            Dists    : out Distance_Matrix);

  procedure E_Step( Dist_Matrix    : in Distance_Matrix;
                    Centroids      : in Positive_Vector;
                    Classification : out Positive_Vector;
                    Variance       : out Natural);
  function M_Step( Classification : in Positive_Vector;
                   K              : in Positive;
                   Dist_Matrix    : in Distance_Matrix) return Positive_Vector;
  procedure EM( K              : in Positive;
                Dist_Matrix    : in Distance_Matrix;
                Number_Of_Runs : in Positive;
                Centroids      : out Positive_Vector;
                Classification : out Positive_Vector;
                Variance       : out Natural);
  procedure Export_Results( Filename       : in String;
                            Centroids_Strs : in Str_Vector;
                            Classification : in Positive_Vector;
                            Variance       : in Natural);

end Expectation_Maximization;
