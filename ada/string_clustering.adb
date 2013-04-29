with Ada.Strings.Unbounded;
with Ada.Text_IO;
with String_Distance;
package body String_Clustering is
  -------------------------------------------------
  -- Compute_Distance_Matrix computes a Distance_Matrix according to the data.
  -- Dist_From_Cache returns the distance of two Strings (identified by their indices)
  -- by reading from a Distance_Matrix.
  -------------------------------------------------
  procedure Compute_Distance_Matrix(
    Data   : in Str_Vector;
    Dists  : out Expectation_Maximization.Distance_Matrix;
    Number_Of_Cores : in Positive;
    Metric : access function(A_String : String;
                             B_String : String)
                    return Expectation_Maximization.Distance_Type) is

    task type Compute_Dist(Modulus : Positive) is
      entry Start( The_Remainder : in Natural );
    end Compute_Dist;
    task body Compute_Dist is
      Dummy : Expectation_Maximization.Dist_Vector;
      Switch_Remainder_Threashold : Positive := (((Data.Size / 2) / Modulus) + 1) * Modulus;
      Remainder : Natural;
    begin
      accept Start( The_Remainder : in Natural ) do
        Remainder := The_Remainder;
      end Start;

      for I in 1..Data.Size loop
        -- Allocate space for this row and compute the distances
        if Remainder = (I mod Modulus) then
          Dummy.Alloc(I-1);
          Dists.Set(I, Dummy);
          for J in 1..(I-1) loop
            Dists.Set( I, J, Metric(Data.get(I), Data.get(J)) );
          end loop;
        end if;

        -- Switch the Remainder so that the first task won't be
        -- constantly computing for shorter rows than the last task
        if I = Switch_Remainder_Threashold then
          Remainder := (Modulus - Remainder + 1) mod Modulus;
        end if;

        -- Log onto screen
        if 0 = (I mod 1000) then
          Ada.Text_IO.Put_Line("Task" & Integer'Image(Remainder) & ": " & Integer'Image(I) );
        end if;
      end loop;
    end Compute_Dist;
    Tasks : array(1..Number_Of_Cores) of Compute_Dist(Modulus => Number_Of_Cores);
  begin
    Dists.Alloc(Data.Size);
    for I in 1..Number_Of_Cores loop
      Tasks(I).Start( The_Remainder => (I mod Number_Of_Cores) );
    end loop;
  end Compute_Distance_Matrix;

  function Lev_Dist( A : String; B : String) return Expectation_Maximization.Distance_Type is
  begin
    return Expectation_Maximization.Distance_Type(String_Distance.Lev_Dist(A, B));
  end Lev_Dist;

  procedure Run( Filename : in String; K : in Positive; Recompute_Dist_Matrix : in Boolean ) is
    Data : Str_Vector;
    Dists : Expectation_Maximization.Distance_Matrix;
    Centroids, Classification : Expectation_Maximization.Positive_Vector;
    Variance : Natural;
  begin
    Expectation_Maximization.Read_File_Lines( Filename => Filename, Data => Data );

    if Recompute_Dist_Matrix then
      Compute_Distance_Matrix( Data => Data,
                               Dists => Dists,
                               Number_Of_Cores => 8,
                               Metric => Lev_Dist'Access);
      Expectation_Maximization.Dump_Distance_Matrix_To_Disk(
        Filename => "key_distances.txt", Dists => Dists );
    else
      Expectation_Maximization.Load_Distance_Matrix_From_Disk(
        Filename => "key_distances.txt", Dists => Dists );
    end if;

    Ada.Text_IO.Put_Line("Running K =" & Integer'Image(K));
    Expectation_Maximization.EM( K => K,
                                 Dist_Matrix => Dists,
                                 Number_Of_Runs => 12,
                                 Centroids => Centroids,
                                 Classification => Classification,
                                 Variance => Variance);
    declare
      Space_K : constant String := Integer'Image(K);
      K_Str   : constant String := Space_K(2..Space_K'Last);
      Out_Filename : String := "K" & K_Str & ".txt";
      Centroids_Strs : Str_Vector;
      Centroid_Str : Ada.Strings.Unbounded.Unbounded_String;
    begin
      Ada.Text_IO.Put_Line("K = " & K_Str & ", Variance =" & Integer'Image(Variance));
      Ada.Text_IO.Put_Line("Writing results to file: " & Out_Filename);
      Centroids_Strs.Alloc(Centroids.Size);
      for I in 1..Centroids.Size loop
        Centroid_Str := Data.Get(Centroids.Get(I));
        Centroids_Strs.Set(I, Centroid_Str);
      end loop;
      Expectation_Maximization.Export_Results( Filename       => Out_Filename,
                                               Centroids_Strs => Centroids_Strs,
                                               Classification => Classification,
                                               Variance       => Variance);
    end;
  end Run;

end String_Clustering;
