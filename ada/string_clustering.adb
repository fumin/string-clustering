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

  procedure Run( Filename : in String; K : in Positive ) is
    Data : Str_Vector;
    Dists : Expectation_Maximization.Distance_Matrix;

--Testt : Expectation_Maximization.Dist_Vector;
  begin
    Expectation_Maximization.Read_File_Lines( Filename => Filename, Data => Data );
    Expectation_Maximization.Read_Distance_Matrix_From_Disk(
      Filename => "key_distances.txt", Dists => Dists );
    --Compute_Distance_Matrix( Data => Data,
    --                         Dists => Dists,
    --                         Number_Of_Cores => 8,
    --                         Metric => Lev_Dist'Access);
    --Expectation_Maximization.Save_Distance_Matrix_To_Disk(
    --  Filename => "key_distances.txt", Dists => Dists );

    Ada.Text_IO.Put_Line("1st, 2nd: " & Integer'Image(Integer(Expectation_Maximization.Read_Distance_Matrix(1,2, Dists))));
    Ada.Text_IO.Put_Line("Last, Last-1: " & Integer'Image(Integer(Expectation_Maximization.Read_Distance_Matrix(Data.Size-1,Data.Size,Dists))));

--Dists.Alloc(2);
--Testt.Alloc(3);
--Dists.Set(1, Testt);
--
--Testt.Alloc(4);
--Dists.Set(2, Testt);
--
--Testt.Alloc(0);
--
--Ada.Text_IO.Put_Line("ASSIGNING...");
--Dists.Set(1, 1, 97); Dists.Set(1, 2, 98); Dists.Set(1, 3, 90);
--Dists.Set(2, 1, 44);  Dists.Set(2, 2, 41); Dists.Set(2, 3, 67);
--
--Ada.Text_IO.Put_Line( "2, 1 = " & Integer'Image(Integer(Dists.Get(2, 1))) );

  end Run;

end String_Clustering;
