with Ada.Numerics.Float_Random;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with GNAT.String_Split;
with Utils;
package body Expectation_Maximization is
  procedure Set( V : in out Distance_Matrix;
                 X : in Positive;
                 Y : in Positive;
                 N : in Distance_Type ) is
  begin
    V.Ptr.all(X).Ptr.all(Y) := N;
  end;
  function Get(  V : in Distance_Matrix;
                 X : in Positive;
                 Y : in Positive) return Distance_Type is
  begin
    return V.Ptr.all(X).Ptr.all(Y);
  end;

  -- Str_Vector methods
  procedure Set( V : in out Str_Vector; Idx : in Positive; S : in String ) is
  begin
    Str_Vector_Pack.Set( V => Str_Vector_Pack.Vector(V),
                         Idx => Idx,
                         E => Ada.Strings.Unbounded.To_Unbounded_String(S) );
  end Set;
  function Get( V : in Str_Vector; Idx : in Positive ) return String is
  begin
    return Ada.Strings.Unbounded.To_String(
             Str_Vector_Pack.Get(V => Str_Vector_Pack.Vector(V), Idx => Idx) );
  end Get;

  procedure Read_File_Lines( Filename : in String; Data : out Str_Vector ) is
    Input_File : Ada.Text_IO.File_Type;
    Idx : Positive := 1;
    Line_Count : Natural := 0;
  begin
    -- Count number of lines
    Ada.Text_IO.Open( File => Input_File,
                      Mode => Ada.Text_IO.In_File,
                      Name => Filename);
    while not Ada.Text_IO.End_Of_File(Input_File) loop
      declare
        Dummy : Ada.Strings.Unbounded.Unbounded_String;
      begin
        Dummy := Ada.Strings.Unbounded.To_Unbounded_String(Ada.Text_IO.Get_Line(Input_File));
      end;
      Line_Count := Line_Count + 1;
    end loop;
    Ada.Text_IO.Close(Input_File);

    -- Store lines into vector
    Data.Alloc( L => Line_Count );
    Ada.Text_IO.Open( File => Input_File,
                      Mode => Ada.Text_IO.In_File,
                      Name => Filename);
    while not Ada.Text_IO.End_Of_File(Input_File) loop
      Data.Set( Idx => Idx, S => Ada.Text_IO.Get_Line(Input_File) );
      Idx := Idx + 1;
    end loop;
    Ada.Text_IO.Close(Input_File);
  end Read_File_Lines;

  -------------------------------------------
  -- Methods that deal with Distance_Matrices
  -- * Read_Distance_Matrix reads the distance from a Distance_Matrix
  -- * Dump_Distance_Matrix_To_Disk dumps a Distance_Matrix to disk
  -- * Load_Distance_Matrix_From_Disk loads a Distance_Matrix back from disk
  -------------------------------------------
  function Read_Distance_Matrix( A_Idx : in Positive;
                                 B_Idx : in Positive;
                                 Dists : in Distance_Matrix) return Distance_Type is
  begin
    if A_Idx > B_Idx then
      return Dists.Get(A_Idx, B_Idx);
    elsif A_Idx = B_Idx then
      return 0;
    else
      return Dists.Get(B_Idx, A_Idx);
    end if;
  end Read_Distance_Matrix;

  procedure Dump_Distance_Matrix_To_Disk( Filename : in String;
                                          Dists    : in Distance_Matrix) is
    Dists_File : Ada.Text_IO.File_Type;
  begin
    Ada.Text_IO.Create( File => Dists_File,
                        Mode => Ada.Text_IO.Out_File,
                        Name => Filename);
    for I in 1..Dists.Size loop
      for J in 1..(I-1) loop
        Ada.Integer_Text_IO.Put(Dists_File,
                                Item => Integer(Read_Distance_Matrix(I, J, Dists)),
                                Width => 0);
        if J /= (I-1) then
          Ada.Text_IO.Put(Dists_File, " ");
        else
          Ada.Text_IO.Put_Line(Dists_File, "");
        end if;
      end loop;
    end loop;
    Ada.Text_IO.Close(Dists_File);
  end Dump_Distance_Matrix_To_Disk;

  procedure Load_Distance_Matrix_From_Disk( Filename : in String;
                                            Dists    : out Distance_Matrix) is
    Strs : Str_Vector;
    Subs : GNAT.String_Split.Slice_Set;
    Dummy : Expectation_Maximization.Dist_Vector;
  begin
    Read_File_Lines( Filename => Filename, Data => Strs );
    Dists.Alloc(Strs.Size + 1);
    for I in 2..(Strs.Size + 1) loop
      Dummy.Alloc(I-1);
      Dists.Set(I, Dummy);
      GNAT.String_Split.Create( S          => Subs,
                                From       => Strs.Get(I-1),
                                Separators => " ",
                                Mode       => GNAT.String_Split.Multiple);
      for J in 1..GNAT.String_Split.Slice_Count(Subs) loop
        Dists.Set(I, Positive(J), Distance_Type'Value(GNAT.String_Split.Slice(Subs, J)) );
      end loop;
    end loop;
  end Load_Distance_Matrix_From_Disk;

  function Min_Idx( V : in Dist_Vector ) return Positive is
    Idx_With_Min : Positive := 1;
  begin
    for I in 2..V.Size loop
      if V.Get(I) < V.Get(Idx_With_Min) then
        Idx_With_Min := I;
      end if;
    end loop;
    return Idx_With_Min;
  end Min_Idx;

  -------------------------------------------------
  -- Implementation of the Expectation Step, E_Step.
  -- `E_Step_For_An_Element` returns the classification
  -- of the element at Index `Idx`, and can put into task in its own.
  -- This opens up possibilities of parallelizing the `E_Step`
  -------------------------------------------------
  function E_Step_For_An_Element( Dist_Matrix : in Distance_Matrix;
                                  Centroids   : in Positive_Vector;
                                  Idx         : in Positive) return Positive is
    -- returns the classification of the element at Index `Idx`
    Distance : Distance_Type;
    Distance_To_Centroids : Dist_Vector;
  begin
    Distance_To_Centroids.Alloc(Centroids.Size);
    for I in 1..Centroids.Size loop
      Distance := Read_Distance_Matrix( A_Idx => Centroids.Get(I),
                                        B_Idx => Idx,
                                        Dists => Dist_Matrix);
      Distance_To_Centroids.Set( I, Distance );
    end loop;
    return Min_Idx(Distance_To_Centroids);
  end E_Step_For_An_Element;

  procedure E_Step( Dist_Matrix    : in Distance_Matrix;
                    Centroids      : in Positive_Vector;
                    Classification : out Positive_Vector;
                    Variance       : out Natural) is
    Classification_For_I : Positive;
    Dist_To_Centroid     : Distance_Type;
    Centroid             : Positive;
  begin
    if Classification.Size /= Dist_Matrix.Size then
      Classification.Alloc(Dist_Matrix.Size);
    end if;
    Variance := 0;

    for I in 1..Dist_Matrix.Size loop
      Classification_For_I := E_Step_For_An_Element( Dist_Matrix => Dist_Matrix,
                                                     Centroids   => Centroids,
                                                     Idx         => I);
      Classification.Set( I, Classification_For_I );

      Centroid := Centroids.Get(Classification_For_I);
      Dist_To_Centroid := Read_Distance_Matrix(Centroid, I, Dist_Matrix);
      Variance := Variance + Natural(Dist_To_Centroid);
    end loop;
  end E_Step;

  --------------------------------------------------
  -- Implementation of the Maximization Step, M_Step.
  -- `Distance_Between` returns a vector denoting
  -- the sum of distances between an element and elements of each class.
  -- For example the first slot of the vector returned by `Distance_Between`
  -- is the sum of distances between that element and elements belonging to the first class.
  -- The second slot is the sum of distances with respect to elements of the second class, etc..
  -- Naturally, `Distance_Between` forms the basis of parallelizing the `M_Step`.
  --------------------------------------------------
  function Distance_Between( Idx            : in Positive;
                             -- Classification is expected to have the same size as Dist_Matrix.
                             -- An example of an Classification would be:
                             -- If Element 1 belongs to class 2; element 2 belongs to class 2;
                             -- and element 3 belongs to class 1.
                             -- Then the Classification vector is [2, 2, 1].
                             -- We also require the numbering of classes to start from 1 as
                             -- is evident in the above example.
                             Classification : in Positive_Vector;
                             -- Although, we can compute K from the Classification,
                             -- it's more efficient to provide it here explicitly.
                             K              : in Positive;
                             Dist_Matrix    : in Distance_Matrix)
           return Positive_Vector is
    Distances_To_Each_Class : Positive_Vector;
    Element_Class : Positive;
    Original_Sum : Positive;
    New_Sum : Positive;
  begin
    Distances_To_Each_Class.Alloc(K);
    for I in 1..K loop
      -- It should be 0 below, but it doesn't hurt.
      -- We're just too lazy to define a new type Natural_Vector.
      Distances_To_Each_Class.Set(I, 1);
    end loop;

    for I in 1..Classification.Size loop
      Element_Class := Classification.Get(I);
      Original_Sum := Distances_To_Each_Class.Get(Element_Class);
      New_Sum := Original_Sum + Natural(Read_Distance_Matrix(Idx, I, Dist_Matrix));
      Distances_To_Each_Class.Set( Element_Class, New_Sum );
    end loop;
    return Distances_To_Each_Class;
  end Distance_Between;

  function M_Step( Classification : in Positive_Vector;
                   -- We can of course compute K, the number of classes from Classification,
                   -- but it's more efficient to provide it here.
                   K              : in Positive;
                   Dist_Matrix    : in Distance_Matrix) return Positive_Vector is
    Centroids : Positive_Vector;
    Centroids_Distances : Positive_Vector;
    Distances_To_Each_Class : Positive_Vector;
  begin
    Centroids.Alloc(K);
    for Class in 1..K loop
      Centroids.Set(Class, 1);
    end loop;
    Centroids_Distances := Distance_Between(1, Classification, K, Dist_Matrix);

    for I in 2..Classification.Size loop
      Distances_To_Each_Class := Distance_Between( Idx => I,
                                                   Classification => Classification,
                                                   K => K,
                                                   Dist_Matrix => Dist_Matrix);
      for Class in 1..K loop
        if Distances_To_Each_Class.Get(Class) < Centroids_Distances.Get(Class) then
          Centroids.Set(Class, I);
          Centroids_Distances.Set( Class, Distances_To_Each_Class.Get(Class) );
        end if;
      end loop;
    end loop;

    return Centroids;
  end M_Step;

  procedure EM( K              : in Positive;
                Dist_Matrix    : in Distance_Matrix;
                Number_Of_Runs : in Positive; 
                Centroids      : out Positive_Vector;
                Classification : out Positive_Vector;
                Variance       : out Natural) is
    Centroids_Of_Each_Round : array(1..Number_Of_Runs) of Positive_Vector;
    type Array_Of_Naturals is array(1..Number_Of_Runs) of Natural;
    Variances_Of_Each_Round : Array_Of_Naturals;
    Best_Run                : Positive;

    function Min_Idx( V : in Array_Of_Naturals ) return Positive is
      Idx_With_Min : Positive := Array_Of_Naturals'First;
    begin
      for I in (Array_Of_Naturals'First+1)..Array_Of_Naturals'Last loop
        if V(I) < V(Idx_With_Min) then
          Idx_With_Min := I;
        end if;
      end loop;
      return Idx_With_Min;
    end Min_Idx;
  begin
    declare
      Initial_Centroids : Positive_Vector;
      Dummy             : Utils.Array_Of_Positives(1..K);
      G                 : Utils.Random_Generator;
      task type EM_Run is
        entry Start( R : in Positive );
      end;
      task body EM_Run is
        Run_Idx                     : Positive;
        Classification_For_This_Run : Positive_Vector;
        Previous_Variance           : Natural := Integer'Last;
        Iteration                   : Positive := 1;
      begin
        accept Start( R : in Positive ) do
          Run_Idx := R;
        end Start;

        EM_Loop:
        loop

-- Log to screen
--declare
--  Cs : Positive_Vector := Centroids_Of_Each_Round(Run_Idx);
--begin
--  Ada.Text_IO.Put("Round" & Integer'Image(Run_Idx) & ":");
--  for I in 1..Cs.Size loop
--    Ada.Text_IO.Put(Integer'Image(Cs.Get(I)) & " ");
--  end loop;
--  Ada.Text_IO.Put_Line(" ");
--end;

          E_Step( Dist_Matrix    => Dist_Matrix,
                  Centroids      => Centroids_Of_Each_Round(Run_Idx),
                  Classification => Classification_For_This_Run,
                  Variance       => Variances_Of_Each_Round(Run_Idx));
          exit EM_Loop when Variances_Of_Each_Round(Run_Idx) = Previous_Variance;
          Previous_Variance := Variances_Of_Each_Round(Run_Idx);

          Centroids_Of_Each_Round(Run_Idx) :=
            M_Step( Classification => Classification_For_This_Run,
                    K              => K,
                    Dist_Matrix    => Dist_Matrix);
--debug
--Ada.Text_IO.Put_Line("Run" & Integer'Image(Run_Idx) & ": Iteration = " & Integer'Image(Iteration));
--Iteration := Iteration + 1;
        end loop EM_Loop;
      end EM_Run;
      Tasks : array(1..Number_Of_Runs) of EM_Run;
    begin
      Initial_Centroids.Alloc(K);
      for R in 1..Number_Of_Runs loop
        --G.Reset;
        Dummy := G.Random_Array_Of_Unique_Positives( Max => Dist_Matrix.Size,
                                                     N   => K);

        for Class in 1..K loop
          Initial_Centroids.Set( Class, Dummy(Class) );
        end loop;
        Centroids_Of_Each_Round(R) := Initial_Centroids;
        Tasks(R).Start( R => R );
      end loop;
    end;

    Best_Run := Min_Idx(Variances_Of_Each_Round);
    Centroids := Centroids_Of_Each_Round(Best_Run);
    E_Step( Dist_Matrix => Dist_Matrix,
            Centroids   => Centroids,
            Classification => Classification,
            Variance => Variance);
  end EM;

  -- Export the results including the Centroids, the Classification,
  -- and the Variance to a file.
  procedure Dump_Vector_To_File( F : in Ada.Text_IO.File_Type;
                                 V : in Positive_Vector) is
  begin
    for I in 1..V.Size loop
      Ada.Integer_Text_IO.Put(F, Item => V.Get(I), Width => 0);
      Ada.Text_IO.Put_Line(F, "");
    end loop;
  end Dump_Vector_To_File;
  procedure Export_Results( Filename       : in String;
                            Centroids_Strs : in Str_Vector;
                            Classification : in Positive_Vector;
                            Variance       : in Natural) is
    Out_File : Ada.Text_IO.File_Type;
  begin
    Ada.Text_IO.Create( File => Out_File,
                        Mode => Ada.Text_IO.Out_File,
                        Name => Filename);
    Ada.Text_IO.Put_Line(Out_File, "Variance: " & Integer'Image(Variance));
    for I in 1..Centroids_Strs.Size loop
      Ada.Text_IO.Put_Line(Out_File, Centroids_Strs.Get(I));
    end loop;
    Ada.Text_IO.Put_Line(Out_File, "------------------");
    Dump_Vector_To_File( Out_File, Classification);
    Ada.Text_IO.Close(Out_File);
  end Export_Results;

end Expectation_Maximization;
