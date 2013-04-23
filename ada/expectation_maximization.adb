with Ada.Text_IO;
with Ada.Integer_Text_IO;
with GNAT.String_Split;
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

  procedure Save_Distance_Matrix_To_Disk( Filename : in String;
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
  end Save_Distance_Matrix_To_Disk;

  procedure Read_Distance_Matrix_From_Disk( Filename : in String;
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
  end Read_Distance_Matrix_From_Disk;

-- WIP
  procedure E_Step( Dist_Matrix    : in Distance_Matrix;
                    Centroids      : in Positive_Vector;
                    Classification : out Positive_Vector) is
  begin
    for I in 1..Dist_Matrix.Size loop
null;
    end loop;
Classification.Alloc(Dist_Matrix.Size);
  end E_Step;

-- WIP
  function M_Step( Dist_Matrix    : in Distance_Matrix;
                   Classification : in Positive_Vector) return Positive_Vector is
    Centroids : Positive_Vector;
  begin
    return Centroids;
  end M_Step;
end Expectation_Maximization;
