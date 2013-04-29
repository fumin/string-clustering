with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Strings.Unbounded;
with GNAT.OS_Lib;
with String_Clustering;
procedure Main is
  DataFilename : Ada.Strings.Unbounded.Unbounded_String;
  K            : Positive;
  Recompute_Dist_Matrix : Boolean;
begin
  if Ada.Command_Line.Argument_Count /= 3 then
    Ada.Text_IO.Put_Line("Usage: 'data filename', 'number of class', 'recompute distance matrix or not (y for true, and others for false)?'");
    Ada.Text_IO.Put_Line("Example: ./main data.txt 7 n");
    GNAT.OS_Lib.OS_Exit(0);
  end if;

  DataFilename := Ada.Strings.Unbounded.To_Unbounded_String(Ada.Command_Line.Argument(1));
  K := Integer'Value(Ada.Command_Line.Argument(2));
  if "y" = Ada.Command_Line.Argument(3) then
    Recompute_Dist_Matrix := True;
  else
    Recompute_Dist_Matrix := False;
  end if;
  String_Clustering.Run( Filename => Ada.Strings.Unbounded.To_String(DataFilename),
                         K        => K,
                         Recompute_Dist_Matrix => Recompute_Dist_Matrix);
end Main;
