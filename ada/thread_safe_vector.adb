with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
package body Thread_Safe_Vector is
  procedure Delete is new Ada.Unchecked_Deallocation(
                            Object => Array_Of_Elements,
                            Name => Array_Of_Elements_Ptr);

  procedure Alloc( V : in out Vector; L : in Natural ) is
  begin
    Delete(V.Content_Ptr);
    V.Content_Ptr := new Array_Of_Elements(1..L);
    V.Size        := L;
  end Alloc;

  procedure Initialize( V : in out Vector ) is
  begin
--    Ada.Text_IO.Put_Line("Initialize!");
    V.Content_Ptr := new Array_Of_Elements(1..0);
    V.Size        := 0;
  end Initialize;

  procedure Finalize( V: in out Vector ) is
  begin
--    Ada.Text_IO.Put_Line("Finalize!" & Integer'Image(V.Size));
    Delete(V.Content_Ptr);
  end Finalize;

  procedure Adjust( V : in out Vector ) is
    Temp : Array_Of_Elements_Ptr := new Array_Of_Elements(1..V.Size);
  begin
--    Ada.Text_IO.Put_Line("Adjust!" & Integer'Image(V.Size));
    for I in 1..V.Size loop
      Temp(I) := V.Content_Ptr(I);
    end loop;
    V.Content_Ptr := Temp;
  end Adjust;

  procedure Set( V : in out Vector; Idx : in Positive; E : in Element_Type ) is
  begin
    V.Content_Ptr(Idx) := E;
  end Set;

  function Get( V : in Vector; Idx : in Positive ) return Element_Type is
  begin
    return V.Content_Ptr(Idx);
  end Get;

  function Size( V : in Vector ) return Natural is
  begin
    return V.Size;
  end Size;

  function Ptr( V : in Vector ) return Array_Of_Elements_Ptr is
  begin
    return V.Content_Ptr;
  end Ptr;

end Thread_Safe_Vector;
