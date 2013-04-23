with Ada.Finalization;
generic
  type Element_Type is private;
package Thread_Safe_Vector is
  type Array_Of_Elements is array (Positive range <>) of Element_Type;
  type Array_Of_Elements_Ptr is access Array_Of_Elements;
  type Vector is tagged private;
  procedure Alloc( V : in out Vector; L : in Natural );
  procedure Set( V : in out Vector; Idx : in Positive; E : in Element_Type );
  function Get( V : in Vector; Idx : in Positive ) return Element_Type;
  function Size( V : in Vector ) return Natural;

  -- Use this function judiciously, for example, when implementing vectors of vectors
  function Ptr( V : in Vector ) return Array_Of_Elements_Ptr;

private
  type Vector is new Ada.Finalization.Controlled with record
    Content_Ptr : Array_Of_Elements_Ptr;
    Size        : Natural;
  end record;
  procedure Initialize( V : in out Vector );
  procedure Finalize( V : in out Vector );
  procedure Adjust( V : in out Vector );
end Thread_Safe_Vector;
