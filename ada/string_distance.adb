with Ada.Containers.Hashed_Maps;

package body String_Distance is
  function Lev_Dist (A_String : String; B_String : String) return Natural is
    Max_Score : constant Natural := A_String'Length + B_String'Length;
    type Score_Matrix is array(A_String'First..(A_String'Last + 2),
                               B_String'First..(B_String'Last + 2)) of Natural;

    -- Matrix of the distance between each char of A_String and B_String
    Distances : Score_Matrix := (others => (others => Max_Score));

    -- Last index of B_String where A_String and B_String are the same char
    Same_Char_B_Idx : Integer := 0;

    -- Hash to store the counting position in A_String for a given char
    function Character_Hash(C : Character) return Ada.Containers.Hash_Type is
    begin return Ada.Containers.Hash_Type( Character'Pos(C) ); end;
    package Char_A_String_Position is new Ada.Containers.Hashed_Maps(
       Key_Type => Character,
       Element_Type => Integer,
       Hash => Character_Hash,
       Equivalent_keys => "=");
    CharPos : Char_A_String_Position.Map;

    -- Local variables
    A_Pos : Integer;
    B_Pos : Integer;
  begin
    -- Initialize Distances
    for I in A_String'First..(A_String'Last+1) loop
      Distances(I + 1, B_String'First+1) := I - A_String'First;
      Distances(I + 1, B_String'First) := Max_Score;
    end loop;
    for J in B_String'First..(B_String'Last+1) loop
      Distances(A_String'First+1, J + 1) := J - B_String'First;
      Distances(A_String'First, J + 1) := Max_Score;
    end loop;

    -- Initialize CharPos with zeros
    for I in A_String'Range loop
      CharPos.Include( A_String(I), 0 );
    end loop;
    for J in B_String'Range loop
      CharPos.Include( B_String(J), 0 );
    end loop;

    for A_String_Idx in (A_String'First+1)..(A_String'Last+1) loop
      for B_String_Idx in (B_String'First+1)..(B_String'Last+1) loop
        A_Pos := CharPos.Element( B_String(B_String_Idx-1) );
        B_Pos := Same_Char_B_Idx;

        if A_String(A_String_Idx-1) = B_String(B_String_Idx-1) then
          Distances(A_String_Idx+1, B_String_Idx+1) := Distances(A_String_Idx, B_String_Idx);
          Same_Char_B_Idx := B_String_Idx - B_String'First;
        else
          Distances(A_String_Idx+1, B_String_Idx+1) :=
            Integer'Min( Distances(A_String_Idx, B_String_Idx),
                         Integer'Min( Distances(A_String_Idx+1, B_String_Idx),
                                      Distances(A_String_Idx, B_String_Idx+1))) + 1;
        end if;

        Distances(A_String_Idx+1, B_String_Idx+1) :=
          Integer'Min( Distances(A_String_Idx+1, B_String_Idx+1),
                       Distances(A_String_Idx, B_String_Idx) +
                         (A_String_Idx - A_Pos - (A_String'First+1)) +
                         1 + (B_String_Idx - B_Pos - (B_String'First+1)) );

      end loop;

      CharPos.Include( A_String(A_String_Idx-1), A_String_Idx - A_String'First );
    end loop;
    return Distances(A_String'Last + 2, B_String'Last + 2);

  end Lev_Dist;
end String_Distance;
