with Ada.Assertions;
package body Utils is

  procedure Reset( G : in Random_Generator ) is
  begin
    Random_Pack.Reset( Gen => G.G ); -- seed from the time-of-day of the clock
  end Reset;

  function Random_Positive( G : Random_Generator; Max : Positive ) return Positive is
  begin
    return (Random_Pack.Random(G.G) mod Max) + 1;
  end Random_Positive;

  function Random_Array_Of_Unique_Positives( G : Random_Generator;
                                             Max : Positive;
                                             N : Positive ) return Array_Of_Positives is
    A      : array(1..Max) of Natural;
    A_Max  : Positive := Max;
    RetVal : Array_Of_Positives(1..N);
  begin
    Ada.Assertions.Assert( Max >= N );
    for I in A'First..A'Last loop
      A(I) := I;
    end loop;

    for I in RetVal'First..RetVal'Last loop
      RetVal(I) := A(G.Random_Positive(A_Max));

      A(RetVal(I)..(A_Max-1)) := A((RetVal(I)+1)..A_Max);
      A_Max := A_Max - 1;
    end loop;
    return RetVal;
  end Random_Array_Of_Unique_Positives;

end Utils;
