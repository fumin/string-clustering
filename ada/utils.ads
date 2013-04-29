with Ada.Numerics.Discrete_Random;
package Utils is

  type Array_Of_Positives is array(Positive range <>) of Positive;
  type Random_Generator is tagged limited private;
  procedure Reset( G : in Random_Generator );
  function Random_Positive( G : in Random_Generator; Max : in Positive ) return Positive;
  function Random_Array_Of_Unique_Positives( G   : in Random_Generator;
                                             Max : in Positive;
                                             N   : in Positive ) return Array_Of_Positives;
private
  subtype R is Positive range 1..32767;
  package Random_Pack is new Ada.Numerics.Discrete_Random(R);
  type Random_Generator is tagged limited record
    G : Random_Pack.Generator;
  end record;
end Utils;
