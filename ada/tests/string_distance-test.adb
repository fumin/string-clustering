with AUnit.Assertions;

package body String_Distance.Test is

  The_String : String := "The";
  Unsliced_String : String := "12345678901234567890";
  Unsliced : String := "12345678901234567890";

  procedure Set_Up (T: in out Test) is
  begin
    The_String := "the";
  end Set_Up;

  procedure Test_Lev_Dist (T: in out Test) is
  begin
    AUnit.Assertions.Assert( Lev_Dist("hat", T.Cat_String) = 1, "hat" );
    AUnit.Assertions.Assert( Lev_Dist("catwalk", T.Cat_String) = 4, "catwalk" );
    AUnit.Assertions.Assert( Lev_Dist("furriers", T.Cat_String) = 8, "furriers" );
    AUnit.Assertions.Assert( Lev_Dist("caparisons", T.Cat_String) = 8, "caparisons" );

    Unsliced_String := "1234567890aahat67890";
    AUnit.Assertions.Assert( Lev_Dist(Unsliced_String(13..15), T.Cat_String) = 1,
                             "sliced hat" );
    AUnit.Assertions.Assert( Lev_Dist(T.Cat_String, Unsliced_String(13..15)) = 1,
                             "sliced hat reversed" );

    Unsliced_String := "1234567890123catwalk";
    Unsliced := "1234567cat1234567890";
    AUnit.Assertions.Assert( Lev_Dist(Unsliced_String(14..20), Unsliced(8..10)) = 4,
                             "sliced catwalk" );
    AUnit.Assertions.Assert( Lev_Dist(Unsliced(8..10), Unsliced_String(14..20)) = 4,
                             "sliced catwalk reversed" );

    Unsliced_String := "12345678901furriers0";
    Unsliced := "12345678901234cat890";
    AUnit.Assertions.Assert( Lev_Dist(Unsliced_String(12..19), Unsliced(15..17)) = 8,
                             "sliced furriers" );
    AUnit.Assertions.Assert( Lev_Dist(Unsliced(15..17), Unsliced_String(12..19)) = 8,
                             "sliced furriers reversed" );

    AUnit.Assertions.Assert( Lev_Dist("hte", The_String) = 2, "hte" );
    AUnit.Assertions.Assert( Lev_Dist("htne", The_String) = 2, "htne" );
    AUnit.Assertions.Assert( Lev_Dist("thhe", The_String) = 1, "hhte" );
    AUnit.Assertions.Assert( Lev_Dist("the", The_String) = 0, "the" );
    AUnit.Assertions.Assert( Lev_Dist("then", The_String) = 1, "then" );
    AUnit.Assertions.Assert( Lev_Dist("The", The_String) = 1, "The" );
    AUnit.Assertions.Assert( Lev_Dist("THE", The_String) = 3, "THE" );
  end Test_Lev_Dist;

end String_Distance.Test;
