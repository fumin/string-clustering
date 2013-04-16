with AUnit.Test_Fixtures;

package String_Distance.Test is

  type Test is new AUnit.Test_Fixtures.Test_Fixture with record
    Cat_String : String(1..3) := "cat";
  end record;

  procedure Set_Up (T : in out Test); -- Set up performed before each test routine

  procedure Test_Lev_Dist (T : in out Test);

end String_Distance.Test;
