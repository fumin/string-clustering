with AUnit.Test_Fixtures;

package Expectation_Maximization.Test is

  type Test is new AUnit.Test_Fixtures.Test_Fixture with record
    Dists : Distance_Matrix;
  end record;

  procedure Set_Up (T : in out Test); -- Set up performed before each test routine

  procedure Test_E_Step (T : in out Test);
  procedure Test_M_Step (T : in out Test);
  procedure Test_EM     (T : in out Test);

end Expectation_Maximization.Test;
