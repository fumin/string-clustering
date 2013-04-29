with AUnit.Test_Caller;
with String_Distance.Test;
with Expectation_Maximization.Test;

package body String_Clustering_Suite is
  package String_Distance_Caller is new AUnit.Test_Caller( String_Distance.Test.Test );
  package EM_Caller is new AUnit.Test_Caller( Expectation_Maximization.Test.Test );

  function Suite return AUnit.Test_Suites.Access_Test_Suite is
    Ret : constant AUnit.Test_Suites.Access_Test_Suite := new AUnit.Test_Suites.Test_Suite;
  begin
    Ret.Add_Test( String_Distance_Caller.Create("Test Levenshtein Distance",
                  String_Distance.Test.Test_Lev_Dist'Access) );

    Ret.Add_Test( EM_Caller.Create("Test E_Step",
                  Expectation_Maximization.Test.Test_E_Step'Access) );
    Ret.Add_Test( EM_Caller.Create("Test M_Step",
                  Expectation_Maximization.Test.Test_M_Step'Access) );
    Ret.Add_Test( EM_Caller.Create("Test EM",
                  Expectation_Maximization.Test.Test_EM'Access) );
    return Ret;
  end Suite;

end String_Clustering_Suite;
