with AUnit.Test_Caller;
with String_Distance.Test;

package body String_Clustering_Suite is
  package Caller is new AUnit.Test_Caller (String_Distance.Test.Test);

  function Suite return AUnit.Test_Suites.Access_Test_Suite is
    Ret : constant AUnit.Test_Suites.Access_Test_Suite := new AUnit.Test_Suites.Test_Suite;
  begin
    Ret.Add_Test( Caller.Create("Test Levenshtein Distance",
                  String_Distance.Test.Test_Lev_Dist'Access) );
    return Ret;
  end Suite;

end String_Clustering_Suite;
