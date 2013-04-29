with AUnit.Assertions;
with Ada.Text_IO; use Ada.Text_IO;
package body Expectation_Maximization.Test is

  procedure Set_Up (T: in out Test) is
    Dummy : Dist_Vector;
  begin
    -- The points 1, 2, 3, 4 are distributed as
    -- 2 <-----------------------> 4
    -- |                           |
    -- 1 <-----------------------> 3
    T.Dists.Alloc(4);
    Dummy.Alloc(1); T.Dists.Set(2, Dummy);
    Dummy.Alloc(2); T.Dists.Set(3, Dummy);
    Dummy.Alloc(3); T.Dists.Set(4, Dummy);

    T.Dists.Set(2, 1, 1);
    T.Dists.Set(3, 1, 5); T.Dists.Set(3, 2, 6);
    T.Dists.Set(4, 1, 6); T.Dists.Set(4, 2, 5); T.Dists.Set(4, 3, 1);
  end Set_Up;

  procedure Test_E_Step (T: in out Test) is
    Centroids, Classification : Positive_Vector;
    Variance : Positive;
  begin
    Centroids.Alloc(2);
    Centroids.Set(1, 1); Centroids.Set(2, 2);
    E_Step( Dist_Matrix    => T.Dists,
            Centroids      => Centroids,
            Classification => Classification,
            Variance       => Variance );
    AUnit.Assertions.Assert( Classification.Get(1) = 1, "First classification 1" );
    AUnit.Assertions.Assert( Classification.Get(2) = 2, "First classification 2" );
    AUnit.Assertions.Assert( Classification.Get(3) = 1, "First classification 3" );
    AUnit.Assertions.Assert( 10 = Variance, "First classification, Variance = 10");

    Centroids.Set(1, 3);
    E_Step( Dist_Matrix    => T.Dists,
            Centroids      => Centroids,
            Classification => Classification,
            Variance       => Variance );
    AUnit.Assertions.Assert( Classification.Get(1) = 2, "Second classification 1" );
    AUnit.Assertions.Assert( Classification.Get(2) = 2, "Second classification 2" );
    AUnit.Assertions.Assert( Classification.Get(3) = 1, "Second classification 3" );
    AUnit.Assertions.Assert( 2 = Variance, "First classification, Variance = 2"   );
  end Test_E_Step;

  procedure Test_M_Step( T : in out Test ) is
    Centroids, Classification : Positive_Vector;
  begin
    Classification.Alloc(4);
    Classification.Set(1, 1); Classification.Set(3, 1); Classification.Set(4, 1); -- first group
    Classification.Set(2, 2); -- second group

    Centroids := M_Step( Classification => Classification, K => 2, Dist_Matrix => T.Dists );
    AUnit.Assertions.Assert( Centroids.Get(1) = 3, "Centroid of 1st group should be 3" );
    AUnit.Assertions.Assert( Centroids.Get(2) = 2, "Centroid of 2nd group should be 2" );
  end Test_M_Step;

  procedure Test_EM( T : in out Test ) is
    Centroids, Classification : Positive_Vector;
    Variance : Natural;

    function Includes( E : Positive; V : Positive_Vector ) return Boolean is
    begin
      for I in 1..V.Size loop
        if E = V.Get(I) then
          return True;
        end if;
      end loop;
      return False;
    end Includes;
    type Array_Of_Positives is array(Positive range <>) of Positive;
    function Set_Equals( A : Array_Of_Positives; V : Positive_Vector ) return Boolean is
    begin
      for I in A'First..A'Last loop
        if not Includes(A(I), V) then
          return False;
        end if;
      end loop;
      return True;
    end Set_Equals;
  begin
    EM( K              => 2,
        Dist_Matrix    => T.Dists,
        Number_Of_Runs => 10,
        Centroids      => Centroids,
        Classification => Classification,
        Variance       => Variance);

    AUnit.Assertions.Assert( not (Set_Equals((1,2),Centroids) and Set_Equals((3,4),Centroids)),
                             "Assert Centroids" );
    AUnit.Assertions.Assert( Classification.Get(1) = Classification.Get(2), "1 = 2" );
    AUnit.Assertions.Assert( Classification.Get(3) = Classification.Get(4), "3 = 4" );
    AUnit.Assertions.Assert( Classification.Get(1) /= Classification.Get(3), "1 /= 3" );
    AUnit.Assertions.Assert( 2 = Variance, "Variance should be 2" );
  end Test_EM;

end Expectation_Maximization.Test;
