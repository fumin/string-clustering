with AUnit.Reporter.Text;
with AUnit.Run;
with String_Clustering_Suite;

procedure Test_String_Clustering is
  procedure Run is new AUnit.Run.Test_Runner (String_Clustering_Suite.Suite);
  Reporter : Aunit.Reporter.Text.Text_Reporter;
begin
  Run( Reporter );
end Test_String_Clustering; 
