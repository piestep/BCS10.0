-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with AUnit;
with AUnit.Test_Fixtures;
with AUnit.Test_Cases;
--

package Source_Package.Source_Test is

    type Test is new AUnit.Test_Cases.Test_Case with null record;

    overriding function Name (The_Test : in Test) return AUnit.Test_String;
    overriding procedure Register_Tests (The_Test : in out Test);

    procedure Test_Source (The_Test : in out AUnit.Test_Cases.Test_Case'Class);

end Source_Package.Source_Test;
