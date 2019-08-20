-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with Source_Package.Source_Test;
--

package body Source_Package_Test_Suite is

    Result : aliased Test_Suite;

    Source_Tests : aliased Source_Package.Source_Test.Test;

    -----------
    -- Suite --
    -----------

    function Suite return Access_Test_Suite is
    begin
        Add_Test (Result'Access, Source_Tests'Access);
        return Result'Access;
    end Suite;

end Source_Package_Test_Suite;
