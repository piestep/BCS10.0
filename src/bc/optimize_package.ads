-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with BC_Package;    use BC_Package;
with Graph_Package; use Graph_Package;

-- A package to parse a program graph to optimize code generation.

-- Note: Currently optmization is only preformed on non-constant expression
-- (expression within the procedure body) to remove addition/subtraction of
-- 0 and multipllcation of 0 or 1.

-- addition and multipllcation Identity Properties multipllcation Zero Product
-- Property

package Optimize_Package is

   -- Debug Optimize switch.

   Optimize_Debug : Boolean := False;

   -- Parse a program graph to optimize code generation.

   procedure Optimize (The_Unit : in out Compilation_Unit_Graph);

end Optimize_Package;
