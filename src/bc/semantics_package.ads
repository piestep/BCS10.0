-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with BC_Package;    use BC_Package;
with Graph_Package; use Graph_Package;
--

-- A package to parse a program graph and check program semenatics.

package Semantics_Package is

   -- Debug Semantics parser switch.

   Semantics_Debug : Boolean := False;

   -- Parse a program graph and check program semenatics.

   procedure Parse (The_Unit : Compilation_Unit_Graph);

end Semantics_Package;
