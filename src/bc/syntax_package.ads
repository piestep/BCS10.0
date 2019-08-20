-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with BC_Package;    use BC_Package;
with Graph_Package; use Graph_Package;
--

-- A package to parse Boolean Compiler program souce code.

package Syntax_Package is

   -- Debug syntax parser switch.

   Syntax_Debug : Boolean := False;

   -- Parse Boolean Compiler program source code syntax.

   procedure Parse (The_Unit : out Compilation_Unit_Graph);

end Syntax_Package;
