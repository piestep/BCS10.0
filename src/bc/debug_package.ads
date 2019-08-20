-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with BC_Package; use BC_Package;
--

-- A package to define debuging definitions.

package Debug_Package is

   -- Debug flag for tracking package initialization.

   Debug_Initialization : Boolean := False;

   -- Raise critical exception if flag is true.

   procedure Check (The_Flag : Boolean);

   -- Print message if flag is true.

   procedure Debug (The_Flag : Boolean; The_Message : String);

end Debug_Package;
