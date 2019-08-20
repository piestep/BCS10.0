-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with System_Package; use System_Package;
--

-- A package to define Boolean Compiler (BC) constants.

package BC_Package is

   -- BC boolean values.

   type Boolean_Value is new SYSInteger range 0 .. 1;

   Boolean_False : constant := 0;
   Boolean_True  : constant := 1;

   -- An exception to end compilation immediately when an internal compiler
   -- consistancy check has failed.

   Critical_Error : exception;

end BC_Package;
