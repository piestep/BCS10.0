-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with Ada.Containers.Vectors;
--
with System_Package;  use System_Package;
with Boolean_Package; use Boolean_Package;
--

-- A package to parse boolean argument values from the command line.
--
-- A boolean argument is either a variable or an array. An array is denoted by a
-- '@' and is formated with a size and a length seperated by a period followed by
-- it's space delimited values. A variable is denoted by a '!' is formated with
-- a size and a value seperated by a period. '%' denotes modular value.
-- Format: { @[%]<size>.<length> <value> { <value> } | ![%]<size>.<value> }

package Argument_Package is

   -- A usage error occurred parsing the argument values.

   Usage_Error : exception;

   -- Return the number of arguments.

   function Number_Of return SYSNatural;

   -- The boolean value of the arguments.

   function Boolean_Of return Boolean_Array_Type;

   -- The length of all boolean argument values.

   function Length_Of return SYSNatural;

   -- Return image of arguments.

   function Image_Of return String;

   -- Parse the command line starting at the start until the end.

   procedure Parse (The_Start : SYSNatural; The_End : SYSNatural);

end Argument_Package;
