-- BC Boolean Compiler
-- Copyright (c) 2015 Paul Estep

-- A package

package Argument_Package.Command_Line is

   function Argument_Count return Natural;

   function Argument_At (The_Offset : Positive) return String;

end Argument_Package.Command_Line;
