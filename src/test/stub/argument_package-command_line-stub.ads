-- BCS Boolean Compiler System
-- Copyright (c) 2017 Paul Estep

-- A package

package Argument_Package.Command_Line is

   procedure Set_Line (The_String : String);

   function Argument_Count return SYSNatural;

   function Argument_At (The_Offset : SYSPositive) return String;

end Argument_Package.Command_Line;
