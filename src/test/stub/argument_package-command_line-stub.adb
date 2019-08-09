-- BCS Boolean Compiler System
-- Copyright (c) 2017 Paul Estep

with Ada.Containers.Vectors;
with Ada.Strings;           use Ada.Strings;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--

-- A package

package body Argument_Package.Command_Line is

   The_Count : Natural := 0;

   package String_Vector is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Unbounded_String);

   The_Arguments : String_Vector.Vector;

   procedure Set_Line (The_String : String) is
      The_Position : SYSPositive := 1;
      The_First    : SYSPositive;
   begin
      The_Arguments := String_Vector.Empty_Vector;
      while The_Position <= The_String'Length loop

         while The_Position <= The_String'Length
           and then The_String (The_Position) = ' '
         loop
            The_Position := The_Position + 1;
         end loop;

         exit when The_Position > The_String'Length;

         The_First := The_Position;
         while The_Position <= The_String'Length
           and then The_String (The_Position) /= ' '
         loop
            The_Position := The_Position + 1;
         end loop;

         The_Count := The_Count + 1;
         String_Vector.Append
           (The_Arguments,
            To_Unbounded_String (The_String (The_First .. The_Position - 1)));
      end loop;

   end Set_Line;

   function Argument_Count return SYSNatural is
   begin
      return The_Count;
   end Argument_Count;

   function Argument_At (The_Offset : SYSPositive) return String is
   begin
      return To_String (String_Vector.Element (The_Arguments, The_Offset));
   end Argument_At;

end Argument_Package.Command_Line;
