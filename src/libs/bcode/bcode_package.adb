-- BC Boolean Compiler
-- Copyright (c) 2015 Paul Estep

with Ada.Text_IO;
--
with Ada.Unchecked_Deallocation;
--
with Variable_Package; use Variable_Package;
--

package body BCode_Package is

   -- Dispose the words.

   procedure Dispose (The_Words : in out BCode_Words) is
      The_Cursor : Cursor;
      The_Word   : Word_Type;
   begin
      The_Cursor := First (The_Words);
      while The_Cursor /= No_Element loop
         The_Word := Element (The_Cursor);
         Dispose (The_Word);
         The_Cursor := Next (The_Cursor);
      end loop;
      Vector (The_Words) := Empty_Vector;
   end Dispose;

   -- Append the word to the bcode words.

   procedure Append
     (The_Words : in out BCode_Words;
      The_Word  :        Word_Type)
   is
   begin
      Vector_Package.Append (Vector (The_Words), The_Word);
   end Append;

   -- Normalize the boolean equations with all its variables.

   procedure Normalize (The_Words : in out BCode_Words) is
      The_Cursor : Cursor;
      The_Set    : Variable_Set_Type := EMPTY_SET;
      The_Word   : Word_Type;
   begin

      The_Cursor := First (The_Words);

      while The_Cursor /= No_Element loop
         The_Set    := The_Set or Variables_Of (Element (The_Cursor));
         The_Cursor := Next (The_Cursor);
      end loop;

      The_Cursor := First (The_Words);

      while The_Cursor /= No_Element loop
         The_Word := Element (The_Cursor);
         Normalize (The_Word, The_Set);
         The_Cursor := Next (The_Cursor);
      end loop;

   end Normalize;

   -- Return number of boolean equations.

   function Number_Of (The_Words : BCode_Words) return Natural is
      The_Count : Natural := 0;
   begin
      for The_Index in First_Index (The_Words) .. Last_Index (The_Words) loop
         The_Count := The_Count + Length_Of (Element (The_Words, The_Index));
      end loop;
      return The_Count;
   end Number_Of;

   -- Return bcode word solutions for the variable values.

   function Solve
     (The_Words       : BCode_Words;
      With_The_Values : Boolean_Array_Type) return Boolean_Array_Type
   is
      The_Result : Boolean_Array_Type (0 .. Number_Of (The_Words) - 1);
      The_Offset : Natural := 0;
      The_Word   : Word_Type;
   begin
      for The_Index in First_Index (The_Words) .. Last_Index (The_Words) loop
         The_Word := Element (The_Words, The_Index);

         The_Result (The_Offset .. The_Offset + Length_Of (The_Word) - 1) :=
           Solve (The_Word, With_The_Values);

         The_Offset := The_Offset + Length_Of (The_Word);
      end loop;
      return The_Result;
   end Solve;

   -- Return the string representation of boolean word values for the words
   -- seperated by a comma.

   function Boolean_Of (The_Words : BCode_Words) return String is
      The_String : Unbounded_String;
      The_Word   : Word_Type;
      First      : Boolean;
   begin
      First := True;
      for The_Index in First_Index (The_Words) .. Last_Index (The_Words) loop
         if First then
            First := False;
         else
            Append (The_String, ",");
         end if;
         The_Word := Element (The_Words, The_Index);
         Append (The_String, Boolean_Of (The_Word));
      end loop;
      return To_String (The_String);
   end Boolean_Of;

   -- Return the string representation of fundamental product terms
   -- for the words seperated by a comma.

   function Image_Of (The_Words : BCode_Words) return String is
      The_String : Unbounded_String;
      The_Word   : Word_Type;
      First      : Boolean;
   begin
      First := True;
      for The_Index in First_Index (The_Words) .. Last_Index (The_Words) loop
         if First then
            First := False;
         else
            Append (The_String, ",");
         end if;

         The_Word := Element (The_Words, The_Index);
         Append (The_String, Image_Of (The_Word));
      end loop;
      return To_String (The_String);
   end Image_Of;

   -- Iterate over the bcode words applying the procedure on each.

   procedure Iterate
     (The_Words : BCode_Words;
      Process   : not null access procedure (The_Word : Word_Type))
   is
      The_Cursor : Cursor;
   begin
      The_Cursor := First (The_Words);
      while The_Cursor /= No_Element loop
         Process (Element (The_Cursor));
         The_Cursor := Next (The_Cursor);
      end loop;
   end Iterate;

end BCode_Package;
