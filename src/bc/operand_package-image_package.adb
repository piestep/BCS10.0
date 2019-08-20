-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with Type_Package.Image_Package;       use Type_Package.Image_Package;
with Identifier_Package.Image_Package; use Identifier_Package.Image_Package;
--

package body Operand_Package.Image_Package is

   -- Return string representation of operand.

   function Image_Of (The_Operand : Operand_Pointer) return String is
   begin
      if The_Operand = null then
         return "Operand *null*";
      end if;

      if Is_Constant (The_Operand) then
         return "[Constant_Operand " &
           Image_Of (Constant_Operand (The_Operand.all).The_Type) &
           " " &
           SYSInteger'Image (Constant_Operand (The_Operand.all).The_Value) &
           "]";
      elsif Is_Variable (The_Operand) then
         return "[Variable_Operand " &
           Image_Of (Variable_Operand (The_Operand.all).The_Type) &
           "]";
      elsif Is_Array (The_Operand) then
         return "[Array_Operand " &
           Image_Of (Array_Operand (The_Operand.all).The_Type) & ":" &
           Image_Of (Array_Operand (The_Operand.all).The_Identifier) & ":" &
           Image_Of (Array_Operand (The_Operand.all).The_Index) &
           "]";
      elsif Is_Identifier (The_Operand) then
         return "[Identifier_Operand " &
           Image_Of (Identifier_Operand (The_Operand.all).The_Type) & ":" &
           Image_Of (Identifier_Operand (The_Operand.all).The_Identifier) &
           "]";
      else
         raise Critical_Error;
      end if;
   end Image_Of;

end Operand_Package.Image_Package;
