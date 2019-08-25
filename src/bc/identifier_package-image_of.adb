-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with Type_Package.Image_Of;

-- Return string representation of the identifier.

function Identifier_Package.Image_Of (The_Identifier : Identifier_Pointer) return String is
begin
   if The_Identifier = null then
      return "{Identifier *null*)";
   end if;

   if Is_Package (The_Identifier) then
      return "{Package_Identifier " &
        To_String (The_Identifier.The_String) &
        "}";
   elsif Is_Procedure (The_Identifier) then
      return "{Procedure_Identifier " &
        To_String (The_Identifier.The_String) &
        "}";
   elsif Is_Type (The_Identifier) then
      return "{Type_Identifier " &
        To_String (The_Identifier.The_String) &
        "; " &
        Image_Of (Type_Identifier (The_Identifier.all).The_Type) &
        "}";
   elsif Is_Variable (The_Identifier) then
      return "{Variable_Identifier " &
        To_String (The_Identifier.The_String) &
        "; " &
        Image_Of (Variable_Identifier (The_Identifier.all).The_Type) &
        "; " & -- initial value
        SYSInteger'Image (Variable_Identifier (The_Identifier.all).The_Value) &
        "; " &
        SYSNatural'Image
        (Variable_Identifier (The_Identifier.all).The_Address) &
        "}";
   elsif Is_Index (The_Identifier) then
      return "{Index_Identifier " &
        To_String (The_Identifier.The_String) &
        "; " &
        Image_Of (Index_Identifier (The_Identifier.all).The_Type) &
        "; " &
        SYSNatural'Image (Index_Identifier (The_Identifier.all).The_Address) &
        "}";
   elsif Is_Parameter (The_Identifier) then
      return "{Parameter_Identifier " &
        To_String (The_Identifier.The_String) &
        "; " &
        Image_Of (Parameter_Identifier (The_Identifier.all).The_Type) &
        "; " &
        Boolean'Image (Parameter_Identifier (The_Identifier.all).Is_In) &
        "; " &
        Boolean'Image (Parameter_Identifier (The_Identifier.all).Is_Out) &
        "; " &
        SYSNatural'Image
        (Parameter_Identifier (The_Identifier.all).The_Address) &
        "}";
   elsif Is_Constant (The_Identifier) then
      return "{Constant_Identifier " &
        To_String (The_Identifier.The_String) &
        "; " &
        Image_Of (Constant_Identifier (The_Identifier.all).The_Type) &
        "; " &
        SYSInteger'Image (Constant_Identifier (The_Identifier.all).The_Value) &
        "}";
   else
      raise Critical_Error;
   end if;
end Identifier_Package.Image_Of;
