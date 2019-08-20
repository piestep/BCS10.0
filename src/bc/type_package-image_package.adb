-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

package body Type_Package.Image_Package is

   -- Return string representation of type.

   function Image_Of (The_Type : Type_Pointer) return String is
   begin
      if The_Type = null then
         return "Type *null*";
      end if;

      if The_Type = Universal_Boolean then
         return "(Universal_Boolean)";

      elsif The_Type = Universal_Integer then
         return "(Universal_Integer)";

      elsif Is_Discrete (The_Type) then
         return "(Discrete_Type " &
           SYSInteger'Image (First_Of (The_Type)) & "," &
           SYSInteger'Image (Last_Of (The_Type)) & "," &
           SYSInteger'Image (Size_Of (The_Type)) & "." &
           " " &
           Image_Of (The_Type.The_Base) &
           ")";

      elsif Is_Signed (The_Type) then
         return "(Signed_Type " &
           SYSInteger'Image (First_Of (The_Type)) & "," &
           SYSInteger'Image (Last_Of (The_Type)) & "," &
           SYSInteger'Image (Size_Of (The_Type)) & "." &
           " " &
           Image_Of (The_Type.The_Base) &
           ")";

      elsif Is_Modular (The_Type) then
         return "(Modular_Type " &
           SYSInteger'Image (First_Of (The_Type)) & "," &
           SYSInteger'Image (Last_Of (The_Type)) & "," &
           SYSInteger'Image (Modular_Type (The_Type.all).The_Modulas) & "," &
           SYSInteger'Image (Size_Of (The_Type)) & "." &
           " " &
           Image_Of (The_Type.The_Base) &
           ")";

      elsif Is_Array (The_Type) then
         return "(Array_Type " &
           Image_Of (Array_Type (The_Type.all).The_Index) & "," &
           SYSInteger'Image (Array_Type (The_Type.all).The_First) & "," &
           SYSInteger'Image (Array_Type (The_Type.all).The_Last) & "." &
           " " &
           Image_Of (Array_Type (The_Type.all).The_Element) &
           ")";

      else
         raise Critical_Error;
      end if;
   end Image_Of;

end Type_Package.Image_Package;
