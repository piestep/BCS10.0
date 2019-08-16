-- BC Boolean Compiler
-- Copyright (c) 2019 Paul Estep

with Ada.Containers.Vectors;
--
with System_Package;  use System_Package;
with Boolean_Package; use Boolean_Package;
--

-- A package for holding input operand length, first value,
-- last value, and output length for PCode and BCode comparison
-- test purposes.

package Parameter_Package is

   type Parameter_List_Type is private;

   -- return size of all parameters in parameter vector.

   function Size_Of (The_Parameters : in Parameter_List_Type) return SYSNatural;

   -- return all first discret values for a parameter vector.

   function First_Of (The_Parameters : in Parameter_List_Type) return Boolean_Array_Type;

   -- return all last discret values for a parameter vector.

   function Last_Of (The_Parameters : in Parameter_List_Type) return Boolean_Array_Type;

   -- increment the boolean values of a parameter vector to the next boolean value.

   procedure Increment
     (The_Values     : in out Boolean_Array_Type;
      The_Parameters :        Parameter_List_Type);

   -- format boolean values with the parameter vector.

   function Format
     (The_Values     : in Boolean_Array_Type;
      The_Parameters : in Parameter_List_Type) return String;

   -- append a scalar paramter to a parameter vector.

   procedure Append
     (The_Parameters : in out Parameter_List_Type;
      The_Size       : in     SYSNatural;
      The_First      : in     BCModular;
      The_Last       : in     BCModular;
      Is_Signed      : in     Boolean);

   -- append an array paramter to a parameter vector.

   procedure Append
     (The_Parameters : in out Parameter_List_Type;
      The_Size       : in     SYSNatural;
      The_First      : in     BCModular;
      The_Last       : in     BCModular;
      Is_Signed      : in     Boolean;
      The_Length     : in     SYSNatural);

private

   type Parameter_Kind is (Scalar_Parameter, Array_Parameter);

   type Parameter_Type (The_Kind : Parameter_Kind := Scalar_Parameter) is record
      The_Size  : SYSNatural;  -- size of scalar element
      The_First : BCModular;  -- first value
      The_Last  : BCModular;  -- last value
      Is_Signed : Boolean := False;

      case The_Kind is
         when Scalar_Parameter =>
            null;
         when Array_Parameter =>
            The_Length : SYSNatural;  -- length of array (initial index assumed to be 0)
      end case;
   end record;

   package Parameter_Vector is new Ada.Containers.Vectors
     (Index_Type   => SYSPositive,
      Element_Type => Parameter_Type);
   use Parameter_Vector;

   type Parameter_List_Type is new Parameter_Vector.Vector with null record;

   function Image_Of (The_Parameter : Parameter_Type) return String;

end Parameter_Package;
