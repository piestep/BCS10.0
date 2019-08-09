-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with Ada.Sequential_IO;
with Ada.Unchecked_Conversion;
--
with System_Package; use System_Package;
--

-- A package to define storage file definitions.
--
-- A storage file is a sequential file of discreate storage values.

package Storage_Package with
     Spark_Mode => Off is

   -- The storage value discreate type.

   type Storage_Type is new SYSModular;

   -- An array of boolean values the size of a discreate storage type.

   type Storage_Boolean_Array_Type is
     array (SYSNatural range 0 .. Storage_Type'Size - 1) of Boolean;
   pragma Pack (Storage_Boolean_Array_Type);

   -- Convert a storage boolean array to a discreate storage type.

   function Storage_Boolean_Array_Type_To_Storage_Type is new Ada
     .Unchecked_Conversion
     (Storage_Boolean_Array_Type,
      Storage_Type);

   -- Convert a discreate storage type to a storage boolean array.

   function Storage_Type_To_Storage_Boolean_Array_Type is new Ada
     .Unchecked_Conversion
     (Storage_Type,
      Storage_Boolean_Array_Type);

   -- The sequential definition of a storage file.

   package Storage_IO is new Ada.Sequential_IO (Storage_Type);

end Storage_Package;
