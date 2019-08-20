-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

-- A pacakge to define operands for parsing. Operands are used to hold operand
-- data passed between language constructs.

package Operand_Package.Image_Package is

   -- Return string representation of operand.

   function Image_Of (The_Operand : Operand_Pointer) return String;

end Operand_Package.Image_Package;
