-- BC Boolean Compiler
-- Copyright (c) 2015 Paul Estep

with AUnit;
with AUnit.Test_Fixtures;
with AUnit.Test_Cases;
--

package Word_Package.Word_Test is

   type Test is new AUnit.Test_Cases.Test_Case with null record;

   overriding function Name (The_Test : in Test) return AUnit.Test_String;
   overriding procedure Register_Tests (The_Test : in out Test);

   overriding procedure Set_Up (The_Test : in out Test);

   procedure Test_Create_Constant_With_Boolean_Array
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Create_Constant_With_Size
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Create_Variable
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Dispose (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Copy (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Append_With_Equation
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Append_With_Word
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Fill (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Shift_Left
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Shift_Right
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Not_Op (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_And_Op (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Or_Op (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Xor_Op (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Not_Equal
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Unsigned_Less_Than
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Signed_Less_Than
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Unsigned_Greater_Than
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Signed_Greater_Than
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Negate (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Add (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Subtract (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Multiply (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Unsigned_Divide
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Signed_Divide
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Unsigned_Remainder
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Signed_Remainder
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Assign_Element
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Access_Element
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_If_Else (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Convert (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Normalize
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Normalize_With_Variables
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Is_Constant
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_To_Constant_Modular
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_To_Constant_Boolean_Array
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Variables_Of
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Length_Of
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Solve (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Boolean_Of
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Image_Of (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Iterate (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Pool (The_Test : in out AUnit.Test_Cases.Test_Case'Class);

end Word_Package.Word_Test;
