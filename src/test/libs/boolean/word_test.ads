-- BC Boolean Compiler
-- Copyright (c) 2015 Paul Estep

with AUnit;
with AUnit.Test_Fixtures;
with AUnit.Test_Cases;
--

package Word_Test is

   -- dump results
   Word_Dump : Boolean := False;

   type Test is new AUnit.Test_Cases.Test_Case with null record;

   overriding function Name (The_Test : in Test) return AUnit.Test_String;
   overriding procedure Register_Tests (The_Test : in out Test);

   overriding procedure Set_Up_Case (The_Test : in out Test);

   procedure Test_Not (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_And (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Or (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Xor (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
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
   procedure Test_Unsigned_Add
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Signed_Add
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Unsigned_Subtract
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Signed_Subtract
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Unsigned_Multiply
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Signed_Multiply
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
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

end Word_Test;
