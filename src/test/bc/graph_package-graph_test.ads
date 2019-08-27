-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with AUnit;
with AUnit.Test_Fixtures;
--
with AUnit.Test_Cases; use AUnit.Test_Cases;
--

package Graph_Package.Graph_Test is

   type Test is new Test_Case with null record;

   overriding function Name (The_Test : in Test) return AUnit.Test_String;
   overriding procedure Register_Tests (The_Test : in out Test);

   overriding procedure Set_Up_Case (The_Test : in out Test);
   overriding procedure Tear_Down_Case (The_Test : in out Test);
   overriding procedure Set_Up (The_Test : in out Test);
   overriding procedure Tear_Down (The_Test : in out Test);

   --  Identifier_Symbol_Graph

   --  expression

   --  Variable_Graph
   --  Integer_Expression_Graph
   --  Variable_Expression_Graph
   --  Attribute_Expression_Graph
   --  Unary_Expression_Graph
   --  Binary_Expression_Graph
   --  Operand_Expression_Graph

   --  definition

   --  Array_Definition_Graph
   --  Range_Definition_Graph
   --  Mod_Definition_Graph

   --  declaration

   --  Identifier_Declaration_Graph Type_Declaration_Graph

   --  statement

   --  Assignment_Statement_Graph Null_Statement_Graph If_Statement_Graph
   --  For_Statement_Graph

   --  Parameter_Graph

   --  Procedure_Body_Graph

   --  Package_Body_Graph

   --  Compilation_Unit_Graph

   procedure Test_Identifier_Symbol_Graph (The_Test : in out Test_Case'Class);

   procedure Test_Variable_Graph (The_Test : in out Test_Case'Class);

   -- expression

   procedure Test_Integer_Expression_Graph (The_Test : in out Test_Case'Class);
   procedure Test_Variable_Expression_Graph (The_Test : in out Test_Case'Class);
   procedure Test_Attribute_Expression_Graph (The_Test : in out Test_Case'Class);
   procedure Test_Unary_Expression_Graph (The_Test : in out Test_Case'Class);
   procedure Test_Binary_Expression_Graph (The_Test : in out Test_Case'Class);
   procedure Test_Operand_Expression_Graph (The_Test : in out Test_Case'Class);

   -- definition

   procedure Test_Array_Definition_Graph (The_Test : in out Test_Case'Class);
   procedure Test_Range_Definition_Graph (The_Test : in out Test_Case'Class);
   procedure Test_Mod_Definition_Graph (The_Test : in out Test_Case'Class);

   -- declaration

   procedure Test_Identifier_Declaration_Graph
     (The_Test : in out Test_Case'Class);
   procedure Test_Type_Declaration_Graph (The_Test : in out Test_Case'Class);

   -- statement

   procedure Test_Assignment_Statement_Graph
     (The_Test : in out Test_Case'Class);
   procedure Test_Null_Statement_Graph (The_Test : in out Test_Case'Class);
   procedure Test_If_Statement_Graph (The_Test : in out Test_Case'Class);
   procedure Test_For_Statement_Graph (The_Test : in out Test_Case'Class);

   procedure Test_Parameter_Graph (The_Test : in out Test_Case'Class);

   procedure Test_Procedure_Body_Graph (The_Test : in out Test_Case'Class);

   procedure Test_Package_Body_Graph (The_Test : in out Test_Case'Class);

   procedure Test_Compilation_Unit_Graph (The_Test : in out Test_Case'Class);

end Graph_Package.Graph_Test;
