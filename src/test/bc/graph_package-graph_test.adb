-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with AUnit.Assertions;
--
with Test_Package;
--
with Ada.Unchecked_Deallocation;
--
with Ada.Text_IO;
--
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--
with Pool_Package;
--
with System_Package;     use System_Package;
with Identifier_Package; use Identifier_Package;
with Boolean_Package;    use Boolean_Package;
with Number_Package;     use Number_Package;
with Word_Package;       use Word_Package;
--

package body Graph_Package.Graph_Test is

   The_Unmarked_Operand_Allocations : SYSNatural;
   The_Unmarked_Graph_Allocations   : SYSNatural;

   ----------
   -- Name --
   ----------

   overriding function Name (The_Test : in Test) return AUnit.Test_String is
   begin
      return AUnit.Format ("graph_package.graph_test!");
   end Name;

   --------------------
   -- Register_Tests --
   --------------------

   overriding procedure Register_Tests (The_Test : in out Test) is
      use Registration;
   begin
      Register_Routine(The_Test, Test_Identifier_Symbol_Graph'Access, "test_identifier_symbol_graph!");
      Register_Routine(The_Test, Test_Variable_Graph'Access, "test_variable_graph!");
      Register_Routine(The_Test, Test_Integer_Expression_Graph'Access, "test_integer_expression_graph!");
      Register_Routine(The_Test, Test_Variable_Expression_Graph'Access, "test_variable_expression_graph!");
      Register_Routine(The_Test, Test_Attribute_Expression_Graph'Access, "test_attribute_expression_graph!");
      Register_Routine(The_Test, Test_Unary_Expression_Graph'Access, "test_unary_expression_graph!");
      Register_Routine(The_Test, Test_Binary_Expression_Graph'Access, "test_binary_expression_graph!");
      Register_Routine(The_Test, Test_Operand_Expression_Graph'Access, "test_operand_expression_graph!");
      Register_Routine(The_Test, Test_Array_Definition_Graph'Access, "test_array_definition_graph!");
      Register_Routine(The_Test, Test_Range_Definition_Graph'Access, "test_range_definition_graph!");
      Register_Routine(The_Test, Test_Mod_Definition_Graph'Access, "test_mod_definition_graph!");
      Register_Routine(The_Test, Test_Identifier_Declaration_Graph'Access, "test_identifier_declaration_graph!");
      Register_Routine(The_Test, Test_Type_Declaration_Graph'Access, "test_type_declaration_graph!");
      Register_Routine(The_Test, Test_Assignment_Statement_Graph'Access, "test_assignment_statement_graph!");
      Register_Routine(The_Test, Test_Null_Statement_Graph'Access, "test_null_statement_graph!");
      Register_Routine(The_Test, Test_If_Statement_Graph'Access, "test_if_statement_graph!");
      Register_Routine(The_Test, Test_For_Statement_Graph'Access, "test_for_statement_graph!");
      Register_Routine(The_Test, Test_Parameter_Graph'Access, "test_parameter_graph!");
      Register_Routine(The_Test, Test_Procedure_Body_Graph'Access, "test_procedure_body_graph!");
      Register_Routine(The_Test, Test_Package_Body_Graph'Access, "test_package_body_graph!");
      Register_Routine(The_Test, Test_Compilation_Unit_Graph'Access, "test_compilation_unit_graph!");
   end Register_Tests;

   -----------------
   -- Set_Up_Case --
   -----------------

   overriding procedure Set_Up_Case (The_Test : in out Test) is
      pragma Unreferenced (The_Test);

   begin
      The_Unmarked_Operand_Allocations :=
        Pool_Package.Unmarked_Allocations (Operand_Package.The_Pool);
      The_Unmarked_Graph_Allocations :=
        Pool_Package.Unmarked_Allocations (Graph_Package.The_Pool);
   end Set_Up_Case;

   --------------------
   -- Tear_Down_Case --
   --------------------

   overriding procedure Tear_Down_Case (The_Test : in out Test) is
      pragma Unreferenced (The_Test);

   begin
      null;
   end Tear_Down_Case;

   ------------
   -- Set_Up --
   ------------

   overriding procedure Set_Up (The_Test : in out Test) is
      pragma Unreferenced (The_Test);

   begin
      null;
   end Set_Up;

   ---------------
   -- Tear_Down --
   ---------------

   overriding procedure Tear_Down (The_Test : in out Test) is
      pragma Unreferenced (The_Test);

   begin
      null;
   end Tear_Down;

   ----------------------------------
   -- Test_Identifier_Symbol_Graph --
   ----------------------------------

   procedure Test_Identifier_Symbol_Graph (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      The_Graph : Identifier_Symbol_Graph;
   begin
      The_Graph :=
        new Identifier_Symbol_Node'
          (The_String   => Null_Unbounded_String,
           The_Position => (1, 1),
           The_Pointer  => null);

      AUnit.Assertions.Assert
        (Position_Of (The_Graph) = (1, 1),
         "Test position of.");
      Dispose (The_Graph);

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Operand_Package.The_Pool) =
             The_Unmarked_Operand_Allocations,
         "Incorrect operand allocations.");
      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (The_Pool) =
             The_Unmarked_Graph_Allocations,
         "Incorrect graph allocations.");
   end Test_Identifier_Symbol_Graph;

   -------------------------
   -- Test_Variable_Graph --
   -------------------------

   procedure Test_Variable_Graph (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      The_Graph : Variable_Graph;
   begin
      The_Graph :=
        new Variable_Node'
          (The_Identifier =>
              new Identifier_Symbol_Node'(Null_Unbounded_String, (1, 1), null),
           The_Expression =>
              new Integer_Expression_Node'(null, Null_Unbounded_String, (1, 1)),
           The_Result => new Constant_Operand'(null, 0));

      AUnit.Assertions.Assert
        (Position_Of (The_Graph) = (1, 1),
         "Test position of.");
      Dispose (The_Graph);

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Operand_Package.The_Pool) =
             The_Unmarked_Operand_Allocations,
         "Incorrect operand allocations.");
      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (The_Pool) =
             The_Unmarked_Graph_Allocations,
         "Incorrect graph allocations.");

      The_Graph :=
        new Variable_Node'
          (The_Identifier =>
              new Identifier_Symbol_Node'(Null_Unbounded_String, (1, 1), null),
           The_Expression => null,
           The_Result     => new Constant_Operand'(null, 0));

      AUnit.Assertions.Assert
        (Position_Of (The_Graph) = (1, 1),
         "Test position of.");
      Dispose (The_Graph);

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Operand_Package.The_Pool) =
             The_Unmarked_Operand_Allocations,
         "Incorrect operand allocations.");
      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (The_Pool) =
             The_Unmarked_Graph_Allocations,
         "Incorrect graph allocations.");
   end Test_Variable_Graph;

   -----------------------------------
   -- Test_Integer_Expression_Graph --
   -----------------------------------

   procedure Test_Integer_Expression_Graph
     (The_Test : in out Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      The_Graph : Expression_Graph;
   begin
      The_Graph :=
        new Integer_Expression_Node'
          (The_Result   => new Constant_Operand'(null, 0),
           The_String   => Null_Unbounded_String,
           The_Position => (1, 1));

      AUnit.Assertions.Assert
        (Position_Of (The_Graph) = (1, 1),
         "Test position of.");
      Dispose (The_Graph);

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Operand_Package.The_Pool) =
             The_Unmarked_Operand_Allocations,
         "Incorrect operand allocations.");
      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (The_Pool) =
             The_Unmarked_Graph_Allocations,
         "Incorrect graph allocations.");
   end Test_Integer_Expression_Graph;

   ------------------------------------
   -- Test_Variable_Expression_Graph --
   ------------------------------------

   procedure Test_Variable_Expression_Graph
     (The_Test : in out Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      The_Graph : Expression_Graph;
   begin
      The_Graph :=
        new Variable_Expression_Node'
          (The_Result   => new Constant_Operand'(null, 0),
           The_Variable =>
              new Variable_Node'
             (new Identifier_Symbol_Node'
                (Null_Unbounded_String, (1, 1), null),
              null,
              null));

      AUnit.Assertions.Assert
        (Position_Of (The_Graph) = (1, 1),
         "Test position of.");
      Dispose (The_Graph);

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Operand_Package.The_Pool) =
             The_Unmarked_Operand_Allocations,
         "Incorrect operand allocations.");
      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (The_Pool) =
             The_Unmarked_Graph_Allocations,
         "Incorrect graph allocations.");
   end Test_Variable_Expression_Graph;

   -------------------------------------
   -- Test_Attribute_Expression_Graph --
   -------------------------------------

   procedure Test_Attribute_Expression_Graph
     (The_Test : in out Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      The_Graph : Expression_Graph;
   begin
      The_Graph :=
        new Attribute_Expression_Node'
          (The_Result   => new Constant_Operand'(null, 0),
           The_Identifier => new Identifier_Symbol_Node'
             (Null_Unbounded_String, (1, 1), null),
           The_String   => Null_Unbounded_String,
           The_Position => (1, 2));

      AUnit.Assertions.Assert
        (Position_Of (The_Graph) = (1, 2),
         "Test position of.");
      Dispose (The_Graph);

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Operand_Package.The_Pool) =
             The_Unmarked_Operand_Allocations,
         "Incorrect operand allocations.");
      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (The_Pool) =
             The_Unmarked_Graph_Allocations,
         "Incorrect graph allocations.");
   end Test_Attribute_Expression_Graph;

   ---------------------------------
   -- Test_Unary_Expression_Graph --
   ---------------------------------

   procedure Test_Unary_Expression_Graph (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      The_Graph : Expression_Graph;
   begin
      The_Graph :=
        new Unary_Expression_Node'
          (The_Result   => new Constant_Operand'(null, 0),
           The_Operator => Plus_Symbol,
           The_Right    =>
              new Integer_Expression_Node'(null, Null_Unbounded_String, (1, 1)));

      AUnit.Assertions.Assert
        (Position_Of (The_Graph) = (1, 1),
         "Test position of.");
      Dispose (The_Graph);

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Operand_Package.The_Pool) =
             The_Unmarked_Operand_Allocations,
         "Incorrect operand allocations.");
      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (The_Pool) =
             The_Unmarked_Graph_Allocations,
         "Incorrect graph allocations.");
   end Test_Unary_Expression_Graph;

   ----------------------------------
   -- Test_Binary_Expression_Graph --
   ----------------------------------

   procedure Test_Binary_Expression_Graph (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      The_Graph : Expression_Graph;
   begin
      The_Graph :=
        new Binary_Expression_Node'
          (The_Result   => new Constant_Operand'(null, 0),
           The_Operator => Plus_Symbol,
           The_Left     =>
              new Integer_Expression_Node'(null, Null_Unbounded_String, (2, 2)),
           The_Right =>
              new Integer_Expression_Node'(null, Null_Unbounded_String, (1, 1)));

      AUnit.Assertions.Assert
        (Position_Of (The_Graph) = (1, 1),
         "Test position of.");
      Dispose (The_Graph);

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Operand_Package.The_Pool) =
             The_Unmarked_Operand_Allocations,
         "Incorrect operand allocations.");
      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (The_Pool) =
             The_Unmarked_Graph_Allocations,
         "Incorrect graph allocations.");
   end Test_Binary_Expression_Graph;

   -----------------------------------
   -- Test_Operand_Expression_Graph --
   -----------------------------------

   procedure Test_Operand_Expression_Graph
     (The_Test : in out Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      The_Graph : Expression_Graph;
   begin
      The_Graph :=
        new Operand_Expression_Node'
          (The_Result   => new Constant_Operand'(null, 0),
           The_Position => (1, 1));

      AUnit.Assertions.Assert
        (Position_Of (The_Graph) = (1, 1),
         "Test position of.");
      Dispose (The_Graph);

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Operand_Package.The_Pool) =
             The_Unmarked_Operand_Allocations,
         "Incorrect operand allocations.");
      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (The_Pool) =
             The_Unmarked_Graph_Allocations,
         "Incorrect graph allocations.");
   end Test_Operand_Expression_Graph;

   ---------------------------------
   -- Test_Array_Definition_Graph --
   ---------------------------------

   procedure Test_Array_Definition_Graph (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      The_Graph : Definition_Graph;
   begin
      The_Graph :=
        new Array_Definition_Node'
          (The_Index =>
              new Identifier_Symbol_Node'(Null_Unbounded_String, (1, 2), null),
           The_First =>
              new Integer_Expression_Node'(null, Null_Unbounded_String, (1, 3)),
           The_Last =>
              new Integer_Expression_Node'(null, Null_Unbounded_String, (1, 4)),
           The_Element =>
              new Identifier_Symbol_Node'(Null_Unbounded_String, (1, 1), null));

      AUnit.Assertions.Assert
        (Position_Of (The_Graph) = (1, 1),
         "Test position of.");
      Dispose (The_Graph);

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Operand_Package.The_Pool) =
             The_Unmarked_Operand_Allocations,
         "Incorrect operand allocations.");
      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (The_Pool) =
             The_Unmarked_Graph_Allocations,
         "Incorrect graph allocations.");
   end Test_Array_Definition_Graph;

   ---------------------------------
   -- Test_Range_Definition_Graph --
   ---------------------------------

   procedure Test_Range_Definition_Graph (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      The_Graph : Definition_Graph;
   begin
      The_Graph :=
        new Range_Definition_Node'
          (The_Identifier =>
              new Identifier_Symbol_Node'(Null_Unbounded_String, (1, 1), null),
           The_First => null,
           The_Last  => null);

      AUnit.Assertions.Assert
        (Position_Of (The_Graph) = (1, 1),
         "Test position of.");
      Dispose (The_Graph);

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Operand_Package.The_Pool) =
             The_Unmarked_Operand_Allocations,
         "Incorrect operand allocations.");
      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (The_Pool) =
             The_Unmarked_Graph_Allocations,
         "Incorrect graph allocations.");

      The_Graph :=
        new Range_Definition_Node'
          (The_Identifier =>
              new Identifier_Symbol_Node'(Null_Unbounded_String, (1, 2), null),
           The_First =>
              new Integer_Expression_Node'(null, Null_Unbounded_String, (1, 3)),
           The_Last =>
              new Integer_Expression_Node'(null, Null_Unbounded_String, (1, 4)));

      AUnit.Assertions.Assert
        (Position_Of (The_Graph) = (1, 4),
         "Test position of.");
      Dispose (The_Graph);

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Operand_Package.The_Pool) =
             The_Unmarked_Operand_Allocations,
         "Incorrect operand allocations.");
      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (The_Pool) =
             The_Unmarked_Graph_Allocations,
         "Incorrect graph allocations.");
   end Test_Range_Definition_Graph;

   -------------------------------
   -- Test_Mod_Definition_Graph --
   -------------------------------

   procedure Test_Mod_Definition_Graph (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      The_Graph : Definition_Graph;
   begin
      The_Graph :=
        new Mod_Definition_Node'
          (The_Expression =>
              new Integer_Expression_Node'(null, Null_Unbounded_String, (1, 1)));

      AUnit.Assertions.Assert
        (Position_Of (The_Graph) = (1, 1),
         "Test position of.");
      Dispose (The_Graph);

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Operand_Package.The_Pool) =
             The_Unmarked_Operand_Allocations,
         "Incorrect operand allocations.");
      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (The_Pool) =
             The_Unmarked_Graph_Allocations,
         "Incorrect graph allocations.");
   end Test_Mod_Definition_Graph;

   ---------------------------------------
   -- Test_Identifier_Declaration_Graph --
   ---------------------------------------

   procedure Test_Identifier_Declaration_Graph
     (The_Test : in out Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      The_Graph : Declaration_Graph;
   begin
      The_Graph :=
        new Identifier_Declaration_Node'
          (The_Next       => null,
           The_Identifier =>
              new Identifier_Symbol_Node'(Null_Unbounded_String, (1, 2), null),
           Is_Constant    => False,
           The_Definition =>
              new Identifier_Symbol_Node'(Null_Unbounded_String, (1, 1), null),
           The_Expression => null);

      AUnit.Assertions.Assert
        (Position_Of (The_Graph) = (1, 1),
         "Test position of.");
      Dispose (The_Graph);

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Operand_Package.The_Pool) =
             The_Unmarked_Operand_Allocations,
         "Incorrect operand allocations.");
      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (The_Pool) =
             The_Unmarked_Graph_Allocations,
         "Incorrect graph allocations.");

      The_Graph :=
        new Identifier_Declaration_Node'
          (The_Next       => null,
           The_Identifier =>
              new Identifier_Symbol_Node'(Null_Unbounded_String, (1, 2), null),
           Is_Constant    => False,
           The_Definition =>
              new Identifier_Symbol_Node'(Null_Unbounded_String, (1, 2), null),
           The_Expression =>
              new Integer_Expression_Node'(null, Null_Unbounded_String, (1, 1)));

      AUnit.Assertions.Assert
        (Position_Of (The_Graph) = (1, 1),
         "Test position of.");
      Dispose (The_Graph);

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Operand_Package.The_Pool) =
             The_Unmarked_Operand_Allocations,
         "Incorrect operand allocations.");
      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (The_Pool) =
             The_Unmarked_Graph_Allocations,
         "Incorrect graph allocations.");

      The_Graph :=
        new Identifier_Declaration_Node'
          (The_Next =>
              new Identifier_Declaration_Node'
             (The_Next       => null,
              The_Identifier => null,
              Is_Constant    => False,
              The_Definition => null,
              The_Expression => null),
           The_Identifier =>
              new Identifier_Symbol_Node'(Null_Unbounded_String, (1, 2), null),
           Is_Constant    => False,
           The_Definition =>
              new Identifier_Symbol_Node'(Null_Unbounded_String, (1, 2), null),
           The_Expression =>
              new Integer_Expression_Node'(null, Null_Unbounded_String, (1, 1)));

      AUnit.Assertions.Assert
        (Position_Of (The_Graph) = (1, 1),
         "Test position of.");
      Dispose (The_Graph);

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Operand_Package.The_Pool) =
             The_Unmarked_Operand_Allocations,
         "Incorrect operand allocations.");
      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (The_Pool) =
             The_Unmarked_Graph_Allocations,
         "Incorrect graph allocations.");
   end Test_Identifier_Declaration_Graph;

   ---------------------------------
   -- Test_Type_Declaration_Graph --
   ---------------------------------

   procedure Test_Type_Declaration_Graph (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      The_Graph : Declaration_Graph;
   begin
      The_Graph :=
        new Type_Declaration_Node'
          (The_Next       => null,
           The_Identifier =>
              new Identifier_Symbol_Node'(Null_Unbounded_String, (1, 2), null),
           The_Definition =>
              new Mod_Definition_Node'
             (The_Expression =>
                     new Integer_Expression_Node'
                (null, Null_Unbounded_String, (1, 1))));

      AUnit.Assertions.Assert
        (Position_Of (The_Graph) = (1, 1),
         "Test position of.");
      Dispose (The_Graph);

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Operand_Package.The_Pool) =
             The_Unmarked_Operand_Allocations,
         "Incorrect operand allocations.");
      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (The_Pool) =
             The_Unmarked_Graph_Allocations,
         "Incorrect graph allocations.");
   end Test_Type_Declaration_Graph;

   -------------------------------------
   -- Test_Assignment_Statement_Graph --
   -------------------------------------

   procedure Test_Assignment_Statement_Graph
     (The_Test : in out Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      The_Graph : Statement_Graph;
   begin
      The_Graph :=
        new Assignment_Statement_Node'
          (The_Next     => new Null_Statement_Node'(The_Next => null),
           The_Variable =>
              new Variable_Node'
             (The_Identifier => null,
              The_Expression => null,
              The_Result     => new Constant_Operand'(null, 0)),
           The_Expression =>
              new Integer_Expression_Node'(null, Null_Unbounded_String, (1, 1)));

      AUnit.Assertions.Assert
        (Position_Of (Assignment_Statement_Graph (The_Graph)) = (1, 1),
         "Test position of.");
      Dispose (The_Graph);

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Operand_Package.The_Pool) =
             The_Unmarked_Operand_Allocations,
         "Incorrect operand allocations.");
      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (The_Pool) =
             The_Unmarked_Graph_Allocations,
         "Incorrect graph allocations.");
   end Test_Assignment_Statement_Graph;

   -------------------------------
   -- Test_Null_Statement_Graph --
   -------------------------------

   procedure Test_Null_Statement_Graph (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      The_Graph : Statement_Graph;
   begin
      The_Graph :=
        new Null_Statement_Node'
          (The_Next => new Null_Statement_Node'(The_Next => null));
      Dispose (The_Graph);

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Operand_Package.The_Pool) =
             The_Unmarked_Operand_Allocations,
         "Incorrect operand allocations.");
      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (The_Pool) =
             The_Unmarked_Graph_Allocations,
         "Incorrect graph allocations.");
   end Test_Null_Statement_Graph;

   -----------------------------
   -- Test_If_Statement_Graph --
   -----------------------------

   procedure Test_If_Statement_Graph (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      The_Graph : Statement_Graph;
   begin
      The_Graph :=
        new If_Statement_Node'
          (The_Next       => new Null_Statement_Node'(The_Next => null),
           The_Expression =>
              new Integer_Expression_Node'(null, Null_Unbounded_String, (1, 1)),
           The_Statements => new Null_Statement_Node'(The_Next => null),
           The_Alternates => new Null_Statement_Node'(The_Next => null));

      AUnit.Assertions.Assert
        (Position_Of (If_Statement_Graph (The_Graph)) = (1, 1),
         "Test position of.");
      Dispose (The_Graph);

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Operand_Package.The_Pool) =
             The_Unmarked_Operand_Allocations,
         "Incorrect operand allocations.");
      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (The_Pool) =
             The_Unmarked_Graph_Allocations,
         "Incorrect graph allocations.");
   end Test_If_Statement_Graph;

   ------------------------------
   -- Test_For_Statement_Graph --
   ------------------------------

   procedure Test_For_Statement_Graph (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      The_Graph : Statement_Graph;
   begin
      The_Graph :=
        new For_Statement_Node'
          (The_Next  => new Null_Statement_Node'(The_Next => null),
           The_Index =>
              new Identifier_Symbol_Node'(Null_Unbounded_String, (1, 2), null),
           The_Definition =>
              new Identifier_Symbol_Node'(Null_Unbounded_String, (1, 2), null),
           Is_Reverse => False,
           The_First  =>
              new Integer_Expression_Node'(null, Null_Unbounded_String, (2, 2)),
           The_Last =>
              new Integer_Expression_Node'(null, Null_Unbounded_String, (1, 1)),
           The_Statements => new Null_Statement_Node'(The_Next => null));

      AUnit.Assertions.Assert
        (Position_Of (For_Statement_Graph (The_Graph)) = (1, 1),
         "Test position of.");
      Dispose (The_Graph);

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Operand_Package.The_Pool) =
             The_Unmarked_Operand_Allocations,
         "Incorrect operand allocations.");
      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (The_Pool) =
             The_Unmarked_Graph_Allocations,
         "Incorrect graph allocations.");
   end Test_For_Statement_Graph;

   --------------------------
   -- Test_Parameter_Graph --
   --------------------------

   procedure Test_Parameter_Graph (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      The_Graph : Parameter_Graph;
   begin
      The_Graph :=
        new Parameter_Node'
          (The_Identifier =>
              new Identifier_Symbol_Node'(Null_Unbounded_String, (1, 2), null),
           Is_In          => False,
           Is_Out         => False,
           The_Definition =>
              new Identifier_Symbol_Node'(Null_Unbounded_String, (1, 1), null),
           The_Next => null);
      Dispose (The_Graph);

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Operand_Package.The_Pool) =
             The_Unmarked_Operand_Allocations,
         "Incorrect operand allocations.");
      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (The_Pool) =
             The_Unmarked_Graph_Allocations,
         "Incorrect graph allocations.");

      The_Graph :=
        new Parameter_Node'
          (The_Identifier =>
              new Identifier_Symbol_Node'(Null_Unbounded_String, (1, 2), null),
           Is_In          => False,
           Is_Out         => False,
           The_Definition =>
              new Identifier_Symbol_Node'(Null_Unbounded_String, (1, 2), null),
           The_Next => new Parameter_Node'(null, False, False, null, null));
      Dispose (The_Graph);

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Operand_Package.The_Pool) =
             The_Unmarked_Operand_Allocations,
         "Incorrect operand allocations.");
      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (The_Pool) =
             The_Unmarked_Graph_Allocations,
         "Incorrect graph allocations.");
   end Test_Parameter_Graph;

   -------------------------------
   -- Test_Procedure_Body_Graph --
   -------------------------------

   procedure Test_Procedure_Body_Graph (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      The_Graph : Procedure_Body_Graph;
   begin
      The_Graph :=
        new Procedure_Body_Node'
          (The_Name =>
              new Identifier_Symbol_Node'(Null_Unbounded_String, (1, 2), null),
           The_Parameters =>
              new Parameter_Node'(null, False, False, null, null),
           The_Statements => new Null_Statement_Node'(The_Next => null),
           The_Identifier =>
              new Identifier_Symbol_Node'(Null_Unbounded_String, (1, 2), null));
      Dispose (The_Graph);

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Operand_Package.The_Pool) =
             The_Unmarked_Operand_Allocations,
         "Incorrect operand allocations.");
      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (The_Pool) =
             The_Unmarked_Graph_Allocations,
         "Incorrect graph allocations.");
   end Test_Procedure_Body_Graph;

   -----------------------------
   -- Test_Package_Body_Graph --
   -----------------------------

   procedure Test_Package_Body_Graph (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      The_Graph : Package_Body_Graph;
   begin
      The_Graph :=
        new Package_Body_Node'
          (The_Name =>
              new Identifier_Symbol_Node'(Null_Unbounded_String, (1, 2), null),
           The_Declarations =>
              new Identifier_Declaration_Node'(null, null, False, null, null),
           The_Procedure  => new Procedure_Body_Node'(null, null, null, null),
           The_Identifier =>
              new Identifier_Symbol_Node'(Null_Unbounded_String, (1, 2), null));
      Dispose (The_Graph);

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Operand_Package.The_Pool) =
             The_Unmarked_Operand_Allocations,
         "Incorrect operand allocations.");
      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (The_Pool) =
             The_Unmarked_Graph_Allocations,
         "Incorrect graph allocations.");
   end Test_Package_Body_Graph;

   ---------------------------------
   -- Test_Compilation_Unit_Graph --
   ---------------------------------

   procedure Test_Compilation_Unit_Graph (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      The_Graph : Compilation_Unit_Graph;
   begin
      The_Graph :=
        new Compilation_Unit_Node'
          (The_Package => new Package_Body_Node'(null, null, null, null));
      Dispose (The_Graph);

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Operand_Package.The_Pool) =
             The_Unmarked_Operand_Allocations,
         "Incorrect operand allocations.");
      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (The_Pool) =
             The_Unmarked_Graph_Allocations,
         "Incorrect graph allocations.");
   end Test_Compilation_Unit_Graph;

end Graph_Package.Graph_Test;
