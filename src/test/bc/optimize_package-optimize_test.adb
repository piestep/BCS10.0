-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with AUnit.Assertions;
--
with Ada.Text_IO;
with Ada.Directories;
--
with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
--
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--
with Test_Package;
with XML_Package;
--
with Error_Package.Clear;
with Graph_Package.Dump;
with Graph_Package.Count;
with List_Package;
with Pool_Package;
--
with System_Package;     use System_Package;
with Error_Package;      use Error_Package;
with Source_Package;     use Source_Package;
with Scanner_Package;    use Scanner_Package;
with Type_Package;       use Type_Package;
with Identifier_Package; use Identifier_Package;
with Graph_Package;      use Graph_Package;
with Syntax_Package;     use Syntax_Package;
with Semantics_Package;  use Semantics_Package;
with Scope_Package;      use Scope_Package;
--
with Scope_Package.Dump;

package body Optimize_Package.Optimize_Test is

   FILENAME : constant String := Test_Package.FILES & "/" & "optimize.bc";
   LISTNAME : constant String := Test_Package.FILES & "/" & "optimize.tmp";

   The_Unmarked_Graph_Allocations   : SYSNatural;
   The_Unmarked_Operand_Allocations : SYSNatural;

   ----------
   -- Name --
   ----------

   function Name (The_Test : in Test) return AUnit.Test_String is
   begin
      return AUnit.Format ("optimize_package.optimize_test!");
   end Name;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (The_Test : in out Test) is
      use Registration;

   begin
      -- Repeat for each test routine:
      Register_Routine(The_Test, Test_Procedure'Access, "test_procedure!");
      Register_Routine(The_Test, Test_Scalar'Access, "test_scalar!");
      Register_Routine(The_Test, Test_Mod'Access, "test_mod!");
      Register_Routine(The_Test, Test_Range'Access, "test_range!");
      Register_Routine(The_Test, Test_Array'Access, "test_array!");
      Register_Routine(The_Test, Test_Identifiers'Access, "test_identifiers!");
      Register_Routine(The_Test, Test_Assignment'Access, "test_assignment!");
      Register_Routine(The_Test, Test_If'Access, "test_if!");
      Register_Routine(The_Test, Test_For'Access, "test_for!");
      Register_Routine(The_Test, Test_Unary'Access, "test_unary!");
      Register_Routine(The_Test, Test_Relation'Access, "test_relation!");
      Register_Routine(The_Test, Test_And'Access, "test_and!");
      Register_Routine(The_Test, Test_Or'Access, "test_or!");
      Register_Routine(The_Test, Test_Xor'Access, "test_xor!");
      Register_Routine(The_Test, Test_Addition'Access, "test_addition!");
      Register_Routine(The_Test, Test_Subtraction'Access, "test_subtraction!");
      Register_Routine(The_Test, Test_Multiplication'Access, "test_multiplication!");
      Register_Routine(The_Test, Test_Division'Access, "test_division!");
      Register_Routine(The_Test, Test_Parameter'Access, "test_parameter!");
      Register_Routine(The_Test, Test_Index'Access, "test_index!");
      Register_Routine(The_Test, Test_Assign'Access, "test_assign!");
      Register_Routine(The_Test, Test_Assign_Left'Access, "test_assign_left!");
      Register_Routine(The_Test, Test_Assign_Right'Access, "test_assign_right!");
      Register_Routine(The_Test, Test_Variable_Errors'Access, "test_variable_errors!");
      Register_Routine(The_Test, Test_Unary_Expression_Errors'Access, "test_unary_expression_errors!");
      Register_Routine(The_Test, Test_Binary_Expression_Errors'Access, "test_binary_expression_errors!");
      --        Register_Routine(The_Test, Test_Optimize'Access, "test_optimize!");
   end Register_Tests;

   -----------------
   -- Set_Up_Case --
   -----------------

   overriding procedure Set_Up_Case (The_Test : in out Test) is
      pragma Unreferenced (The_Test);

   begin
      The_Unmarked_Graph_Allocations :=
        Pool_Package.Unmarked_Allocations (Graph_Package.The_Pool);
      The_Unmarked_Operand_Allocations :=
        Pool_Package.Unmarked_Allocations (Operand_Package.The_Pool);
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
      --        pragma Unreferenced (The_Test);

      The_Identifier : Identifier_Pointer;

   begin
      -- Open initial scope.

      Scope_Package.Open;

      The_Identifier :=
        new Identifier_Package.Type_Identifier'
          (The_Previous => Identifier_Package.The_Last,
           The_String => Scanner_Package.Boolean_String,
           The_Type   => Type_Package.Boolean_Type);
      Identifier_Package.The_Last := The_Identifier;
      Scope_Package.Enter (The_Identifier);

      The_Identifier :=
        new Identifier_Package.Constant_Identifier'
          (The_Previous => Identifier_Package.The_Last,
           The_String => Scanner_Package.False_String,
           The_Type   => Type_Package.Universal_Boolean,
           The_Value  => 0);
      Identifier_Package.The_Last := The_Identifier;
      Scope_Package.Enter (The_Identifier);

      The_Identifier := new Identifier_Package.Constant_Identifier'
        (The_Previous => Identifier_Package.The_Last,
         The_String => Scanner_Package.True_String,
         The_Type   => Type_Package.Universal_Boolean,
         The_Value  => 1);
      Identifier_Package.The_Last := The_Identifier;
      Scope_Package.Enter (The_Identifier);

      The_Identifier := new Identifier_Package.Type_Identifier'
        (The_Previous => Identifier_Package.The_Last,
         The_String => Scanner_Package.Integer_String,
         The_Type   => Type_Package.Integer_Type);
      Identifier_Package.The_Last := The_Identifier;
      Scope_Package.Enter (The_Identifier);
   end Set_Up;

   ---------------
   -- Tear_Down --
   ---------------

   overriding procedure Tear_Down (The_Test : in out Test) is
      --        pragma Unreferenced (The_Test);

   begin
      Scope_Package.Close;
      Type_Package.Clear;
      Identifier_Package.Clear;

      Ada.Text_IO.Put_Line("Optimize_Package.Optimize_Test " & Routine_Name(The_Test).all);
      Ada.Text_IO.Put_Line
        ("Type_Allocations: " &
           SYSNatural'Image(Pool_Package.Unmarked_Allocations (Type_Package.The_Pool)));
      Ada.Text_IO.Put_Line
        ("Identifier_Allocations: " &
           SYSNatural'Image(Pool_Package.Unmarked_Allocations (Identifier_Package.The_Pool)));
      Ada.Text_IO.Put_Line
        ("Operand_Allocations: " &
           SYSNatural'Image(Pool_Package.Unmarked_Allocations (Operand_Package.The_Pool)));
   end Tear_Down;

   --------------
   -- Run_Test --
   --------------

   procedure Run_Test
     (The_Test    : XML_Package.XML_Record;
      The_Message : String;
      Error_Test  : Boolean;
      Dump        : Boolean;
      Generate    : Boolean) is

      use XML_Package.Strings_Vector;

      Run : constant Boolean := not (Dump or Generate);
      Result : Boolean := False;
      The_File : Ada.Text_IO.File_Type;
      The_Unit : Compilation_Unit_Graph;

   begin
      --        Ada.Text_IO.Put_Line(To_String(The_Test.The_Name));
      --        The_Unmarked_Operand_Allocations :=
      --          Pool_Package.Unmarked_Allocations (Operand_Package.The_Pool);

      Error_Package.Clear;
      Test_Package.Save (FILENAME, The_Test.The_Code);

      Source_Package.Open (FILENAME);

      Scanner_Package.Next_Symbol;
      Syntax_Package.Parse (The_Unit);

      Source_Package.Close;

      if The_Number_Of_Errors = 0 and The_Number_Of_Warnings = 0 then
         Semantics_Package.Parse (The_Unit);
         if The_Number_Of_Errors = 0 and The_Number_Of_Warnings = 0 then

            Optimize (The_Unit);

            if Dump then
               List_Package.List (FILENAME, XML_Format => False);

               -- Delete code file
               Ada.Text_IO.Open (The_File, Ada.Text_IO.Out_File, FILENAME);
               Ada.Text_IO.Delete (The_File);

            elsif Generate then
               -- Delete code file
               Ada.Text_IO.Open (The_File, Ada.Text_IO.Out_File, FILENAME);
               Ada.Text_IO.Delete (The_File);

               -- generate file opened by calling test procedure
               Ada.Text_IO.New_Line;
               Ada.Text_IO.Put_Line ("<test>");
               Ada.Text_IO.Put_Line
                 ("<name>" & To_String (The_Test.The_Name) & "</name>");

               Test_Package.List (The_Test.The_Code, XML_Format => True);

               if Error_Test then
                  Error_Package.List_Messages (XML_Format => True);
               else
                  Graph_Package.Dump
                    (The_Unit,
                     Semantic_Dump => True,
                     Full_Dump     => True,
                     XML_Format    => True);
               end if;

               Ada.Text_IO.Put_Line ("</test>");

            else -- Run
               -- Delete code file
               Ada.Text_IO.Open (The_File, Ada.Text_IO.Out_File, FILENAME);
               Ada.Text_IO.Delete (The_File);

               if Error_Test then
                  -- create (temporary) listing file
                  Ada.Text_IO.Create (The_File, Ada.Text_IO.Out_File, LISTNAME);
                  Ada.Text_IO.Set_Output (The_File);

                  -- Save errors to listing file for comparison
                  Error_Package.List_Messages (XML_Format => False);

                  Ada.Text_IO.Set_Output (Ada.Text_IO.Standard_Output);
                  Ada.Text_IO.Close(The_File);

                  Result := Test_Package.Compare_First_Line_Of_File_To_String
                    (LISTNAME, To_String (Element (The_Test.The_Listing, 1)));

                  -- Delete listing file
                  Ada.Text_IO.Open(The_File, Ada.Text_IO.Out_File, LISTNAME);
                  Ada.Text_IO.Delete (The_File);

                  AUnit.Assertions.Assert
                    (Result, "Test " & The_Message & " error.");
               else
                  -- create (temporary) listing file
                  Ada.Text_IO.Create (The_File, Ada.Text_IO.Out_File, LISTNAME);
                  Ada.Text_IO.Set_Output (The_File);

                  -- Save nodes to listing file for comparison
                  Graph_Package.Dump
                    (The_Unit,
                     Semantic_Dump => True,
                     Full_Dump     => True,
                     XML_Format    => False);

                  Ada.Text_IO.Set_Output (Ada.Text_IO.Standard_Output);
                  Ada.Text_IO.Close(The_File);

                  Result := Test_Package.Compare_File_To_Strings_Vector
                    (LISTNAME, The_Test.The_Listing);

                  -- Delete listing file
                  Ada.Text_IO.Open(The_File, Ada.Text_IO.Out_File, LISTNAME);
                  Ada.Text_IO.Delete (The_File);

                  AUnit.Assertions.Assert
                    (Count (The_Unit) =
                         Pool_Package.Unmarked_Allocations (Graph_Package.The_Pool),
                     "Test " & The_Message & " node count.");

                  -- Compare listing file to test listing
                  AUnit.Assertions.Assert
                    (Result, "Test " & The_Message & " listing.");
               end if;
            end if;

         else -- semantic error found
            if Dump then
               List_Package.List (FILENAME, XML_Format => False);

               -- Delete code file
               Ada.Text_IO.Open (The_File, Ada.Text_IO.Out_File, FILENAME);
               Ada.Text_IO.Delete (The_File);
               AUnit.Assertions.Assert
                 (The_Number_Of_Errors = 0,
                  "Test " & The_Message & " semantic errors.");
               AUnit.Assertions.Assert
                 (The_Number_Of_Warnings = 0,
                  "Test " & The_Message & " semantic warnngs.");

            else
               -- Delete code file
               Ada.Text_IO.Open (The_File, Ada.Text_IO.Out_File, FILENAME);
               Ada.Text_IO.Delete (The_File);
               AUnit.Assertions.Assert
                 (The_Number_Of_Errors = 0,
                  "Test " & The_Message & " semantic errors.");
               AUnit.Assertions.Assert
                 (The_Number_Of_Warnings = 0,
                  "Test " & The_Message & " semantic warnngs.");
            end if;
         end if;

      else -- syntax error found
         if Dump then
            List_Package.List (FILENAME, XML_Format => False);

            -- Delete code file
            Ada.Text_IO.Open (The_File, Ada.Text_IO.Out_File, FILENAME);
            Ada.Text_IO.Delete (The_File);
            AUnit.Assertions.Assert
              (The_Number_Of_Errors = 0,
               "Test " & The_Message & " syntax errors.");
            AUnit.Assertions.Assert
              (The_Number_Of_Warnings = 0,
               "Test " & The_Message & " syntax warnngs.");

         else
            -- Delete code file
            Ada.Text_IO.Open (The_File, Ada.Text_IO.Out_File, FILENAME);
            Ada.Text_IO.Delete (The_File);
            AUnit.Assertions.Assert
              (The_Number_Of_Errors = 0,
               "Test " & The_Message & " syntax errors.");
            AUnit.Assertions.Assert
              (The_Number_Of_Warnings = 0,
               "Test " & The_Message & " syntax warnngs.");
         end if;
      end if;

      --        Ada.Text_IO.Put_Line
      --          (SYSNatural'Image(The_Unmarked_Operand_Allocations) & " - " &
      --             SYSNatural'Image(Pool_Package.Unmarked_Allocations (Operand_Package.The_Pool)));

      Dispose (The_Unit);

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Graph_Package.The_Pool) =
             The_Unmarked_Graph_Allocations,
         "Incorrect graph allocations.");
   end Run_Test;

   --------------------------
   -- Create_Generate_File --
   --------------------------

   procedure Create_Generate_File
     (The_File     : out Ada.Text_IO.File_Type;
      The_Filename : String) is
   begin
      Ada.Text_IO.Create (The_File, Ada.Text_IO.Out_File,The_Filename);
      Ada.Text_IO.Set_Output (The_File);

      Ada.Text_IO.Put_Line ("<?xml version=""1.0""?>");
      Ada.Text_IO.Put_Line ("<body>");
   end Create_Generate_File;

   -------------------------
   -- Close_Generate_File --
   -------------------------

   procedure Close_Generate_File
     (The_File     : out Ada.Text_IO.File_Type) is
   begin
      Ada.Text_IO.Put_Line ("</body>");
      Ada.Text_IO.Set_Output (Ada.Text_IO.Standard_Output);
      Ada.Text_IO.Close (The_File);
   end Close_Generate_File;

   ---------------------
   -- Replace_XMLFile --
   ---------------------

   procedure Replace_XMLFile
     (The_XMLName  : String;
      The_Listname : String) is
      The_File : Ada.Text_IO.File_Type;
   begin
      Ada.Directories.Copy_File (The_XMLName, The_XMLName & ".nxml");
      Ada.Directories.Copy_File (The_Listname, The_XMLName);
      Ada.Text_IO.Open(The_File, Ada.Text_IO.Out_File, The_Listname);
      Ada.Text_IO.Delete(The_File);
   end Replace_XMLFile;


   --------------------
   -- Test_Procedure --
   --------------------

   procedure Test_Procedure (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "optimize/procedure.xml";

      The_Tests : XML_Package.Tests_Map.Map;
      The_File  : Ada.Text_IO.File_Type;

   begin
      XML_Package.Load (XMLNAME, The_Tests);

      if Test_Package.Generate_Flag then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Procedure.")),
         "procedure",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      if Test_Package.Generate_Flag then
         Close_Generate_File (The_File);

         if Test_Package.Replace_Flag then
            Replace_XMLfile (XMLNAME, LISTNAME);
         end if;
      end if;
   end Test_Procedure;

   -----------------
   -- Test_Scalar --
   -----------------

   procedure Test_Scalar (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "optimize/scalar.xml";

      The_Tests : XML_Package.Tests_Map.Map;
      The_File  : Ada.Text_IO.File_Type;

   begin
      XML_Package.Load (XMLNAME, The_Tests);

      if Test_Package.Generate_Flag then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Scalar.")),
         "scalar type",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      if Test_Package.Generate_Flag then
         Close_Generate_File (The_File);

         if Test_Package.Replace_Flag then
            Replace_XMLfile (XMLNAME, LISTNAME);
         end if;
      end if;
   end Test_Scalar;

   --------------
   -- Test_Mod --
   --------------

   procedure Test_Mod (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "optimize/mod.xml";

      The_Tests : XML_Package.Tests_Map.Map;
      The_File  : Ada.Text_IO.File_Type;

   begin
      XML_Package.Load (XMLNAME, The_Tests);

      if Test_Package.Generate_Flag then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Mod.")),
         "mod type",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Mod_Without_Range.")),
         "mod type without range",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      if Test_Package.Generate_Flag then
         Close_Generate_File (The_File);

         if Test_Package.Replace_Flag then
            Replace_XMLfile (XMLNAME, LISTNAME);
         end if;
      end if;
   end Test_Mod;

   -----------------
   -- Test_Range --
   -----------------

   procedure Test_Range (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "optimize/range.xml";

      The_Tests : XML_Package.Tests_Map.Map;
      The_File  : Ada.Text_IO.File_Type;

   begin
      XML_Package.Load (XMLNAME, The_Tests);

      if Test_Package.Generate_Flag then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Scalar.")),
         "scalar range type",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Mod.")),
         "mod range type",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      if Test_Package.Generate_Flag then
         Close_Generate_File (The_File);

         if Test_Package.Replace_Flag then
            Replace_XMLfile (XMLNAME, LISTNAME);
         end if;
      end if;
   end Test_Range;

   ----------------
   -- Test_Array --
   ----------------

   procedure Test_Array (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "optimize/array.xml";

      The_Tests : XML_Package.Tests_Map.Map;
      The_File  : Ada.Text_IO.File_Type;

   begin
      XML_Package.Load (XMLNAME, The_Tests);

      if Test_Package.Generate_Flag then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Array.")),
         "array type",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      if Test_Package.Generate_Flag then
         Close_Generate_File (The_File);

         if Test_Package.Replace_Flag then
            Replace_XMLfile (XMLNAME, LISTNAME);
         end if;
      end if;
   end Test_Array;

   ----------------------
   -- Test_Identifiers --
   ----------------------

   procedure Test_Identifiers (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "optimize/identifiers.xml";

      The_Tests : XML_Package.Tests_Map.Map;
      The_File  : Ada.Text_IO.File_Type;

   begin
      XML_Package.Load (XMLNAME, The_Tests);

      if Test_Package.Generate_Flag then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Identifier.")),
         "identifier declaration",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Identifier_Expression.")),
         "identifier expression declaration",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Identifier_Constant.")),
         "identifier constant declaration",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      if Test_Package.Generate_Flag then
         Close_Generate_File (The_File);

         if Test_Package.Replace_Flag then
            Replace_XMLfile (XMLNAME, LISTNAME);
         end if;
      end if;
   end Test_Identifiers;

   ---------------------
   -- Test_Assignment --
   ---------------------

   procedure Test_Assignment (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "optimize/assignment.xml";

      The_Tests : XML_Package.Tests_Map.Map;
      The_File  : Ada.Text_IO.File_Type;

   begin
      --        Operand_Package.The_Pool.Allocate_Debug := True;
      --        Semantics_Package.Semenatics_Debug := True;
      --        Optimize_Package.Optimize_Debug := True;

      XML_Package.Load (XMLNAME, The_Tests);

      if Test_Package.Generate_Flag then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Assignment.")),
         "scalar assignment",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      --        Run_Test
      --          (XML_Package.Tests_Map.Element
      --             (The_Tests,
      --              To_Unbounded_String ("Constant_Index.")),
      --           "array constant index assignment",
      --           Error_Test => False,
      --           Dump     => Test_Package.Dump_Flag,
      --           Generate => Test_Package.Generate_Flag);
      --
      --        Run_Test
      --          (XML_Package.Tests_Map.Element
      --             (The_Tests,
      --              To_Unbounded_String ("Variable_Index.")),
      --           "array variable index assignment",
      --           Error_Test => False,
      --           Dump     => Test_Package.Dump_Flag,
      --           Generate => Test_Package.Generate_Flag);

      if Test_Package.Generate_Flag then
         Close_Generate_File (The_File);

         if Test_Package.Replace_Flag then
            Replace_XMLfile (XMLNAME, LISTNAME);
         end if;
      end if;
   end Test_Assignment;

   -------------
   -- Test_If --
   -------------

   procedure Test_If (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "optimize/if.xml";

      The_Tests : XML_Package.Tests_Map.Map;
      The_File  : Ada.Text_IO.File_Type;

   begin
      XML_Package.Load (XMLNAME, The_Tests);

      if Test_Package.Generate_Flag then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("If_Then.")),
         "if then",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("If_Then_Else.")),
         "if then else",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      if Test_Package.Generate_Flag then
         Close_Generate_File (The_File);

         if Test_Package.Replace_Flag then
            Replace_XMLfile (XMLNAME, LISTNAME);
         end if;
      end if;
   end Test_If;

   --------------
   -- Test_For --
   --------------

   procedure Test_For (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "optimize/for.xml";

      The_Tests : XML_Package.Tests_Map.Map;
      The_File  : Ada.Text_IO.File_Type;

   begin
      XML_Package.Load (XMLNAME, The_Tests);

      if Test_Package.Generate_Flag then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("For.")),
         "for statement",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      if Test_Package.Generate_Flag then
         Close_Generate_File (The_File);

         if Test_Package.Replace_Flag then
            Replace_XMLfile (XMLNAME, LISTNAME);
         end if;
      end if;
   end Test_For;

   ----------------
   -- Test_Unary --
   ----------------

   procedure Test_Unary (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "optimize/unary.xml";

      The_Tests : XML_Package.Tests_Map.Map;
      The_File  : Ada.Text_IO.File_Type;

   begin
      XML_Package.Load (XMLNAME, The_Tests);

      if Test_Package.Generate_Flag then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Boolean_Constant.")),
         "unary boolean constant expression",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Boolean_Variable.")),
         "unary boolean variable expression",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Integer_Constant.")),
         "unary integer constant expression",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Integer_Variable.")),
         "unary integer variable expression",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      if Test_Package.Generate_Flag then
         Close_Generate_File (The_File);

         if Test_Package.Replace_Flag then
            Replace_XMLfile (XMLNAME, LISTNAME);
         end if;
      end if;
   end Test_Unary;

   -------------------
   -- Test_Relation --
   -------------------

   procedure Test_Relation (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "optimize/relation.xml";

      The_Tests : XML_Package.Tests_Map.Map;
      The_File  : Ada.Text_IO.File_Type;

   begin
      XML_Package.Load (XMLNAME, The_Tests);

      if Test_Package.Generate_Flag then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Constant.")),
         "binary relation constant expression",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable.")),
         "binary relation variable expression",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      if Test_Package.Generate_Flag then
         Close_Generate_File (The_File);

         if Test_Package.Replace_Flag then
            Replace_XMLfile (XMLNAME, LISTNAME);
         end if;
      end if;
   end Test_Relation;

   --------------
   -- Test_And --
   --------------

   procedure Test_And (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "optimize/and.xml";

      The_Tests : XML_Package.Tests_Map.Map;
      The_File  : Ada.Text_IO.File_Type;

   begin
      XML_Package.Load (XMLNAME, The_Tests);

      if Test_Package.Generate_Flag then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Constant.")),
         "binary and constant expression",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("False_And_Variable.")),
         "binary and variable expression",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_And_False.")),
         "binary and variable expression",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("True_And_Variable.")),
         "binary and variable expression",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_And_True.")),
         "binary and variable expression",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable.")),
         "binary and variable expression",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      if Test_Package.Generate_Flag then
         Close_Generate_File (The_File);

         if Test_Package.Replace_Flag then
            Replace_XMLfile (XMLNAME, LISTNAME);
         end if;
      end if;
   end Test_And;

   -------------
   -- Test_Or --
   -------------

   procedure Test_Or (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "optimize/or.xml";

      The_Tests : XML_Package.Tests_Map.Map;
      The_File  : Ada.Text_IO.File_Type;

   begin
      XML_Package.Load (XMLNAME, The_Tests);

      if Test_Package.Generate_Flag then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Constant.")),
         "binary or constant expression",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("False_Or_Variable.")),
         "binary or variable expression",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_Or_False.")),
         "binary or variable expression",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("True_Or_Variable.")),
         "binary or variable expression",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_Or_True.")),
         "binary or variable expression",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable.")),
         "binary or variable expression",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      if Test_Package.Generate_Flag then
         Close_Generate_File (The_File);

         if Test_Package.Replace_Flag then
            Replace_XMLfile (XMLNAME, LISTNAME);
         end if;
      end if;
   end Test_Or;

   --------------
   -- Test_Xor --
   --------------

   procedure Test_Xor (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "optimize/xor.xml";

      The_Tests : XML_Package.Tests_Map.Map;
      The_File  : Ada.Text_IO.File_Type;

   begin
      XML_Package.Load (XMLNAME, The_Tests);

      if Test_Package.Generate_Flag then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Constant.")),
         "binary xor constant expression",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("False_Xor_Variable.")),
         "binary xor variable expression",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_Xor_False.")),
         "binary xor variable expression",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("True_Xor_Variable.")),
         "binary xor variable expression",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_Xor_True.")),
         "binary xor variable expression",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable.")),
         "binary xor variable expression",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      if Test_Package.Generate_Flag then
         Close_Generate_File (The_File);

         if Test_Package.Replace_Flag then
            Replace_XMLfile (XMLNAME, LISTNAME);
         end if;
      end if;
   end Test_Xor;

   -------------------
   -- Test_Addition --
   -------------------

   procedure Test_Addition (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "optimize/addition.xml";

      The_Tests : XML_Package.Tests_Map.Map;
      The_File  : Ada.Text_IO.File_Type;

   begin
      XML_Package.Load (XMLNAME, The_Tests);

      if Test_Package.Generate_Flag then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Constant.")),
         "binary addition constant expression",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Zero_Plus_Variable.")),
         "binary addition variable expression",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_Plus_Zero.")),
         "binary addition variable expression",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("One_Plus_Variable.")),
         "binary addition variable expression",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_Plus_One.")),
         "binary addition variable expression",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable.")),
         "binary addition variable expression",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      if Test_Package.Generate_Flag then
         Close_Generate_File (The_File);

         if Test_Package.Replace_Flag then
            Replace_XMLfile (XMLNAME, LISTNAME);
         end if;
      end if;
   end Test_Addition;

   ----------------------
   -- Test_Subtraction --
   ----------------------

   procedure Test_Subtraction (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "optimize/subtraction.xml";

      The_Tests : XML_Package.Tests_Map.Map;
      The_File  : Ada.Text_IO.File_Type;

   begin
      XML_Package.Load (XMLNAME, The_Tests);

      if Test_Package.Generate_Flag then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Constant.")),
         "binary subtraction constant expression",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Zero_Minus_Variable.")),
         "binary subtraction variable expression",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_Minus_Zero.")),
         "binary subtraction variable expression",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("One_Minus_Variable.")),
         "binary subtraction variable expression",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_Minus_One.")),
         "binary subtraction variable expression",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable.")),
         "binary subtraction variable expression",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      if Test_Package.Generate_Flag then
         Close_Generate_File (The_File);

         if Test_Package.Replace_Flag then
            Replace_XMLfile (XMLNAME, LISTNAME);
         end if;
      end if;
   end Test_Subtraction;

   -------------------------
   -- Test_Multiplication --
   -------------------------

   procedure Test_Multiplication (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "optimize/multiplication.xml";

      The_Tests : XML_Package.Tests_Map.Map;
      The_File  : Ada.Text_IO.File_Type;

   begin
      XML_Package.Load (XMLNAME, The_Tests);

      if Test_Package.Generate_Flag then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Constant.")),
         "binary multiplication constant expression",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Zero_Times_Variable.")),
         "binary multiplication variable expression",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_Times_Zero.")),
         "binary multiplication variable expression",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("One_Times_Variable.")),
         "binary multiplication variable expression",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_Times_One.")),
         "binary multiplication variable expression",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Two_Times_Variable.")),
         "binary multiplication variable expression",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_Times_Two.")),
         "binary multiplication variable expression",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable.")),
         "binary multiplication variable expression",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      if Test_Package.Generate_Flag then
         Close_Generate_File (The_File);

         if Test_Package.Replace_Flag then
            Replace_XMLfile (XMLNAME, LISTNAME);
         end if;
      end if;
   end Test_Multiplication;

   -------------------
   -- Test_Division --
   -------------------

   procedure Test_Division (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "optimize/division.xml";

      The_Tests : XML_Package.Tests_Map.Map;
      The_File  : Ada.Text_IO.File_Type;

   begin
      XML_Package.Load (XMLNAME, The_Tests);

      if Test_Package.Generate_Flag then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Constant.")),
         "binary division constant expression",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Zero_Divide_Variable.")),
         "binary division variable expression",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("One_Divide_Variable.")),
         "binary division variable expression",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_Divide_One.")),
         "binary division variable expression",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Two_Divide_Variable.")),
         "binary division variable expression",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_Divide_Two.")),
         "binary division variable expression",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable.")),
         "binary division variable expression",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      if Test_Package.Generate_Flag then
         Close_Generate_File (The_File);

         if Test_Package.Replace_Flag then
            Replace_XMLfile (XMLNAME, LISTNAME);
         end if;
      end if;
   end Test_Division;

   --------------------
   -- Test_Parameter --
   --------------------

   procedure Test_Parameter (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "optimize/parameter.xml";

      The_Tests : XML_Package.Tests_Map.Map;
      The_File  : Ada.Text_IO.File_Type;

   begin
      XML_Package.Load (XMLNAME, The_Tests);

      if Test_Package.Generate_Flag then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Parameter.")),
         "parameter expression",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      if Test_Package.Generate_Flag then
         Close_Generate_File (The_File);

         if Test_Package.Replace_Flag then
            Replace_XMLfile (XMLNAME, LISTNAME);
         end if;
      end if;
   end Test_Parameter;

   ----------------
   -- Test_Index --
   ----------------

   procedure Test_Index (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "optimize/index.xml";

      The_Tests : XML_Package.Tests_Map.Map;
      The_File  : Ada.Text_IO.File_Type;

   begin
      XML_Package.Load (XMLNAME, The_Tests);

      if Test_Package.Generate_Flag then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Index.")),
         "index expression",
         Error_Test => False,
         Dump       => Test_Package.Dump_Flag,
         Generate   => Test_Package.Generate_Flag);

      if Test_Package.Generate_Flag then
         Close_Generate_File (The_File);

         if Test_Package.Replace_Flag then
            Replace_XMLfile (XMLNAME, LISTNAME);
         end if;
      end if;
   end Test_Index;


   procedure Test_Assign (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);
   begin
      null;
   end Test_Assign;

   procedure Test_Assign_Left (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);
   begin
      null;
   end Test_Assign_Left;

   procedure Test_Assign_Right (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);
   begin
      null;
   end Test_Assign_Right;

   procedure Test_Variable_Errors (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "optimize/errors/variable.xml";

      The_Tests : XML_Package.Tests_Map.Map;
      The_File  : Ada.Text_IO.File_Type;

   begin
      XML_Package.Load (XMLNAME, The_Tests);

      if Test_Package.Generate_Flag then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Index_Range.")),
         "not within array index",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      if Test_Package.Generate_Flag then
         Close_Generate_File (The_File);

         if Test_Package.Replace_Flag then
            Replace_XMLfile (XMLNAME, LISTNAME);
         end if;
      end if;
   end Test_Variable_Errors;

   procedure Test_Unary_Expression_Errors (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "optimize/errors/unary.xml";

      The_Tests : XML_Package.Tests_Map.Map;
      The_File  : Ada.Text_IO.File_Type;

   begin
      XML_Package.Load (XMLNAME, The_Tests);

      if Test_Package.Generate_Flag then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Unary_Boolean.")),
         "not within boolean type",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      if Test_Package.Generate_Flag then
         Close_Generate_File (The_File);

         if Test_Package.Replace_Flag then
            Replace_XMLfile (XMLNAME, LISTNAME);
         end if;
      end if;
   end Test_Unary_Expression_Errors;

   procedure Test_Binary_Expression_Errors (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "optimize/errors/binary.xml";

      The_Tests : XML_Package.Tests_Map.Map;
      The_File  : Ada.Text_IO.File_Type;

   begin
      XML_Package.Load (XMLNAME, The_Tests);

      if Test_Package.Generate_Flag then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Binary_Constant_Boolean.")),
         "not within boolean type",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Binary_And_Right_Boolean.")),
         "not within boolean type",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Binary_Or_Left_Boolean.")),
         "not within boolean type",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Binary_And_Left_Boolean.")),
         "not within boolean type",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Binary_Or_Right_Boolean.")),
         "not within boolean type",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Binary_Times_Right_Integer.")),
         "not within integer type",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Binary_Divide_By_Zero.")),
         "divide by zero",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Binary_Times_Left_Integer.")),
         "not within integer type",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Binary_Divide_Right_Integer.")),
         "not within integer type",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      if Test_Package.Generate_Flag then
         Close_Generate_File (The_File);

         if Test_Package.Replace_Flag then
            Replace_XMLfile (XMLNAME, LISTNAME);
         end if;
      end if;
   end Test_Binary_Expression_Errors;

   procedure Test_Optimize (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "optimize/optimize.xml";

      The_Tests : XML_Package.Tests_Map.Map;
      The_File  : Ada.Text_IO.File_Type;

   begin
      XML_Package.Load (XMLNAME, The_Tests);

      if Test_Package.Generate_Flag then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Optimize.")),
         "optimize",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      if Test_Package.Generate_Flag then
         Close_Generate_File (The_File);

         if Test_Package.Replace_Flag then
            Replace_XMLfile (XMLNAME, LISTNAME);
         end if;
      end if;
   end Test_Optimize;

end Optimize_Package.Optimize_Test;
