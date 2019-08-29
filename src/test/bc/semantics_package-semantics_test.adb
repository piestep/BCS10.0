-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with AUnit.Assertions;
--
with Ada.Containers.Vectors;
with Ada.Text_IO;
with Ada.Directories;
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
with Scope_Package;      use Scope_Package;
--
with Scope_Package.Dump;
with Operand_Package;

package body Semantics_Package.Semantics_Test is

   FILENAME : constant String := Test_Package.FILES & "/" & "semantics.bc";
   LISTNAME : constant String := Test_Package.FILES & "/" & "semantics.tmp";

   The_Unmarked_Graph_Allocations   : SYSNatural;

   ----------
   -- Name --
   ----------

   function Name (The_Test : in Test) return AUnit.Test_String is
   begin
      return AUnit.Format ("semantics_package.semantics_test!");
   end Name;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (The_Test : in out Test) is
      use AUnit.Test_Cases.Registration;

   begin
      -- Repeat for each test routine:
      Register_Routine(The_Test, Test_Procedure'Access, "test_procedure!");
      Register_Routine(The_Test, Test_Discreate_Declaration'Access, "test_discreate_declaration!");
      Register_Routine(The_Test, Test_Scalar_Declaration'Access, "test_scalar_declaration!");
      Register_Routine(The_Test, Test_Mod_Declaration'Access, "test_mod_declaration!");
      Register_Routine(The_Test, Test_Range_Declarations'Access, "test_range_declarations!");
      Register_Routine(The_Test, Test_Array_Declaration'Access, "test_array_declaration!");
      Register_Routine(The_Test, Test_Identifier_Declaration'Access, "test_identifier_declaration!");
      Register_Routine(The_Test, Test_Parameters'Access, "test_parameters!");
      Register_Routine(The_Test, Test_Assignments'Access, "test_assignments!");
      Register_Routine(The_Test, Test_Ifs'Access, "test_ifs!");
      Register_Routine(The_Test, Test_Fors'Access, "test_fors!");
      Register_Routine(The_Test, Test_Operators'Access, "test_operators!");
      Register_Routine(The_Test, Test_Precedence'Access, "test_precedence!");
      Register_Routine(The_Test, Test_Expressions'Access, "test_expressions!");
      Register_Routine(The_Test, Test_Attributes'Access, "test_attributes!");
      Register_Routine(The_Test, Test_Package_Errors'Access, "test_package_errors!");
      Register_Routine(The_Test, Test_Procedure_Errors'Access, "test_procedure_errors!");
      Register_Routine(The_Test, Test_Parameter_Errors'Access, "test_parameter_errors!");
      Register_Routine(The_Test, Test_Type_Errors'Access, "test_type_errors!");
      Register_Routine(The_Test, Test_Identifier_Errors'Access, "test_identifier_errors!");
      Register_Routine(The_Test, Test_Range_Errors'Access, "test_range_errors!");
      Register_Routine(The_Test, Test_Mod_Errors'Access, "test_mod_errors!");
      Register_Routine(The_Test, Test_Array_Errors'Access, "test_array_errors!");
      Register_Routine(The_Test, Test_Assignment_Errors'Access, "test_assignment_errors!");
      Register_Routine(The_Test, Test_Variable_Errors'Access, "test_variable_errors!");
      Register_Routine(The_Test, Test_If_Errors'Access, "test_if_errors!");
      Register_Routine(The_Test, Test_For_Errors'Access, "test_for_errors!");
      Register_Routine(The_Test, Test_Unary_Errors'Access, "test_unary_errors!");
      Register_Routine(The_Test, Test_Binary_Errors'Access, "test_binary_errors!");
      Register_Routine(The_Test, Test_Attribute_Errors'Access, "test_attribute_errors!");
   end Register_Tests;

   -----------------
   -- Set_Up_Case --
   -----------------

   overriding procedure Set_Up_Case (The_Test : in out Test) is
      pragma Unreferenced (The_Test);

   begin
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
      --              pragma Unreferenced (The_Test);

   begin
      Scope_Package.Close;
      Type_Package.Clear;
      Identifier_Package.Clear;

      Ada.Text_IO.Put_Line("Semantics_Package.Semantics_Test " & Name(The_Test).all);
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
      Error_Package.Clear;
      Test_Package.Save (FILENAME, The_Test.The_Code);

      Source_Package.Open (FILENAME);

      Scanner_Package.Next_Symbol;
      Syntax_Package.Parse (The_Unit);

      Source_Package.Close;

      if The_Number_Of_Errors = 0 and The_Number_Of_Warnings = 0 then
         Semantics_Package.Parse (The_Unit);

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
      Ada.Text_IO.Create (The_File, Ada.Text_IO.Out_File, The_Filename);
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
        Test_Package.FILES & "/" & "semantics/test_procedure.xml";

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

   --------------------------------
   -- Test_Discreate_Declaration --
   --------------------------------

   procedure Test_Discreate_Declaration (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "semantics/test_discreate.xml";

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
            To_Unbounded_String ("Discreate.")),
         "discreate declaration",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      if Test_Package.Generate_Flag then
         Close_Generate_File (The_File);

         if Test_Package.Replace_Flag then
            Replace_XMLfile (XMLNAME, LISTNAME);
         end if;
      end if;
   end Test_Discreate_Declaration;

   -----------------------------
   -- Test_Scalar_Declaration --
   -----------------------------

   procedure Test_Scalar_Declaration (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "semantics/test_scalar.xml";

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
         "scalar declaration",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      if Test_Package.Generate_Flag then
         Close_Generate_File (The_File);

         if Test_Package.Replace_Flag then
            Replace_XMLfile (XMLNAME, LISTNAME);
         end if;
      end if;
   end Test_Scalar_Declaration;

   --------------------------
   -- Test_Mod_Declaration --
   --------------------------

   procedure Test_Mod_Declaration (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "semantics/test_mod.xml";

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
         "mod declaration",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Mod_Without_Range.")),
         "mod without range declaration",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      if Test_Package.Generate_Flag then
         Close_Generate_File (The_File);

         if Test_Package.Replace_Flag then
            Replace_XMLfile (XMLNAME, LISTNAME);
         end if;
      end if;
   end Test_Mod_Declaration;

   -----------------------------
   -- Test_Range_Declarations --
   -----------------------------

   procedure Test_Range_Declarations (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "semantics/test_range.xml";

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
            To_Unbounded_String ("Discreate.")),
         "discreate range declaration",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Scalar.")),
         "scalar range declaration",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Mod.")),
         "mod range declaration",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      if Test_Package.Generate_Flag then
         Close_Generate_File (The_File);

         if Test_Package.Replace_Flag then
            Replace_XMLfile (XMLNAME, LISTNAME);
         end if;
      end if;
   end Test_Range_Declarations;

   ----------------------------
   -- Test_Array_Declaration --
   ----------------------------

   procedure Test_Array_Declaration (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "semantics/test_array.xml";

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
            To_Unbounded_String ("Array_Declaration.")),
         "array declaration",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      if Test_Package.Generate_Flag then
         Close_Generate_File (The_File);

         if Test_Package.Replace_Flag then
            Replace_XMLfile (XMLNAME, LISTNAME);
         end if;
      end if;
   end Test_Array_Declaration;

   ---------------------------------
   -- Test_Identifier_Declaration --
   ---------------------------------

   procedure Test_Identifier_Declaration (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "semantics/test_identifiers.xml";

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
            To_Unbounded_String ("Identifier_Declaration.")),
         "identifier declaration",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Identifier_Expression_Declaration.")),
         "identifier expression declaration",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Identifier_Constant_Declaration.")),
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
   end Test_Identifier_Declaration;

   ---------------------
   -- Test_Parameters --
   ---------------------

   procedure Test_Parameters (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "semantics/test_parameters.xml";

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
         "parameter",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("In_Parameter.")),
         "in parameter",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Out_Parameter.")),
         "out parameter",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("In_Out_Parameter.")),
         "in out parameter",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      if Test_Package.Generate_Flag then
         Close_Generate_File (The_File);

         if Test_Package.Replace_Flag then
            Replace_XMLfile (XMLNAME, LISTNAME);
         end if;
      end if;
   end Test_Parameters;

   ----------------------
   -- Test_Assignments --
   ----------------------

   procedure Test_Assignments (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "semantics/test_assignments.xml";

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
            To_Unbounded_String ("Assignment.")),
         "assignment",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Parameter.")),
         "parameter assignment",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Array.")),
         "array assignment",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Array_Variable_Index.")),
         "array assignment with variable index",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      if Test_Package.Generate_Flag then
         Close_Generate_File (The_File);

         if Test_Package.Replace_Flag then
            Replace_XMLfile (XMLNAME, LISTNAME);
         end if;
      end if;
   end Test_Assignments;

   --------------
   -- Test_Ifs --
   --------------

   procedure Test_Ifs (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "semantics/test_ifs.xml";

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
   end Test_Ifs;

   ---------------
   -- Test_Fors --
   ---------------

   procedure Test_Fors (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "semantics/test_fors.xml";

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
         "for",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("For_Reverse.")),
         "for reverse",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      if Test_Package.Generate_Flag then
         Close_Generate_File (The_File);

         if Test_Package.Replace_Flag then
            Replace_XMLfile (XMLNAME, LISTNAME);
         end if;
      end if;
   end Test_Fors;

   --------------------
   -- Test_Operators --
   --------------------

   procedure Test_Operators (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "semantics/test_operators.xml";

      The_Tests : XML_Package.Tests_Map.Map;
      The_File  : Ada.Text_IO.File_Type;
   begin
      XML_Package.Load (XMLNAME, The_Tests);

      if Test_Package.Generate_Flag then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      -- Boolean operators

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Not.")),
         "not",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("And.")),
         "and",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element (The_Tests, To_Unbounded_String ("Or.")),
         "or",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Xor.")),
         "xor",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      -- Relation operators

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Equals.")),
         "equals",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Not_Equals.")),
         "not equals",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Less_Than.")),
         "less than",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Greater_Than.")),
         "greater than",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Less_Than_Equals.")),
         "less than equals",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Greater_Than_Equals.")),
         "greater than equals",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      -- Arithmetic Operators

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Plus.")),
         "plus",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Minus.")),
         "minus",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Addition.")),
         "addition",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Subtraction.")),
         "subtraction",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Times.")),
         "times",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Divide.")),
         "divide",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Remainder.")),
         "remainder",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Modulas.")),
         "modulas",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      if Test_Package.Generate_Flag then
         Close_Generate_File (The_File);

         if Test_Package.Replace_Flag then
            Replace_XMLfile (XMLNAME, LISTNAME);
         end if;
      end if;
   end Test_Operators;

   ---------------------
   -- Test_Precedence --
   ---------------------

   procedure Test_Precedence (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "semantics/test_precedence.xml";

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
            To_Unbounded_String ("And_Not.")),
         "and not precedence",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Not_And.")),
         "not and precedence",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Xor.")),
         "xor precedence",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Or.")),
         "or precedence",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Minus.")),
         "minus precedence",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Plus.")),
         "plus precedence",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Paren.")),
         "paren precedence",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Times.")),
         "times precedence",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Divide.")),
         "divide precedence",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Rem.")),
         "rem precedence",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Mod.")),
         "mod precedence",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      if Test_Package.Generate_Flag then
         Close_Generate_File (The_File);

         if Test_Package.Replace_Flag then
            Replace_XMLfile (XMLNAME, LISTNAME);
         end if;
      end if;
   end Test_Precedence;

   ----------------------
   -- Test_Expressions --
   ----------------------

   procedure Test_Expressions (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "semantics/test_expressions.xml";

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
            To_Unbounded_String ("Unary-Boolean.")),
         "unary boolean",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Unary-Integer.")),
         "unary integer",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Unary-Mod.")),
         "unary mod",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Binary-Relation.")),
         "binary boolean",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Binary-Boolean.")),
         "binary boolean",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Binary-Integer.")),
         "binary integer",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Binary-Mod.")),
         "binary mod",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Parameter.")),
         "parameter expression",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Index.")),
         "index expression",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      if Test_Package.Generate_Flag then
         Close_Generate_File (The_File);

         if Test_Package.Replace_Flag then
            Replace_XMLfile (XMLNAME, LISTNAME);
         end if;
      end if;
   end Test_Expressions;

   ---------------------
   -- Test_Attributes --
   ---------------------

   procedure Test_Attributes (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "semantics/test_attributes.xml";

      The_Tests : XML_Package.Tests_Map.Map;
      The_File  : Ada.Text_IO.File_Type;
   begin
      XML_Package.Load (XMLNAME, The_Tests);

      if Test_Package.Generate_Flag then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      -- Type array attributes

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Type_Array_Length.")),
         "type array length attribute",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Type_Array_First.")),
         "type array first attribute",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Type_Array_Last.")),
         "type array last attribute",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Type_Array_Size.")),
         "type array size attribute",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      -- Type scalar attributes

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Type_Scalar_First.")),
         "type Scalar first attribute",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Type_Scalar_Last.")),
         "type Scalar last attribute",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Type_Scalar_Size.")),
         "type Scalar size attribute",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      -- Variable array attributes

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_Array_Length.")),
         "variable array length attribute",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_Array_First.")),
         "variable array first attribute",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_Array_Last.")),
         "variable array last attribute",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_Array_Size.")),
         "variable array size attribute",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      -- Variable scalar attributes

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_Scalar_First.")),
         "variable Scalar first attribute",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_Scalar_Last.")),
         "variable Scalar last attribute",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_Scalar_Size.")),
         "variable Scalar size attribute",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      -- Parameter array attributes

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Parameter_Array_Length.")),
         "parameter array length attribute",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Parameter_Array_First.")),
         "parameter array first attribute",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Parameter_Array_Last.")),
         "parameter array last attribute",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Parameter_Array_Size.")),
         "parameter array size attribute",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      -- Parameter scalar attributes

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Parameter_Scalar_First.")),
         "parameter Scalar first attribute",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Parameter_Scalar_Last.")),
         "parameter Scalar last attribute",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Parameter_Scalar_Size.")),
         "parameter Scalar size attribute",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      -- Index attributes

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Index_First.")),
         "index first attribute",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Index_Last.")),
         "index last attribute",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Index_Size.")),
         "index size attribute",
         Error_Test => False,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      if Test_Package.Generate_Flag then
         Close_Generate_File (The_File);

         if Test_Package.Replace_Flag then
            Replace_XMLfile (XMLNAME, LISTNAME);
         end if;
      end if;
   end Test_Attributes;

   --------------------------
   -- Semantic Error Tests --
   --------------------------

   -------------------------
   -- Test_Package_Errors --
   -------------------------

   procedure Test_Package_Errors (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "semantics/errors/test_package.xml";

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
            To_Unbounded_String ("Package_Identifier.")),
         "package identifier",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Package_End.")),
         "package end identifier",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      if Test_Package.Generate_Flag then
         Close_Generate_File (The_File);

         if Test_Package.Replace_Flag then
            Replace_XMLfile (XMLNAME, LISTNAME);
         end if;
      end if;
   end Test_Package_Errors;

   ---------------------------
   -- Test_Procedure_Errors --
   ---------------------------

   procedure Test_Procedure_Errors (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "semantics/errors/test_procedure.xml";

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
            To_Unbounded_String ("Procedure_Identifier.")),
         "procedure identifier",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Procedure_End.")),
         "procedure end identifier",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      if Test_Package.Generate_Flag then
         Close_Generate_File (The_File);

         if Test_Package.Replace_Flag then
            Replace_XMLfile (XMLNAME, LISTNAME);
         end if;
      end if;
   end Test_Procedure_Errors;

   ---------------------------
   -- Test_Parameter_Errors --
   ---------------------------

   procedure Test_Parameter_Errors (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "semantics/errors/test_parameter.xml";

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
            To_Unbounded_String ("Parameter_Identifier.")),
         "parameter identifier",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Parameter_Excessive.")),
         "parameter excessive",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Parameter_Expected.")),
         "parameter expected",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Parameter_Undefined.")),
         "parameter undefined",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      if Test_Package.Generate_Flag then
         Close_Generate_File (The_File);

         if Test_Package.Replace_Flag then
            Replace_XMLfile (XMLNAME, LISTNAME);
         end if;
      end if;
   end Test_Parameter_Errors;

   ----------------------
   -- Test_Type_Errors --
   ----------------------

   procedure Test_Type_Errors (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "semantics/errors/test_type.xml";

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
            To_Unbounded_String ("Type_Identifier.")),
         "type identifier",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      if Test_Package.Generate_Flag then
         Close_Generate_File (The_File);

         if Test_Package.Replace_Flag then
            Replace_XMLfile (XMLNAME, LISTNAME);
         end if;
      end if;
   end Test_Type_Errors;

   ----------------------------
   -- Test_Identifier_Errors --
   ----------------------------

   procedure Test_Identifier_Errors (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "semantics/errors/test_identifier.xml";

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
         "identifier",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Identifier_Excessive.")),
         "identifier excessive",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Identifier_Expected.")),
         "identifier expected",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Identifier_Undefined.")),
         "identifier undefined",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Identifier_Not_Compatiable_Type.")),
         "identifier not compatiable type",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Identifier_Not_Compatiable.")),
         "identifier not compatiable",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Identifier_Constant.")),
         "identifier constant",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Identifier_Array.")),
         "identifier array",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Identifier_Constant_Value.")),
         "identifier constant value",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      if Test_Package.Generate_Flag then
         Close_Generate_File (The_File);

         if Test_Package.Replace_Flag then
            Replace_XMLfile (XMLNAME, LISTNAME);
         end if;
      end if;
   end Test_Identifier_Errors;

   -----------------------
   -- Test_Range_Errors --
   -----------------------

   procedure Test_Range_Errors (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "semantics/errors/test_range.xml";

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
            To_Unbounded_String ("Range_Expected.")),
         "range expected",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Range_Undefined.")),
         "range undefined",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Range_Not_Compatiable_Type-First.")),
         "range not compatiable type (first)",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Range_Not_Compatiable-First.")),
         "range not compatiable (first)",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Range_Constant-First.")),
         "range first constant (first)",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Range_Not_Compatiable_Type-Last.")),
         "range last not compatiable type (last)",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Range_Not_Compatiable-Last.")),
         "range not compatiable (last)",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Range_Constant-Last.")),
         "range last constant (last)",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      if Test_Package.Generate_Flag then
         Close_Generate_File (The_File);

         if Test_Package.Replace_Flag then
            Replace_XMLfile (XMLNAME, LISTNAME);
         end if;
      end if;
   end Test_Range_Errors;

   ---------------------
   -- Test_Mod_Errors --
   ---------------------

   procedure Test_Mod_Errors (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "semantics/errors/test_mod.xml";

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
            To_Unbounded_String ("Mod_Not_Compatiable_Type.")),
         "mod not compatiable type",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Mod_Not_Compatiable.")),
         "mod not compatiable",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Mod_Constant.")),
         "mod constant",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      if Test_Package.Generate_Flag then
         Close_Generate_File (The_File);

         if Test_Package.Replace_Flag then
            Replace_XMLfile (XMLNAME, LISTNAME);
         end if;
      end if;
   end Test_Mod_Errors;

   -----------------------
   -- Test_Array_Errors --
   -----------------------

   procedure Test_Array_Errors (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "semantics/errors/test_array.xml";

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
            To_Unbounded_String ("Array_Excessive-Index.")),
         "array excessive index",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Array_Scalar-Index.")),
         "array scalar index",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Array_Expected-Index.")),
         "array expected index",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Array_Undefined-Index.")),
         "array undefined index",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Array_Constraint-First.")),
         "array constraint first",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Array_Not_Compatiable-First.")),
         "array index not compatiable first",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Array_Constant-First.")),
         "array constant first",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Array_Constraint-Last.")),
         "array constraint last",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Array_Not_Compatiable-Last.")),
         "array index not compatiable last",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Array_Constant-Last.")),
         "array constant last",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Array_Excessive-Element.")),
         "array excessive element",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Array_Scalar-Element.")),
         "array scalar element",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Array_Expected-Element.")),
         "array expected element",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Array_Undefined-Element.")),
         "array undefined element",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      if Test_Package.Generate_Flag then
         Close_Generate_File (The_File);

         if Test_Package.Replace_Flag then
            Replace_XMLfile (XMLNAME, LISTNAME);
         end if;
      end if;
   end Test_Array_Errors;

   ----------------------------
   -- Test_Assignment_Errors --
   ----------------------------

   procedure Test_Assignment_Errors (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "semantics/errors/test_assignment.xml";

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
         "assignment to in parameter",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Constant.")),
         "assignment to constant",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Constraint.")),
         "assignment not within constraint",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Not_Compatiable.")),
         "assignment not compatiable with identifier",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      if Test_Package.Generate_Flag then
         Close_Generate_File (The_File);

         if Test_Package.Replace_Flag then
            Replace_XMLfile (XMLNAME, LISTNAME);
         end if;
      end if;
   end Test_Assignment_Errors;

   --------------------------
   -- Test_Variable_Errors --
   --------------------------

   procedure Test_Variable_Errors (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "semantics/errors/test_variable.xml";

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
            To_Unbounded_String ("Variable_Undefined.")),
         "variable undefined",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_Scalar.")),
         "variable scalar",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_Constant.")),
         "variable or constant variable",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_Not_Within.")),
         "variable not within constraint",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_Not_Compatiable.")),
         "variable not compatiable",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_Array.")),
         "array variable or parameter",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_Parameter.")),
         "variable or parameter",
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

   --------------------
   -- Test_If_Errors --
   --------------------

   procedure Test_If_Errors (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "semantics/errors/test_if.xml";

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
            To_Unbounded_String ("Condition_Error.")),
         "boolean condition",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      if Test_Package.Generate_Flag then
         Close_Generate_File (The_File);

         if Test_Package.Replace_Flag then
            Replace_XMLfile (XMLNAME, LISTNAME);
         end if;
      end if;
   end Test_If_Errors;

   ---------------------
   -- Test_For_Errors --
   ---------------------

   procedure Test_For_Errors (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "semantics/errors/test_for.xml";

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
            To_Unbounded_String ("Index_Expected.")),
         "index identifier expected",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Index_Undefined.")),
         "index type undefined",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Index_Constraint-First.")),
         "index not within type constraint (first)",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Index_Not_Compatiable-First.")),
         "index not compatiable with type (first)",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Index_Constraint-Last.")),
         "index not within type constraint (last)",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Index_Not_Compatiable-Last.")),
         "index not compatiable with type (last)",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      if Test_Package.Generate_Flag then
         Close_Generate_File (The_File);

         if Test_Package.Replace_Flag then
            Replace_XMLfile (XMLNAME, LISTNAME);
         end if;
      end if;
   end Test_For_Errors;

   -----------------------
   -- Test_Unary_Errors --
   -----------------------

   procedure Test_Unary_Errors (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "semantics/errors/test_unary.xml";

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
            To_Unbounded_String ("Boolean_Type.")),
         "not within unary operator type",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Boolean_Compatiable.")),
         "not compatiable with unary operator",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Integer_Type.")),
         "not within unary operator type",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Integer_Compatiable.")),
         "not compatiable with unary operator",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      if Test_Package.Generate_Flag then
         Close_Generate_File (The_File);

         if Test_Package.Replace_Flag then
            Replace_XMLfile (XMLNAME, LISTNAME);
         end if;
      end if;
   end Test_Unary_Errors;

   -----------------------
   -- Test_Binary_Errors --
   -----------------------

   procedure Test_Binary_Errors (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "semantics/errors/test_binary.xml";

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
            To_Unbounded_String ("Expressions_Not_Compatiable.")),
         "expressions not compatiable",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Type_Error-Boolean.")),
         "not within binary operator",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Boolean_Error.")),
         "not compatiable with binary operator",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Type_Error-Integer.")),
         "not within binary operator",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Integer_Error.")),
         "not compatiable with binary operator",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Operands_Not_Compatiable.")),
         "operands not compatiable",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      if Test_Package.Generate_Flag then
         Close_Generate_File (The_File);

         if Test_Package.Replace_Flag then
            Replace_XMLfile (XMLNAME, LISTNAME);
         end if;
      end if;
   end Test_Binary_Errors;

   ---------------------------
   -- Test_Attribute_Errors --
   ---------------------------

   procedure Test_Attribute_Errors (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "semantics/errors/test_attribute.xml";

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
            To_Unbounded_String ("Undefined_Identifier.")),
         "undefined identifier",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Length_Requires_Array.")),
         "length requires array",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Test
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Expected_Attribute.")),
         "expected attribute",
         Error_Test => True,
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      if Test_Package.Generate_Flag then
         Close_Generate_File (The_File);

         if Test_Package.Replace_Flag then
            Replace_XMLfile (XMLNAME, LISTNAME);
         end if;
      end if;
   end Test_Attribute_Errors;

end Semantics_Package.Semantics_Test;
