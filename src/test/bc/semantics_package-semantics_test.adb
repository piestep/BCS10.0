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

package body Semantics_Package.Semantics_Test is

   FILENAME : constant String := Test_Package.FILES & "/" & "semantics.bc";
   LISTNAME : constant String := Test_Package.FILES & "/" & "semantics.tmp";

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
      --        Register_Routine (The_Test, Test_Procedure'Access, "Procedure.");
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
   end Register_Tests;

   ------------
   -- Set_Up --
   ------------

   overriding procedure Set_Up (The_Test : in out Test) is
      pragma Unreferenced (The_Test);

   begin
      -- Open initial scope.

      Scope_Package.Open;

      Scope_Package.Enter
        (new Identifier_Package.Type_Identifier'
           (The_String => Scanner_Package.Boolean_String,
            The_Type   => Type_Package.Boolean_Type));

      Scope_Package.Enter
        (new Identifier_Package.Constant_Identifier'
           (The_String => Scanner_Package.False_String,
            The_Type   => Type_Package.Universal_Boolean,
            The_Value  => 0));

      Scope_Package.Enter
        (new Identifier_Package.Constant_Identifier'
           (The_String => Scanner_Package.True_String,
            The_Type   => Type_Package.Universal_Boolean,
            The_Value  => 1));

      Scope_Package.Enter
        (new Identifier_Package.Type_Identifier'
           (The_String => Scanner_Package.Integer_String,
            The_Type   => Type_Package.Integer_Type));
   end Set_Up;

   ---------------
   -- Tear_Down --
   ---------------

   overriding procedure Tear_Down (The_Test : in out Test) is
      pragma Unreferenced (The_Test);

   begin
      Scope_Package.Close;
   end Tear_Down;

   -----------------
   -- Set_Up_Case --
   -----------------

   overriding procedure Set_Up_Case (The_Test : in out Test) is
      pragma Unreferenced (The_Test);

   begin
      null;
   end Set_Up_Case;

   --------------------
   -- Tear_Down_Case --
   --------------------

   overriding procedure Tear_Down_Case (The_Test : in out Test) is
      pragma Unreferenced (The_Test);

   begin
      null;
   end Tear_Down_Case;

   ---------------
   -- Run_Tests --
   ---------------

   procedure Run_Tests
     (The_Test    : XML_Package.XML_Record;
      The_Message : String;
      Dump        : Boolean;
      Generate    : Boolean) with
     Pre => not (Dump and Generate) is
      use XML_Package.Strings_Vector;

      The_File : Ada.Text_IO.File_Type;
      The_Unit : Compilation_Unit_Graph;

   begin
      Error_Package.Clear;
      Test_Package.Save (FILENAME, The_Test.The_Code);

      if Generate then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("<test>");
         Ada.Text_IO.Put_Line
           ("<name>" & To_String (The_Test.The_Name) & "</name>");

         Test_Package.List (The_Test.The_Code, XML_Format => True);
      end if;

      Source_Package.Open (FILENAME);

      Scanner_Package.Next_Symbol;
      Syntax_Package.Parse (The_Unit);

      Source_Package.Close;

      if not (Dump or Generate) then
         AUnit.Assertions.Assert
           (The_Number_Of_Errors = 0,
            "Test " & The_Message & " syntax errors.");
         AUnit.Assertions.Assert
           (The_Number_Of_Warnings = 0,
            "Test " & The_Message & " syntax warnngs.");
      end if;

      Parse (The_Unit);

      if Dump then
         List_Package.List (FILENAME, XML_Format => False);
      end if;

      if not (Dump or Generate) then
         AUnit.Assertions.Assert
           (The_Number_Of_Errors = 0,
            "Test " & The_Message & " semantic errors.");
         AUnit.Assertions.Assert
           (The_Number_Of_Warnings = 0,
            "Test " & The_Message & " semantic warnngs.");
      end if;

      if not Generate then
         Ada.Text_IO.Create (The_File, Ada.Text_IO.Out_File, LISTNAME);
         Ada.Text_IO.Set_Output (The_File);
      end if;

      if The_Number_Of_Errors = 0 and The_Number_Of_Warnings = 0 then

         if Generate then
            Graph_Package.Dump
              (The_Unit,
               Semantic_Dump => True,
               Full_Dump     => True,
               XML_Format    => True);

         else
            Graph_Package.Dump
              (The_Unit,
               Semantic_Dump => True,
               Full_Dump     => True,
               XML_Format    => False);
         end if;
      end if;

      if not Generate then
         Ada.Text_IO.Close (The_File);
         Ada.Text_IO.Set_Output (Ada.Text_IO.Standard_Output);
      end if;

      if not (Dump or Generate) then

         AUnit.Assertions.Assert
           (Count (The_Unit) =
                Pool_Package.Unmarked_Allocations (Graph_Package.The_Pool),
            "Test " & The_Message & " node count.");

         AUnit.Assertions.Assert
           (Test_Package.Compare_File_To_Strings_Vector
              (LISTNAME,
               The_Test.The_Listing),
            "Test " & The_Message & " listing.");
      end if;

      Dispose (The_Unit);

      Ada.Text_IO.Open (The_File, Ada.Text_IO.Out_File, FILENAME);
      Ada.Text_IO.Delete (The_File);

      if Generate then
         Ada.Text_IO.Put_Line ("</test>");
      else
         Ada.Text_IO.Open (The_File, Ada.Text_IO.Out_File, LISTNAME);
         Ada.Text_IO.Delete (The_File);
      end if;
   end Run_Tests;

   ---------------------
   -- Run_Error_Tests --
   ---------------------

   procedure Run_Error_Tests
     (The_Test    : XML_Package.XML_Record;
      The_Message : String;
      Dump        : Boolean;
      Generate    : Boolean) with
     Pre => not (Dump and Generate) is
      use XML_Package.Strings_Vector;

      The_File : Ada.Text_IO.File_Type;
      The_Unit : Compilation_Unit_Graph;
   begin
      Error_Package.Clear;
      Test_Package.Save (FILENAME, The_Test.The_Code);

      if Generate then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("<test>");
         Ada.Text_IO.Put_Line
           ("<name>" & To_String (The_Test.The_Name) & "</name>");

         Test_Package.List (The_Test.The_Code, XML_Format => True);
      end if;

      Source_Package.Open (FILENAME);

      Scanner_Package.Next_Symbol;
      Syntax_Package.Parse (The_Unit);

      Source_Package.Close;

      if not (Dump or Generate) then
         AUnit.Assertions.Assert
           (The_Number_Of_Errors = 0,
            "Test " & The_Message & " syntax errors.");
         AUnit.Assertions.Assert
           (The_Number_Of_Warnings = 0,
            "Test " & The_Message & " syntax warnngs.");
      end if;

      Semantics_Package.Parse (The_Unit);

      if Dump then
         List_Package.List (FILENAME, XML_Format => False);
      end if;

      if not (Dump or Generate) then
         AUnit.Assertions.Assert
           (The_Number_Of_Errors > 0 or The_Number_Of_Warnings > 0,
            "Test " & The_Message & " semantic errors and warnings.");
      end if;

      if Generate then
         Error_Package.List_Messages (XML_Format => True);

      else
         Ada.Text_IO.Create (The_File, Ada.Text_IO.Out_File, LISTNAME);
         Ada.Text_IO.Set_Output (The_File);

         Error_Package.List_Messages (XML_Format => False);

         Ada.Text_IO.Close (The_File);
         Ada.Text_IO.Set_Output (Ada.Text_IO.Standard_Output);
      end if;

      if not (Dump or Generate) then
         AUnit.Assertions.Assert
           (Test_Package.Compare_First_Line_Of_File_To_String
              (LISTNAME,
               To_String (Element (The_Test.The_Listing, 1))),
            "Test " & The_Message & " error.");
      end if;

      Dispose (The_Unit);

      Ada.Text_IO.Open (The_File, Ada.Text_IO.Out_File, FILENAME);
      Ada.Text_IO.Delete (The_File);

      if Generate then
         Ada.Text_IO.Put_Line ("</test>");
      else
         Ada.Text_IO.Open (The_File, Ada.Text_IO.Out_File, LISTNAME);
         Ada.Text_IO.Delete (The_File);
      end if;
   end Run_Error_Tests;

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
   begin
      Ada.Directories.Copy_File (The_XMLName, The_XMLName & ".nxml");
      Ada.Directories.Copy_File (The_Listname, The_XMLName);
   end Replace_XMLFile;


   --------------------
   -- Test_Procedure --
   --------------------

   procedure Test_Procedure (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "semantics/procedure.xml";

      The_Tests : XML_Package.Tests_Map.Map;
      The_File  : Ada.Text_IO.File_Type;

   begin
      XML_Package.Load (XMLNAME, The_Tests);

      if Test_Package.Generate_Flag then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Procedure.")),
         "procedure",
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
        Test_Package.FILES & "/" & "semantics/discreate.xml";

      The_Tests : XML_Package.Tests_Map.Map;
      The_File  : Ada.Text_IO.File_Type;

   begin
      XML_Package.Load (XMLNAME, The_Tests);

      if Test_Package.Generate_Flag then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Discreate.")),
         "discreate declaration",
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
        Test_Package.FILES & "/" & "semantics/scalar.xml";

      The_Tests : XML_Package.Tests_Map.Map;
      The_File  : Ada.Text_IO.File_Type;

   begin
      XML_Package.Load (XMLNAME, The_Tests);

      if Test_Package.Generate_Flag then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Scalar.")),
         "scalar declaration",
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
        Test_Package.FILES & "/" & "semantics/mod.xml";

      The_Tests : XML_Package.Tests_Map.Map;
      The_File  : Ada.Text_IO.File_Type;
   begin
      XML_Package.Load (XMLNAME, The_Tests);

      if Test_Package.Generate_Flag then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Mod.")),
         "mod declaration",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Mod_Without_Range.")),
         "mod without range declaration",
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
        Test_Package.FILES & "/" & "semantics/range.xml";

      The_Tests : XML_Package.Tests_Map.Map;
      The_File  : Ada.Text_IO.File_Type;
   begin
      XML_Package.Load (XMLNAME, The_Tests);

      if Test_Package.Generate_Flag then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Discreate.")),
         "discreate range declaration",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Scalar.")),
         "scalar range declaration",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Mod.")),
         "mod range declaration",
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
        Test_Package.FILES & "/" & "semantics/array.xml";

      The_Tests : XML_Package.Tests_Map.Map;
      The_File  : Ada.Text_IO.File_Type;
   begin
      XML_Package.Load (XMLNAME, The_Tests);

      if Test_Package.Generate_Flag then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Array_Declaration.")),
         "array declaration",
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
        Test_Package.FILES & "/" & "semantics/identifiers.xml";

      The_Tests : XML_Package.Tests_Map.Map;
      The_File  : Ada.Text_IO.File_Type;
   begin
      XML_Package.Load (XMLNAME, The_Tests);

      if Test_Package.Generate_Flag then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Identifier_Declaration.")),
         "identifier declaration",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Identifier_Expression_Declaration.")),
         "identifier expression declaration",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Identifier_Constant_Declaration.")),
         "identifier constant declaration",
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
        Test_Package.FILES & "/" & "semantics/parameters.xml";

      The_Tests : XML_Package.Tests_Map.Map;
      The_File  : Ada.Text_IO.File_Type;
   begin
      XML_Package.Load (XMLNAME, The_Tests);

      if Test_Package.Generate_Flag then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Parameter.")),
         "parameter",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("In_Parameter.")),
         "in parameter",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Out_Parameter.")),
         "out parameter",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("In_Out_Parameter.")),
         "in out parameter",
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
        Test_Package.FILES & "/" & "semantics/assignments.xml";

      The_Tests : XML_Package.Tests_Map.Map;
      The_File  : Ada.Text_IO.File_Type;
   begin
      XML_Package.Load (XMLNAME, The_Tests);

      if Test_Package.Generate_Flag then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Assignment.")),
         "assignment",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Parameter.")),
         "parameter assignment",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Array.")),
         "array assignment",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Array_Variable_Index.")),
         "array assignment with variable index",
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
        Test_Package.FILES & "/" & "semantics/ifs.xml";

      The_Tests : XML_Package.Tests_Map.Map;
      The_File  : Ada.Text_IO.File_Type;
   begin
      XML_Package.Load (XMLNAME, The_Tests);

      if Test_Package.Generate_Flag then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("If_Then.")),
         "if then",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("If_Then_Else.")),
         "if then else",
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
        Test_Package.FILES & "/" & "semantics/fors.xml";

      The_Tests : XML_Package.Tests_Map.Map;
      The_File  : Ada.Text_IO.File_Type;
   begin
      XML_Package.Load (XMLNAME, The_Tests);

      if Test_Package.Generate_Flag then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("For.")),
         "for",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("For_Reverse.")),
         "for reverse",
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
        Test_Package.FILES & "/" & "semantics/operators.xml";

      The_Tests : XML_Package.Tests_Map.Map;
      The_File  : Ada.Text_IO.File_Type;
   begin
      XML_Package.Load (XMLNAME, The_Tests);

      if Test_Package.Generate_Flag then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      -- Boolean operators

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Not.")),
         "not",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("And.")),
         "and",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element (The_Tests, To_Unbounded_String ("Or.")),
         "or",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Xor.")),
         "xor",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      -- Relation operators

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Equals.")),
         "equals",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Not_Equals.")),
         "not equals",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Less_Than.")),
         "less than",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Greater_Than.")),
         "greater than",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Less_Than_Equals.")),
         "less than equals",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Greater_Than_Equals.")),
         "greater than equals",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      -- Arithmetic Operators

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Plus.")),
         "plus",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Minus.")),
         "minus",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Addition.")),
         "addition",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Subtraction.")),
         "subtraction",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Times.")),
         "times",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Divide.")),
         "divide",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Remainder.")),
         "remainder",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Modulas.")),
         "modulas",
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
        Test_Package.FILES & "/" & "semantics/precedence.xml";

      The_Tests : XML_Package.Tests_Map.Map;
      The_File  : Ada.Text_IO.File_Type;
   begin
      XML_Package.Load (XMLNAME, The_Tests);

      if Test_Package.Generate_Flag then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("And_Not.")),
         "and not precedence",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Not_And.")),
         "not and precedence",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Xor.")),
         "xor precedence",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Or.")),
         "or precedence",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Minus.")),
         "minus precedence",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Plus.")),
         "plus precedence",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Paren.")),
         "paren precedence",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Times.")),
         "times precedence",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Divide.")),
         "divide precedence",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Rem.")),
         "rem precedence",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Mod.")),
         "mod precedence",
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
        Test_Package.FILES & "/" & "semantics/expressions.xml";

      The_Tests : XML_Package.Tests_Map.Map;
      The_File  : Ada.Text_IO.File_Type;
   begin
      XML_Package.Load (XMLNAME, The_Tests);

      if Test_Package.Generate_Flag then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Unary-Boolean.")),
         "unary boolean",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Unary-Integer.")),
         "unary integer",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Unary-Mod.")),
         "unary mod",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Binary-Relation.")),
         "binary boolean",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Binary-Boolean.")),
         "binary boolean",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Binary-Integer.")),
         "binary integer",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Binary-Mod.")),
         "binary mod",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Parameter.")),
         "parameter expression",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Index.")),
         "index expression",
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
        Test_Package.FILES & "/" & "semantics/attributes.xml";

      The_Tests : XML_Package.Tests_Map.Map;
      The_File  : Ada.Text_IO.File_Type;
   begin
      XML_Package.Load (XMLNAME, The_Tests);

      if Test_Package.Generate_Flag then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      -- Type array attributes

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Type_Array_Length.")),
         "type array length attribute",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Type_Array_First.")),
         "type array first attribute",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Type_Array_Last.")),
         "type array last attribute",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Type_Array_Size.")),
         "type array size attribute",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      -- Type scalar attributes

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Type_Scalar_First.")),
         "type Scalar first attribute",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Type_Scalar_Last.")),
         "type Scalar last attribute",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Type_Scalar_Size.")),
         "type Scalar size attribute",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      -- Variable array attributes

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_Array_Length.")),
         "variable array length attribute",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_Array_First.")),
         "variable array first attribute",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_Array_Last.")),
         "variable array last attribute",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_Array_Size.")),
         "variable array size attribute",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      -- Variable scalar attributes

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_Scalar_First.")),
         "variable Scalar first attribute",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_Scalar_Last.")),
         "variable Scalar last attribute",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_Scalar_Size.")),
         "variable Scalar size attribute",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      -- Parameter array attributes

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Parameter_Array_Length.")),
         "parameter array length attribute",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Parameter_Array_First.")),
         "parameter array first attribute",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Parameter_Array_Last.")),
         "parameter array last attribute",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Parameter_Array_Size.")),
         "parameter array size attribute",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      -- Parameter scalar attributes

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Parameter_Scalar_First.")),
         "parameter Scalar first attribute",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Parameter_Scalar_Last.")),
         "parameter Scalar last attribute",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Parameter_Scalar_Size.")),
         "parameter Scalar size attribute",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      -- Index attributes

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Index_First.")),
         "index first attribute",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Index_Last.")),
         "index last attribute",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Index_Size.")),
         "index size attribute",
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
        Test_Package.FILES & "/" & "semantics/errors/package.xml";

      The_Tests : XML_Package.Tests_Map.Map;
      The_File  : Ada.Text_IO.File_Type;

   begin
      XML_Package.Load (XMLNAME, The_Tests);

      if Test_Package.Generate_Flag then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Package_Identifier.")),
         "package identifier",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Package_End.")),
         "package end identifier",
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
        Test_Package.FILES & "/" & "semantics/errors/procedure.xml";

      The_Tests : XML_Package.Tests_Map.Map;
      The_File  : Ada.Text_IO.File_Type;

   begin
      XML_Package.Load (XMLNAME, The_Tests);

      if Test_Package.Generate_Flag then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Procedure_Identifier.")),
         "procedure identifier",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Procedure_End.")),
         "procedure end identifier",
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
        Test_Package.FILES & "/" & "semantics/errors/parameter.xml";

      The_Tests : XML_Package.Tests_Map.Map;
      The_File  : Ada.Text_IO.File_Type;

   begin
      XML_Package.Load (XMLNAME, The_Tests);

      if Test_Package.Generate_Flag then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Parameter_Identifier.")),
         "parameter identifier",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Parameter_Excessive.")),
         "parameter excessive",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Parameter_Expected.")),
         "parameter expected",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Parameter_Undefined.")),
         "parameter undefined",
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
        Test_Package.FILES & "/" & "semantics/errors/type.xml";

      The_Tests : XML_Package.Tests_Map.Map;
      The_File  : Ada.Text_IO.File_Type;

   begin
      XML_Package.Load (XMLNAME, The_Tests);

      if Test_Package.Generate_Flag then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Type_Identifier.")),
         "type identifier",
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
        Test_Package.FILES & "/" & "semantics/errors/identifier.xml";

      The_Tests : XML_Package.Tests_Map.Map;
      The_File  : Ada.Text_IO.File_Type;

   begin
      XML_Package.Load (XMLNAME, The_Tests);

      if Test_Package.Generate_Flag then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Identifier.")),
         "identifier",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Identifier_Excessive.")),
         "identifier excessive",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Identifier_Expected.")),
         "identifier expected",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Identifier_Undefined.")),
         "identifier undefined",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Identifier_Not_Compatiable_Type.")),
         "identifier not compatiable type",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Identifier_Not_Compatiable.")),
         "identifier not compatiable",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Identifier_Constant.")),
         "identifier constant",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Identifier_Array.")),
         "identifier array",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Identifier_Constant_Value.")),
         "identifier constant value",
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
        Test_Package.FILES & "/" & "semantics/errors/range.xml";

      The_Tests : XML_Package.Tests_Map.Map;
      The_File  : Ada.Text_IO.File_Type;

   begin
      XML_Package.Load (XMLNAME, The_Tests);

      if Test_Package.Generate_Flag then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Range_Expected.")),
         "range expected",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Range_Undefined.")),
         "range undefined",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Range_Not_Compatiable_Type-First.")),
         "range not compatiable type (first)",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Range_Not_Compatiable-First.")),
         "range not compatiable (first)",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Range_Constant-First.")),
         "range first constant (first)",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Range_Not_Compatiable_Type-Last.")),
         "range last not compatiable type (last)",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Range_Not_Compatiable-Last.")),
         "range not compatiable (last)",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Range_Constant-Last.")),
         "range last constant (last)",
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
        Test_Package.FILES & "/" & "semantics/errors/mod.xml";

      The_Tests : XML_Package.Tests_Map.Map;
      The_File  : Ada.Text_IO.File_Type;

   begin
      XML_Package.Load (XMLNAME, The_Tests);

      if Test_Package.Generate_Flag then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Mod_Not_Compatiable_Type.")),
         "mod not compatiable type",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Mod_Not_Compatiable.")),
         "mod not compatiable",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Mod_Constant.")),
         "mod constant",
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
        Test_Package.FILES & "/" & "semantics/errors/array.xml";

      The_Tests : XML_Package.Tests_Map.Map;
      The_File  : Ada.Text_IO.File_Type;

   begin
      XML_Package.Load (XMLNAME, The_Tests);

      if Test_Package.Generate_Flag then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Array_Excessive-Index.")),
         "array excessive index",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Array_Scalar-Index.")),
         "array scalar index",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Array_Expected-Index.")),
         "array expected index",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Array_Undefined-Index.")),
         "array undefined index",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Array_Constraint-First.")),
         "array constraint first",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Array_Not_Compatiable-First.")),
         "array index not compatiable first",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Array_Constant-First.")),
         "array constant first",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Array_Constraint-Last.")),
         "array constraint last",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Array_Not_Compatiable-Last.")),
         "array index not compatiable last",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Array_Constant-Last.")),
         "array constant last",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Array_Excessive-Element.")),
         "array excessive element",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Array_Scalar-Element.")),
         "array scalar element",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Array_Expected-Element.")),
         "array expected element",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Array_Undefined-Element.")),
         "array undefined element",
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
        Test_Package.FILES & "/" & "semantics/errors/assignment.xml";

      The_Tests : XML_Package.Tests_Map.Map;
      The_File  : Ada.Text_IO.File_Type;

   begin
      XML_Package.Load (XMLNAME, The_Tests);

      if Test_Package.Generate_Flag then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Parameter.")),
         "assignment to in parameter",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Constant.")),
         "assignment to constant",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Constraint.")),
         "assignment not within constraint",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Not_Compatiable.")),
         "assignment not compatiable with identifier",
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
        Test_Package.FILES & "/" & "semantics/errors/variable.xml";

      The_Tests : XML_Package.Tests_Map.Map;
      The_File  : Ada.Text_IO.File_Type;

   begin
      XML_Package.Load (XMLNAME, The_Tests);

      if Test_Package.Generate_Flag then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_Undefined.")),
         "variable undefined",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_Scalar.")),
         "variable scalar",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_Constant.")),
         "variable or constant variable",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_Not_Within.")),
         "variable not within constraint",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_Not_Compatiable.")),
         "variable not compatiable",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_Array.")),
         "array variable or parameter",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_Parameter.")),
         "variable or parameter",
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
        Test_Package.FILES & "/" & "semantics/errors/if.xml";

      The_Tests : XML_Package.Tests_Map.Map;
      The_File  : Ada.Text_IO.File_Type;

   begin
      XML_Package.Load (XMLNAME, The_Tests);

      if Test_Package.Generate_Flag then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Condition_Error.")),
         "boolean condition",
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
        Test_Package.FILES & "/" & "semantics/errors/for.xml";

      The_Tests : XML_Package.Tests_Map.Map;
      The_File  : Ada.Text_IO.File_Type;

   begin
      XML_Package.Load (XMLNAME, The_Tests);

      if Test_Package.Generate_Flag then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Index_Expected.")),
         "index identifier expected",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Index_Undefined.")),
         "index type undefined",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Index_Constraint-First.")),
         "index not within type constraint (first)",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Index_Not_Compatiable-First.")),
         "index not compatiable with type (first)",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Index_Constraint-Last.")),
         "index not within type constraint (last)",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Index_Not_Compatiable-Last.")),
         "index not compatiable with type (last)",
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
        Test_Package.FILES & "/" & "semantics/errors/unary.xml";

      The_Tests : XML_Package.Tests_Map.Map;
      The_File  : Ada.Text_IO.File_Type;

   begin
      XML_Package.Load (XMLNAME, The_Tests);

      if Test_Package.Generate_Flag then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Boolean_Type.")),
         "not within unary operator type",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Boolean_Compatiable.")),
         "not compatiable with unary operator",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Integer_Type.")),
         "not within unary operator type",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Integer_Compatiable.")),
         "not compatiable with unary operator",
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
        Test_Package.FILES & "/" & "semantics/errors/binary.xml";

      The_Tests : XML_Package.Tests_Map.Map;
      The_File  : Ada.Text_IO.File_Type;

   begin
      XML_Package.Load (XMLNAME, The_Tests);

      if Test_Package.Generate_Flag then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Expressions_Not_Compatiable.")),
         "expressions not compatiable",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Type_Error-Boolean.")),
         "not within binary operator",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Boolean_Error.")),
         "not compatiable with binary operator",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Type_Error-Integer.")),
         "not within binary operator",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Integer_Error.")),
         "not compatiable with binary operator",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Operands_Not_Compatiable.")),
         "operands not compatiable",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      if Test_Package.Generate_Flag then
         Close_Generate_File (The_File);

         if Test_Package.Replace_Flag then
            Replace_XMLfile (XMLNAME, LISTNAME);
         end if;
      end if;
   end Test_Binary_Errors;

end Semantics_Package.Semantics_Test;
