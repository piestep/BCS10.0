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

package body Optimize_Package.Optimize_Test is

   FILENAME : constant String := Test_Package.FILES & "/" & "optimize.bc";
   LISTNAME : constant String := Test_Package.FILES & "/" & "optimize.tmp";

   The_Unmarked_Graph_Allocations : SYSNatural;

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

      The_Unmarked_Graph_Allocations :=
        Pool_Package.Unmarked_Allocations (Graph_Package.The_Pool);
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

      Semantics_Package.Parse (The_Unit);

      if not (Dump or Generate) then
         AUnit.Assertions.Assert
           (The_Number_Of_Errors = 0,
            "Test " & The_Message & " semantic errors.");
         AUnit.Assertions.Assert
           (The_Number_Of_Warnings = 0,
            "Test " & The_Message & " semantic warnngs.");
      end if;

      Optimize (The_Unit);

      if Dump then
         List_Package.List (FILENAME, XML_Format => False);
      end if;

      if not (Dump or Generate) then
         AUnit.Assertions.Assert
           (The_Number_Of_Errors = 0,
            "Test " & The_Message & " optimize errors.");
         AUnit.Assertions.Assert
           (The_Number_Of_Warnings = 0,
            "Test " & The_Message & " optimize warnngs.");
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

      if not (Dump or Generate) then
         AUnit.Assertions.Assert
           (The_Number_Of_Errors = 0,
            "Test " & The_Message & " semantic errors.");
         AUnit.Assertions.Assert
           (The_Number_Of_Warnings = 0,
            "Test " & The_Message & " semantic warnngs.");
      end if;

      Optimize (The_Unit);

      if Dump then
         List_Package.List (FILENAME, XML_Format => False);
      end if;

      if not (Dump or Generate) then
         AUnit.Assertions.Assert
           (The_Number_Of_Errors > 0 or The_Number_Of_Warnings > 0,
            "Test " & The_Message & " optimize errors and warnings.");
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
        Test_Package.FILES & "/" & "optimize/procedure.xml";

      The_Tests : XML_Package.Tests_Map.Map;
      The_File  : Ada.Text_IO.File_Type;

   begin
      XML_Package.Load (XMLNAME, The_Tests);

      if Optimize_Generate then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Procedure.")),
         "procedure",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      if Optimize_Generate then
         Close_Generate_File (The_File);

         if Optimize_Replace then
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

      if Optimize_Generate then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Scalar.")),
         "scalar type",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      if Optimize_Generate then
         Close_Generate_File (The_File);

         if Optimize_Replace then
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

      if Optimize_Generate then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Mod.")),
         "mod type",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Mod_Without_Range.")),
         "mod type without range",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      if Optimize_Generate then
         Close_Generate_File (The_File);

         if Optimize_Replace then
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

      if Optimize_Generate then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Scalar.")),
         "scalar range type",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Mod.")),
         "mod range type",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      if Optimize_Generate then
         Close_Generate_File (The_File);

         if Optimize_Replace then
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

      if Optimize_Generate then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Array.")),
         "array type",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      if Optimize_Generate then
         Close_Generate_File (The_File);

         if Optimize_Replace then
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

      if Optimize_Generate then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Identifier.")),
         "identifier declaration",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Identifier_Expression.")),
         "identifier expression declaration",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Identifier_Constant.")),
         "identifier constant declaration",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      if Optimize_Generate then
         Close_Generate_File (The_File);

         if Optimize_Replace then
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
      XML_Package.Load (XMLNAME, The_Tests);

      if Optimize_Generate then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Assignment.")),
         "scalar assignment",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Constant_Index.")),
         "array constant index assignment",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_Index.")),
         "array variable index assignment",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      if Optimize_Generate then
         Close_Generate_File (The_File);

         if Optimize_Replace then
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

      if Optimize_Generate then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("If_Then.")),
         "if then",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("If_Then_Else.")),
         "if then else",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      if Optimize_Generate then
         Close_Generate_File (The_File);

         if Optimize_Replace then
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

      if Optimize_Generate then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("For.")),
         "for statement",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      if Optimize_Generate then
         Close_Generate_File (The_File);

         if Optimize_Replace then
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

      if Optimize_Generate then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Boolean_Constant.")),
         "unary boolean constant expression",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Boolean_Variable.")),
         "unary boolean variable expression",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Integer_Constant.")),
         "unary integer constant expression",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Integer_Variable.")),
         "unary integer variable expression",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      if Optimize_Generate then
         Close_Generate_File (The_File);

         if Optimize_Replace then
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

      if Optimize_Generate then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Constant.")),
         "binary relation constant expression",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable.")),
         "binary relation variable expression",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      if Optimize_Generate then
         Close_Generate_File (The_File);

         if Optimize_Replace then
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

      if Optimize_Generate then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Constant.")),
         "binary and constant expression",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("False_And_Variable.")),
         "binary and variable expression",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_And_False.")),
         "binary and variable expression",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("True_And_Variable.")),
         "binary and variable expression",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_And_True.")),
         "binary and variable expression",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable.")),
         "binary and variable expression",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      if Optimize_Generate then
         Close_Generate_File (The_File);

         if Optimize_Replace then
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

      if Optimize_Generate then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Constant.")),
         "binary or constant expression",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("False_Or_Variable.")),
         "binary or variable expression",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_Or_False.")),
         "binary or variable expression",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("True_Or_Variable.")),
         "binary or variable expression",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_Or_True.")),
         "binary or variable expression",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable.")),
         "binary or variable expression",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      if Optimize_Generate then
         Close_Generate_File (The_File);

         if Optimize_Replace then
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

      if Optimize_Generate then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Constant.")),
         "binary xor constant expression",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("False_Xor_Variable.")),
         "binary xor variable expression",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_Xor_False.")),
         "binary xor variable expression",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("True_Xor_Variable.")),
         "binary xor variable expression",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_Xor_True.")),
         "binary xor variable expression",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable.")),
         "binary xor variable expression",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      if Optimize_Generate then
         Close_Generate_File (The_File);

         if Optimize_Replace then
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

      if Optimize_Generate then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Constant.")),
         "binary addition constant expression",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Zero_Plus_Variable.")),
         "binary addition variable expression",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_Plus_Zero.")),
         "binary addition variable expression",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("One_Plus_Variable.")),
         "binary addition variable expression",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_Plus_One.")),
         "binary addition variable expression",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable.")),
         "binary addition variable expression",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      if Optimize_Generate then
         Close_Generate_File (The_File);

         if Optimize_Replace then
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

      if Optimize_Generate then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Constant.")),
         "binary subtraction constant expression",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Zero_Minus_Variable.")),
         "binary subtraction variable expression",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_Minus_Zero.")),
         "binary subtraction variable expression",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("One_Minus_Variable.")),
         "binary subtraction variable expression",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_Minus_One.")),
         "binary subtraction variable expression",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable.")),
         "binary subtraction variable expression",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      if Optimize_Generate then
         Close_Generate_File (The_File);

         if Optimize_Replace then
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

      if Optimize_Generate then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Constant.")),
         "binary multiplication constant expression",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Zero_Times_Variable.")),
         "binary multiplication variable expression",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_Times_Zero.")),
         "binary multiplication variable expression",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("One_Times_Variable.")),
         "binary multiplication variable expression",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_Times_One.")),
         "binary multiplication variable expression",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Two_Times_Variable.")),
         "binary multiplication variable expression",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_Times_Two.")),
         "binary multiplication variable expression",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable.")),
         "binary multiplication variable expression",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      if Optimize_Generate then
         Close_Generate_File (The_File);

         if Optimize_Replace then
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

      if Optimize_Generate then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Constant.")),
         "binary division constant expression",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Zero_Divide_Variable.")),
         "binary division variable expression",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("One_Divide_Variable.")),
         "binary division variable expression",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_Divide_One.")),
         "binary division variable expression",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Two_Divide_Variable.")),
         "binary division variable expression",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable_Divide_Two.")),
         "binary division variable expression",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Variable.")),
         "binary division variable expression",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      if Optimize_Generate then
         Close_Generate_File (The_File);

         if Optimize_Replace then
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

      if Optimize_Generate then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Parameter.")),
         "parameter expression",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      if Optimize_Generate then
         Close_Generate_File (The_File);

         if Optimize_Replace then
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

      if Optimize_Generate then
         Create_Generate_File (The_File, LISTNAME);
      end if;

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Index.")),
         "index expression",
         Dump     => Optimize_Dump,
         Generate => Optimize_Generate);

      if Optimize_Generate then
         Close_Generate_File (The_File);

         if Optimize_Replace then
            Replace_XMLfile (XMLNAME, LISTNAME);
         end if;
      end if;
   end Test_Index;

end Optimize_Package.Optimize_Test;
