-- BC Boolean Compiler
-- Copyright (c) 2015 Paul Estep

with AUnit.Assertions;
--
with Ada.Text_IO;
with Ada.Directories;
--
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--
with Test_Package;
with XML_Package;
--
with Error_Package.Clear;
with Graph_Package.List;
with Graph_Package.Dump;
with Graph_Package.Count;
with List_Package;
with Pool_Package;
--
with System_Package;     use System_Package;
with Source_Package;     use Source_Package;
with Scanner_Package;    use Scanner_Package;
with Type_Package;       use Type_Package;
with Identifier_Package; use Identifier_Package;
with Graph_Package;      use Graph_Package;
with Error_Package;      use Error_Package;
--

package body Syntax_Package.Syntax_Test is

   FILENAME : constant String := Test_Package.FILES & "/" & "syntax.bc";
   LISTNAME : constant String := Test_Package.FILES & "/" & "syntax.tmp";

   ----------
   -- Name --
   ----------

   function Name (The_Test : in Test) return AUnit.Test_String is
   begin
      return AUnit.Format ("syntax_package.syntax_test!");
   end Name;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (The_Test : in out Test) is
      use Registration;
   begin
      Register_Routine(The_Test, Test_Procedure'Access, "test_procedure!");
      Register_Routine(The_Test, Test_Scalar_Declarations'Access, "test_scalar_declarations!");
      Register_Routine(The_Test, Test_Mod_Declaration'Access, "test_mod_declaration!");
      Register_Routine(The_Test, Test_Array_Declaration'Access, "test_array_declaration!");
      Register_Routine(The_Test, Test_Identifier_Declaration'Access, "test_identifier_declaration!");
      Register_Routine(The_Test, Test_Parameters'Access, "test_parameters!");
      Register_Routine(The_Test, Test_Assignments'Access, "test_assignments!");
      Register_Routine(The_Test, Test_Ifs'Access, "test_ifs!");
      Register_Routine(The_Test, Test_Fors'Access, "test_fors!");
      Register_Routine(The_Test, Test_Statements'Access, "test_statements!");
      Register_Routine(The_Test, Test_Operators'Access, "test_operators!");
      Register_Routine(The_Test, Test_Precedences'Access, "test_precedences!");
      Register_Routine(The_Test, Test_Expressions'Access, "test_expressions!");
      Register_Routine(The_Test, Test_Syntax_Errors'Access, "test_syntax_errors!");
   end Register_Tests;

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
      Type_Package.Clear;
      Identifier_Package.Clear;
   end Tear_Down;

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

      if Dump then
         List_Package.List (FILENAME, XML_Format => False);
      end if;

      if not (Dump or Generate) then
         AUnit.Assertions.Assert
           (The_Number_Of_Errors = 0,
            "Test " & The_Message & " syntax errors.");
         AUnit.Assertions.Assert
           (The_Number_Of_Warnings = 0,
            "Test " & The_Message & " syntax warnngs.");
      end if;

      if not Generate then
         Ada.Text_IO.Create (The_File, Ada.Text_IO.Out_File, LISTNAME);
         Ada.Text_IO.Set_Output (The_File);
      end if;

      if The_Number_Of_Errors = 0 and The_Number_Of_Warnings = 0 then

         if Generate then
            Graph_Package.Dump
              (The_Unit,
               Semantic_Dump => False,
               Full_Dump     => False,
               XML_Format    => True);

         else
            Graph_Package.Dump
              (The_Unit,
               Semantic_Dump => False,
               Full_Dump     => False,
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

      The_File   : Ada.Text_IO.File_Type;
      The_Cursor : XML_Package.Strings_Vector.Cursor;
      The_Unit   : Compilation_Unit_Graph;
   begin
      Error_Package.Clear;
      Test_Package.Save (FILENAME, The_Test.The_Code);

      if Generate then
         Ada.Text_IO.Put_Line ("<test>");
         Ada.Text_IO.Put_Line
           ("<name>" & To_String (The_Test.The_Name) & "</name>");

         Test_Package.List (The_Test.The_Code, XML_Format => True);
      end if;

      Open (FILENAME);

      Next_Symbol;
      Syntax_Package.Parse (The_Unit);

      Close;

      if Dump then
         List_Package.List (FILENAME, XML_Format => False);
      end if;

      if not (Dump or Generate) then
         AUnit.Assertions.Assert
           (The_Number_Of_Errors > 0 or The_Number_Of_Warnings > 0,
            "Test " & The_Message & " syntax errors and warnings.");
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
        Test_Package.FILES & "/" & "syntax/test_procedure.xml";

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

   ------------------------------
   -- Test_Scalar_Declarations --
   ------------------------------

   procedure Test_Scalar_Declarations (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "syntax/test_scalar.xml";

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
            To_Unbounded_String ("Scalar_Declaration.")),
         "scalar declaration",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Scalar_Range_Declaration.")),
         "scalar range declaration",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      if Test_Package.Generate_Flag then
         Close_Generate_File (The_File);

         if Test_Package.Replace_Flag then
            Replace_XMLfile (XMLNAME, LISTNAME);
         end if;
      end if;
   end Test_Scalar_Declarations;

   --------------------------
   -- Test_Mod_Declaration --
   --------------------------

   procedure Test_Mod_Declaration (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String := Test_Package.FILES & "/" & "syntax/test_mod.xml";

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
            To_Unbounded_String ("Mod_Declaration.")),
         "mod declaration",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      if Test_Package.Generate_Flag then
         Close_Generate_File (The_File);

         if Test_Package.Replace_Flag then
            Replace_XMLfile (XMLNAME, LISTNAME);
         end if;
      end if;
   end Test_Mod_Declaration;

   ----------------------------
   -- Test_Array_Declaration --
   ----------------------------

   procedure Test_Array_Declaration (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "syntax/test_array.xml";

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

   ----------------------------
   -- Test_Identifier_Declaration --
   ----------------------------

   procedure Test_Identifier_Declaration (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "syntax/test_identifiers.xml";

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
        Test_Package.FILES & "/" & "syntax/test_parameters.xml";

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
            To_Unbounded_String ("Parameter_In.")),
         "parameter in",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Parameter_Out.")),
         "parameter out",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Parameter_In_Out.")),
         "parameter in out",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Parameter_List.")),
         "parameter list",
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
        Test_Package.FILES & "/" & "syntax/test_assignments.xml";

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
            To_Unbounded_String ("Array_Assignment.")),
         "array assignment",
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

      XMLNAME : constant String := Test_Package.FILES & "/" & "syntax/test_ifs.xml";

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

      XMLNAME : constant String := Test_Package.FILES & "/" & "syntax/test_fors.xml";

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

   ---------------------
   -- Test_Statements --
   ---------------------

   procedure Test_Statements (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "syntax/test_statements.xml";

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
            To_Unbounded_String ("Statement_List.")),
         "Statement list",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      if Test_Package.Generate_Flag then
         Close_Generate_File (The_File);

         if Test_Package.Replace_Flag then
            Replace_XMLfile (XMLNAME, LISTNAME);
         end if;
      end if;
   end Test_Statements;

   --------------------
   -- Test_Operators --
   --------------------

   procedure Test_Operators (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "syntax/test_operators.xml";

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

   ----------------------
   -- Test_Precedences --
   ----------------------

   procedure Test_Precedences (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "syntax/test_precedences.xml";

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
            To_Unbounded_String ("And_Not_Precedence.")),
         "and not precedence",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Xor_Precedence.")),
         "xor precedence",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Or_Precedence.")),
         "or precedence",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Negate_Precedence.")),
         "minus precedence",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Plus_Precedence.")),
         "plus precedence",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Paren_Precedence.")),
         "paren precedence",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Times_Precedence.")),
         "times precedence",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Divide_Precedence.")),
         "divide precedence",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Rem_Precedence.")),
         "rem precedence",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Mod_Precedence.")),
         "mod precedence",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      if Test_Package.Generate_Flag then
         Close_Generate_File (The_File);

         if Test_Package.Replace_Flag then
            Replace_XMLfile (XMLNAME, LISTNAME);
         end if;
      end if;
   end Test_Precedences;

   ----------------------
   -- Test_Expressions --
   ----------------------

   procedure Test_Expressions (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "syntax/test_expressions.xml";

      The_Tests : XML_Package.Tests_Map.Map;
      The_File  : Ada.Text_IO.File_Type;
   begin
      XML_Package.Load (XMLNAME, The_Tests);

      if Test_Package.Generate_Flag then
         Ada.Text_IO.Create (The_File, Ada.Text_IO.Out_File, LISTNAME);
         Ada.Text_IO.Set_Output (The_File);

         Ada.Text_IO.Put_Line ("<?xml version=""1.0""?>");
         Ada.Text_IO.Put_Line ("<body>");
      end if;

      Run_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Tick.")),
         "tick",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      if Test_Package.Generate_Flag then
         Ada.Text_IO.Put_Line ("</body>");
         Ada.Text_IO.Set_Output (Ada.Text_IO.Standard_Output);
      end if;
   end Test_Expressions;

   ------------------------
   -- Test_Syntax_Errors --
   ------------------------

   procedure Test_Syntax_Errors (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      XMLNAME : constant String :=
        Test_Package.FILES & "/" & "syntax/test_errors.xml";

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
            To_Unbounded_String ("Accept.")),
         "accept symbol",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Starters.")),
         "starter symbols",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Followers.")),
         "follower symbols",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      Run_Error_Tests
        (XML_Package.Tests_Map.Element
           (The_Tests,
            To_Unbounded_String ("Eof.")),
         "eof",
         Dump     => Test_Package.Dump_Flag,
         Generate => Test_Package.Generate_Flag);

      if Test_Package.Generate_Flag then
         Close_Generate_File (The_File);

         if Test_Package.Replace_Flag then
            Replace_XMLfile (XMLNAME, LISTNAME);
         end if;
      end if;
   end Test_Syntax_Errors;

end Syntax_Package.Syntax_Test;
