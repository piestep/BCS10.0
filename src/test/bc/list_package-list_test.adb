-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with AUnit.Assertions;
--
with Ada.Text_IO;
--
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--
with Test_Package;
--
with Error_Package.Clear;
--
with System_Package; use System_Package;
with Error_Package;  use Error_Package;
with Source_Package; use Source_Package;
--

package body List_Package.List_Test is

   SOURCE_FILENAME : constant String := Test_Package.FILES & "/" & "list.txt";
   LISTNAME        : constant String := Test_Package.FILES & "/" & "list.tmp";

   LISTING : constant Test_Package.Array_Of_Unbounded_Strings :=
     (To_Unbounded_String ("    1: A line with errors."),
      To_Unbounded_String ("               ^ Warning: First message"),
      To_Unbounded_String ("                    ^ Second message"),
      To_Unbounded_String ("                    ^ Thrid message"),
      To_Unbounded_String ("    2: A second line with errors."),
      To_Unbounded_String ("      ^ Warning: Forth message"),
      To_Unbounded_String ("               ^ Warning: Fifth message"),
      To_Unbounded_String ("    3: "),
      To_Unbounded_String ("    4: A line with no errors."));

   type Test_Message is record
      The_Position : Source_Position;
      The_Message  : Unbounded_String;
   end record;

   MESSAGES : array (1 .. 5) of Test_Message :=
     (((1, 10), To_Unbounded_String ("First message")),
      ((1, 15), To_Unbounded_String ("Second message")),
      ((1, 15), To_Unbounded_String ("Thrid message")),
      ((2, 1), To_Unbounded_String ("Forth message")),
      ((2, 10), To_Unbounded_String ("Fifth message")));

   ----------
   -- Name --
   ----------

   overriding function Name (The_Test : in Test) return AUnit.Test_String is
   begin
      return AUnit.Format ("list_package.list_test!");
   end Name;

   --------------------
   -- Register_Tests --
   --------------------

   overriding procedure Register_Tests (The_Test : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine(The_Test, Test_Listing'Access, "test_listing!");
   end Register_Tests;

   -----------------
   -- Set_Up_Case --
   -----------------

   overriding procedure Set_Up_Case (The_Test : in out Test) is
   begin
      Source_Warning
        (MESSAGES (1).The_Position,
         To_String (MESSAGES (1).The_Message));

      Source_Error
        (MESSAGES (2).The_Position,
         To_String (MESSAGES (2).The_Message));
      Source_Error
        (MESSAGES (3).The_Position,
         To_String (MESSAGES (3).The_Message));

      Source_Warning
        (MESSAGES (4).The_Position,
         To_String (MESSAGES (4).The_Message));
      Source_Warning
        (MESSAGES (5).The_Position,
         To_String (MESSAGES (5).The_Message));
   end Set_Up_Case;

   --------------------
   -- Tear_Down_Case --
   --------------------

   overriding procedure Tear_Down_Case (The_Test : in out Test) is
   begin
      Error_Package.Clear;
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

   ------------------
   -- Test_Listing --
   ------------------

   procedure Test_Listing (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      The_File : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Create (The_File, Ada.Text_IO.Out_File, LISTNAME);
      Ada.Text_IO.Set_Output (The_File);

      List (SOURCE_FILENAME);

      Ada.Text_IO.Close (The_File);
      Ada.Text_IO.Set_Output (Ada.Text_IO.Standard_Output);

      AUnit.Assertions.Assert
        (Test_Package.Compare_File_To_Strings (LISTNAME, LISTING),
         "Test listing.");

      Ada.Text_IO.Open (The_File, Ada.Text_IO.Out_File, LISTNAME);
      Ada.Text_IO.Delete (The_File);
   end Test_Listing;

end List_Package.List_Test;
