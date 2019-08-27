-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with AUnit.Assertions;
--
with Ada.Text_IO;
--
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--
with Error_Package.Clear;
--
with Test_Package; use Test_Package;
--
with System_Package; use System_Package;
with Source_Package; use Source_Package;
--

package body Error_Package.Error_Test is

   LISTNAME : constant String := Test_Package.FILES & "/" & "error.txt";

   ERROR_LISTING : constant Test_Package.Array_Of_Unbounded_Strings :=
     (To_Unbounded_String ("1, 10: Warning: First message"),
      To_Unbounded_String ("1, 15: Second message"),
      To_Unbounded_String ("1, 15: Thrid message"),
      To_Unbounded_String ("2, 1: Warning: Forth message"),
      To_Unbounded_String ("2, 10: Warning: Fifth message"));

   COMPILER_ERROR_MESSAGE : constant Test_Package.Array_Of_Unbounded_Strings :=
     (1 => To_Unbounded_String ("*** A compiler error."));

   COMPILER_ERROR_POSITION_MESSAGE : constant Test_Package
     .Array_Of_Unbounded_Strings :=
       (1 => To_Unbounded_String ("*** 1, 1: A compiler error."));

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
      return AUnit.Format ("error_package.error_test!");
   end Name;

   --------------------
   -- Register_Tests --
   --------------------

   overriding procedure Register_Tests (The_Test : in out Test) is
      use Registration;
   begin
      Register_Routine(The_Test, Test_Source_Error'Access, "test_source_error!");
      Register_Routine(The_Test, Test_Source_Warning'Access, "test_source_warning!");
      Register_Routine(The_Test, Test_Compiler_Error'Access, "test_compiler_error!");
      Register_Routine(The_Test, Test_Compiler_Error_With_Position'Access, "test_compiler_error_with_position!");
      Register_Routine(The_Test, Test_List_Messages'Access, "test_list_messages!");
      Register_Routine(The_Test, Test_Iterate'Access, "test_iterate!");
      Register_Routine(The_Test, Test_Is_Less_Than_Equal'Access, "test_is_less_than_equal!");
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

   ------------
   -- Assert --
   ------------

   procedure Assert
     (The_Result   : Message_Type;
      The_Kind     : Message_Kind;
      The_Expected : Test_Message;
      The_Test     : String)
   is
   begin
      AUnit.Assertions.Assert
        (The_Result.The_Kind = The_Kind,
         The_Test & " (message kind).");
      AUnit.Assertions.Assert
        (The_Result.The_Text = The_Expected.The_Message,
         The_Test & " (message).");
      AUnit.Assertions.Assert
        (The_Result.The_Position = The_Expected.The_Position,
         The_Test & " (position).");
   end Assert;

   -----------------------
   -- Test_Source_Error --
   -----------------------

   procedure Test_Source_Error (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      The_Message : Message_Type;
      The_Cursor  : Cursor;
   begin
      Source_Error
        (MESSAGES (2).The_Position,
         To_String (MESSAGES (2).The_Message));
      Source_Error
        (MESSAGES (3).The_Position,
         To_String (MESSAGES (3).The_Message));

      The_Cursor  := First (The_Messages);
      The_Message := Element (The_Cursor);

      Assert
        (The_Result   => The_Message,
         The_Kind     => Error_Kind,
         The_Expected => MESSAGES (2),
         The_Test     => "Test 1 source error");

      The_Cursor  := Next (The_Cursor);
      The_Message := Element (The_Cursor);

      Assert
        (The_Result   => The_Message,
         The_Kind     => Error_Kind,
         The_Expected => MESSAGES (3),
         The_Test     => "Test 3 source error");
   end Test_Source_Error;

   -------------------------
   -- Test_Source_Warning --
   -------------------------

   procedure Test_Source_Warning (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      The_Message : Message_Type;
      The_Cursor  : Cursor;
   begin
      Source_Warning
        (MESSAGES (1).The_Position,
         To_String (MESSAGES (1).The_Message));
      Source_Warning
        (MESSAGES (4).The_Position,
         To_String (MESSAGES (4).The_Message));
      Source_Warning
        (MESSAGES (5).The_Position,
         To_String (MESSAGES (5).The_Message));

      The_Cursor  := First (The_Messages);
      The_Message := Element (The_Cursor);

      Assert
        (The_Result   => The_Message,
         The_Kind     => Warning_Kind,
         The_Expected => MESSAGES (1),
         The_Test     => "Test 1 source warning");

      The_Cursor  := Next (The_Cursor); -- 2nd message
      The_Cursor  := Next (The_Cursor); -- 3rd message
      The_Cursor  := Next (The_Cursor); -- 4th message
      The_Message := Element (The_Cursor);

      Assert
        (The_Result   => The_Message,
         The_Kind     => Warning_Kind,
         The_Expected => MESSAGES (4),
         The_Test     => "Test 2 source warning");

      The_Cursor  := Next (The_Cursor); -- 5th message
      The_Message := Element (The_Cursor);

      Assert
        (The_Result   => The_Message,
         The_Kind     => Warning_Kind,
         The_Expected => MESSAGES (5),
         The_Test     => "Test 3 source warning");
   end Test_Source_Warning;

   -------------------------
   -- Test_Compiler_Error --
   -------------------------

   procedure Raise_Compiler_Error is
   begin
      Compiler_Error ("A compiler error.");
   end Raise_Compiler_Error;

   procedure Test_Compiler_Error (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      The_File : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Create (The_File, Ada.Text_IO.Out_File, LISTNAME);
      Ada.Text_IO.Set_Output (The_File);

      AUnit.Assertions.Assert_Exception
        (Raise_Compiler_Error'Access,
         "Test compiler error.");

      Ada.Text_IO.Close (The_File);
      Ada.Text_IO.Set_Output (Ada.Text_IO.Standard_Output);

      AUnit.Assertions.Assert
        (Test_Package.Compare_File_To_Strings
           (LISTNAME,
            COMPILER_ERROR_MESSAGE),
         "Test error listing");

      Ada.Text_IO.Open (The_File, Ada.Text_IO.Out_File, LISTNAME);
      Ada.Text_IO.Delete (The_File);
   end Test_Compiler_Error;

   ---------------------------------------
   -- Test_Compiler_Error_With_Position --
   ---------------------------------------

   procedure Raise_Compiler_Error_With_Position is
   begin
      Compiler_Error ((1, 1), "A compiler error.");
   end Raise_Compiler_Error_With_Position;

   procedure Test_Compiler_Error_With_Position
     (The_Test : in out Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      The_File : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Create (The_File, Ada.Text_IO.Out_File, LISTNAME);
      Ada.Text_IO.Set_Output (The_File);

      AUnit.Assertions.Assert_Exception
        (Raise_Compiler_Error_With_Position'Access,
         "Test compiler error.");

      Ada.Text_IO.Close (The_File);
      Ada.Text_IO.Set_Output (Ada.Text_IO.Standard_Output);

      AUnit.Assertions.Assert
        (Test_Package.Compare_File_To_Strings
           (LISTNAME,
            COMPILER_ERROR_POSITION_MESSAGE),
         "Test error listing");

      Ada.Text_IO.Open (The_File, Ada.Text_IO.Out_File, LISTNAME);
      Ada.Text_IO.Delete (The_File);
   end Test_Compiler_Error_With_Position;

   ------------------------
   -- Test_List_Messages --
   ------------------------

   procedure Test_List_Messages (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      The_File : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Create (The_File, Ada.Text_IO.Out_File, LISTNAME);
      Ada.Text_IO.Set_Output (The_File);

      List_Messages;
      Ada.Text_IO.Close (The_File);
      Ada.Text_IO.Set_Output (Ada.Text_IO.Standard_Output);

      AUnit.Assertions.Assert
        (Test_Package.Compare_File_To_Strings (LISTNAME, ERROR_LISTING),
         "Test error listing");

      Ada.Text_IO.Open (The_File, Ada.Text_IO.Out_File, LISTNAME);
      Ada.Text_IO.Delete (The_File);
   end Test_List_Messages;

   ------------------
   -- Test_Iterate --
   ------------------

   procedure Test_Iterate (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      Pass      : Boolean;
      The_Index : Positive;

      procedure Test
        (The_Position : Source_Position;
         The_Kind     : Message_Kind;
         The_Message  : String)
      is
      begin
         if MESSAGES (The_Index).The_Position /= The_Position
           or else To_String (MESSAGES (The_Index).The_Message) /= The_Message
         then
            Pass := False;
         end if;
         The_Index := The_Index + 1;
      end Test;

   begin
      Pass      := True;
      The_Index := 1;
      Iterate (Test'Access);
      AUnit.Assertions.Assert (Pass, "Test iterate");
   end Test_Iterate;

   -----------------------------
   -- Test_Is_Less_Than_Equal --
   -----------------------------

   procedure Test_Is_Less_Than_Equal (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

   begin
      AUnit.Assertions.Assert
        (Is_Less_Than_Equal ((1, 1), (1, 2)),
         "Test 1 is less than.");
      AUnit.Assertions.Assert
        (Is_Less_Than_Equal ((1, 1), (2, 1)),
         "Test 2 is less than.");
      AUnit.Assertions.Assert
        (not Is_Less_Than_Equal ((1, 2), (1, 1)),
         "Test 3 is less than.");
      AUnit.Assertions.Assert
        (not Is_Less_Than_Equal ((2, 1), (1, 1)),
         "Test 4 is less than.");
   end Test_Is_Less_Than_Equal;

end Error_Package.Error_Test;
