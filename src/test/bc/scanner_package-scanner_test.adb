-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with AUnit.Assertions;
--
with Test_Package;
--
with Ada.Text_IO;
--
with System_Package; use System_Package;
with Source_Package; use Source_Package;
--

package body Scanner_Package.Scanner_Test is

   FILENAME : constant String := Test_Package.FILES & "/" & "scanner.txt";

   type Symbol_Record is record
      The_Symbol : Symbol;
      The_Column : Column_Position;
   end record;

   The_Records : constant array (1 .. 48) of Symbol_Record :=
     ((Identifier_Symbol, 2),
      (Identifier_Symbol, 2),
      (Integer_Symbol, 3),
      (Plus_Symbol, 2),
      (Minus_Symbol, 2),
      (Times_Symbol, 2),
      (Divide_Symbol, 2),
      (Mod_Symbol, 4),
      (Rem_Symbol, 4),
      (And_Symbol, 4),
      (Or_Symbol, 3),
      (Xor_Symbol, 4),
      (Not_Symbol, 4),
      (Equal_Symbol, 2),
      (Not_Equal_Symbol, 3),
      (Less_Than_Symbol, 2),
      (Greater_Than_Symbol, 2),
      (Less_Than_Equal_Symbol, 3),
      (Greater_Than_Equal_Symbol, 3),
      (Left_Paren_Symbol, 2),
      (Right_Paren_Symbol, 2),
      (Tick_Symbol, 2),
      (Thru_Symbol, 3),
      (Becomes_Symbol, 3),
      (Period_Symbol, 2),
      (Comma_Symbol, 2),
      (Colon_Symbol, 2),
      (Semicolon_Symbol, 2),
      (Package_Symbol, 8),
      (Procedure_Symbol, 10),
      (In_Symbol, 3),
      (Out_Symbol, 4),
      (Is_Symbol, 3),
      (Type_Symbol, 5),
      (Constant_Symbol, 9),
      (New_Symbol, 4),
      (Range_Symbol, 6),
      (Array_Symbol, 6),
      (Of_Symbol, 3),
      (Begin_Symbol, 6),
      (Null_Symbol, 5),
      (If_Symbol, 3),
      (Then_Symbol, 5),
      (Else_Symbol, 5),
      (For_Symbol, 4),
      (Reverse_Symbol, 8),
      (Loop_Symbol, 5),
      (End_Symbol, 4));

   ----------
   -- Name --
   ----------

   function Name (The_Test : in Test) return AUnit.Test_String is
   begin
      return AUnit.Format ("scanner_package.scanner_test!");
   end Name;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (The_Test : in out Test) is
      use Registration;
   begin
      -- Repeat for each test routine:
      Register_Routine(The_Test, Test_Scanner'Access, "test_scanner!");
  end Register_Tests;

   -------------------
   -- Assert_Symbol --
   -------------------

   procedure Assert_Symbol
     (The_Symbol   : Symbol;
      The_Position : Source_Position;
      The_Test     : String)
   is
   begin
      AUnit.Assertions.Assert
        (Scanner_Package.The_Symbol = The_Symbol,
         The_Test & " Scanner (symbol). " &
         Symbol'Image (Scanner_Package.The_Symbol) &
         " -- " &
           Symbol'Image (The_Symbol));

      AUnit.Assertions.Assert
        (Scanner_Package.The_Position = The_Position,
         The_Test & " Scanner (position). " &
         Image_Of (Scanner_Package.The_Position) &
         " -- " &
         Image_Of (The_Position));
   end Assert_Symbol;

   -----------------------
   -- Assert_Identifier --
   -----------------------

   procedure Assert_Identifier
     (The_String   : String;
      The_Position : Source_Position;
      The_Test     : String)
   is
   begin
      AUnit.Assertions.Assert
        (The_Symbol = Identifier_Symbol,
         The_Test & " Scanner (symbol). " &
         Symbol'Image (The_Symbol) &
         " -- " &
           Symbol'Image (Identifier_Symbol));

      AUnit.Assertions.Assert
        (Scanner_Package.The_Position = The_Position,
         The_Test & " Scanner (position). " &
         Image_Of (Scanner_Package.The_Position) &
         " -- " &
         Image_Of (The_Position));

      AUnit.Assertions.Assert
        (To_String (Scanner_Package.The_String) = The_String,
         The_Test & " Scanner (identifier). """ &
         To_String (Scanner_Package.The_String) &
         """ -- """ &
         The_String &
           """");
   end Assert_Identifier;

   --------------------
   -- Assert_Integer --
   --------------------

   procedure Assert_Integer
     (The_String   : String;
      The_Position : Source_Position;
      The_Test     : String)
   is
   begin
      AUnit.Assertions.Assert
        (The_Symbol = Integer_Symbol,
         The_Test & " Scanner (symbol). " &
         Symbol'Image (The_Symbol) &
         " -- " &
           Symbol'Image (Integer_Symbol));

      AUnit.Assertions.Assert
        (Scanner_Package.The_Position = The_Position,
         The_Test & " Scanner (position). " &
         Image_Of (Scanner_Package.The_Position) &
         " -- " &
         Image_Of (The_Position));

      AUnit.Assertions.Assert
        (To_String (Scanner_Package.The_String) = The_String,
         The_Test & " Scanner (integer). """ &
         To_String (Scanner_Package.The_String) &
         """ -- """ &
         The_String &
           """");
   end Assert_Integer;

   -----------------
   -- Test_Scanner --
   -----------------

   procedure Test_Scanner (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

   begin
      Open (FILENAME);
      Next_Symbol;

      Assert_Identifier
        (The_String   => "A",
         The_Position => (1, The_Records (1).The_Column),
         The_Test     => "Test 1");

      Next_Symbol;

      Assert_Identifier
        (The_String   => "A",
         The_Position => (2, The_Records (2).The_Column),
         The_Test     => "Test 2");

      Next_Symbol;

      Assert_Integer
        (The_String   => "10",
         The_Position => (3, The_Records (3).The_Column),
         The_Test     => "Test 3");

      for The_Index in 4 .. The_Records'Last loop
         Next_Symbol;

         Assert_Symbol
           (The_Symbol   => The_Records (The_Index).The_Symbol,
            The_Position => (Line_Number (The_Index), The_Records (The_Index).The_Column),
            The_Test     => "Test symbols");

      end loop;

      Next_Symbol;

      Assert_Identifier
        (The_String   => "AA",
         The_Position => (50, 3),
         The_Test     => "Test 1 positions");

      Next_Symbol;

      Assert_Symbol
        (The_Symbol   => Comma_Symbol,
         The_Position => (50, 4),
         The_Test     => "Test 2 position");

      Next_Symbol;

      Assert_Identifier
        (The_String   => "AA",
         The_Position => (50, 6),
         The_Test     => "Test 3 positions");

      Next_Symbol;

      Assert_Identifier
        (The_String   => "AA",
         The_Position => (50, 9),
         The_Test     => "Test 4 positions");

      Next_Symbol;

      Assert_Identifier
        (The_String   => "AA",
         The_Position => (51, 3),
         The_Test     => "Test 5 positions");

      Close;

   end Test_Scanner;

end Scanner_Package.Scanner_Test;
