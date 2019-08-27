-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with AUnit.Assertions; use AUnit.Assertions;
--
with Ada.Characters.Latin_1;
--
--  with Ada.Strings;       use Ada.Strings;
--  with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with AUnit.Assertions;  use AUnit.Assertions;
--
with Test_Package;
--
with Ada.Text_IO;
--
with System_Package; use System_Package;
--
with Test_Package; use Test_Package;
--

package body Source_Package.Source_Test is

   FILENAME : constant String := Test_Package.FILES & "/" & "source.txt";

   --   !"#$%&'()*+,-./
   --  0123456789
   --  :;<=>?@
   --  ABCDEFGHIJKLMNOPQRSTUVWXYZ
   --  [\]^_`
   --  abcdefghijklmnopqrstuvwxyz
   --  {|}~

   ----------
   -- Name --
   ----------

   function Name (The_Test : in Test) return AUnit.Test_String is
   begin
      return AUnit.Format ("source_package.source_test!");
   end Name;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (The_Test : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine(The_Test, Test_Source'Access, "test_source!");
   end Register_Tests;

   -----------------
   -- Set_Up_Case --
   -----------------

   overriding procedure Set_Up_Case (The_Test : in out Test) is
   begin
      null;
   end Set_Up_Case;

   --------------------
   -- Tear_Down_Case --
   --------------------

   overriding procedure Tear_Down_Case (The_Test : in out Test) is
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

   -------------------
   -- Assert_Source --
   -------------------

   procedure Assert_Source
     (The_Character : in Character;
      The_Position  : in Source_Position) is
   begin
      AUnit.Assertions.Assert
        (Source_Package.The_Character = The_Character,
         "Character. " &
           Character'Image (Source_Package.The_Character) &
           " -- " &
           Character'Image (The_Character));

      AUnit.Assertions.Assert
        (Source_Package.The_Position = The_Position,
         "Position. " &
           Image_Of (Source_Package.The_Position) &
           " -- " &
           Image_Of (The_Position));
   end Assert_Source;

   -----------------
   -- Test_Source --
   -----------------

   procedure Test_Source
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (The_Test);

   begin
      Open (FILENAME);

      -- ISO 646 graphic characters

      Assert_Source (' ', (1, 1));
      Next_Character;

      Assert_Source ('!', (1, 2));
      Next_Character;

      Assert_Source ('"', (1, 3));
      Next_Character;

      Assert_Source ('#', (1, 4));
      Next_Character;

      Assert_Source ('$', (1, 5));
      Next_Character;

      Assert_Source ('%', (1, 6));
      Next_Character;

      Assert_Source ('&', (1, 7));
      Next_Character;

      Assert_Source (''', (1, 8));
      Next_Character;

      Assert_Source ('(', (1, 9));
      Next_Character;

      Assert_Source (')', (1, 10));
      Next_Character;

      Assert_Source ('*', (1, 11));
      Next_Character;

      Assert_Source ('+', (1, 12));
      Next_Character;

      Assert_Source (',', (1, 13));
      Next_Character;

      Assert_Source ('-', (1, 14));
      Next_Character;

      Assert_Source ('.', (1, 15));
      Next_Character;

      Assert_Source ('/', (1, 16));
      Next_Character;

      Assert_Source (Ada.Characters.Latin_1.LF, (1, 17));
      Next_Character;

      -- Decimal digits '0' though '9' are at positions 48 through 57

      Assert_Source ('0', (2, 1));
      Next_Character;

      Assert_Source ('1', (2, 2));
      Next_Character;

      Assert_Source ('2', (2, 3));
      Next_Character;

      Assert_Source ('3', (2, 4));
      Next_Character;

      Assert_Source ('4', (2, 5));
      Next_Character;

      Assert_Source ('5', (2, 6));
      Next_Character;

      Assert_Source ('6', (2, 7));
      Next_Character;

      Assert_Source ('7', (2, 8));
      Next_Character;

      Assert_Source ('8', (2, 9));
      Next_Character;

      Assert_Source ('9', (2, 10));
      Next_Character;

      Assert_Source (Ada.Characters.Latin_1.LF, (2, 11));
      Next_Character;

      --

      Assert_Source (':', (3, 1));
      Next_Character;

      Assert_Source (';', (3, 2));
      Next_Character;

      Assert_Source ('<', (3, 3));
      Next_Character;

      Assert_Source ('=', (3, 4));
      Next_Character;

      Assert_Source ('>', (3, 5));
      Next_Character;

      Assert_Source ('?', (3, 6));
      Next_Character;

      Assert_Source ('@', (3, 7));
      Next_Character;

      Assert_Source (Ada.Characters.Latin_1.LF, (3, 8));
      Next_Character;

      -- Letters 'A' through 'Z' are at positions 65 through 90

      Assert_Source ('A', (4, 1));
      Next_Character;

      Assert_Source ('B', (4, 2));
      Next_Character;

      Assert_Source ('C', (4, 3));
      Next_Character;

      Assert_Source ('D', (4, 4));
      Next_Character;

      Assert_Source ('E', (4, 5));
      Next_Character;

      Assert_Source ('F', (4, 6));
      Next_Character;

      Assert_Source ('G', (4, 7));
      Next_Character;

      Assert_Source ('H', (4, 8));
      Next_Character;

      Assert_Source ('I', (4, 9));
      Next_Character;

      Assert_Source ('J', (4, 10));
      Next_Character;

      Assert_Source ('K', (4, 11));
      Next_Character;

      Assert_Source ('L', (4, 12));
      Next_Character;

      Assert_Source ('M', (4, 13));
      Next_Character;

      Assert_Source ('N', (4, 14));
      Next_Character;

      Assert_Source ('O', (4, 15));
      Next_Character;

      Assert_Source ('P', (4, 16));
      Next_Character;

      Assert_Source ('Q', (4, 17));
      Next_Character;

      Assert_Source ('R', (4, 18));
      Next_Character;

      Assert_Source ('S', (4, 19));
      Next_Character;

      Assert_Source ('T', (4, 20));
      Next_Character;

      Assert_Source ('U', (4, 21));
      Next_Character;

      Assert_Source ('V', (4, 22));
      Next_Character;

      Assert_Source ('W', (4, 23));
      Next_Character;

      Assert_Source ('X', (4, 24));
      Next_Character;

      Assert_Source ('Y', (4, 25));
      Next_Character;

      Assert_Source ('Z', (4, 26));
      Next_Character;

      Assert_Source (Ada.Characters.Latin_1.LF, (4, 27));
      Next_Character;

      --

      Assert_Source ('[', (5, 1));
      Next_Character;

      Assert_Source ('\', (5, 2));
      Next_Character;

      Assert_Source (']', (5, 3));
      Next_Character;

      Assert_Source ('^', (5, 4));
      Next_Character;

      Assert_Source ('_', (5, 5));
      Next_Character;

      Assert_Source ('`', (5, 6));
      Next_Character;

      Assert_Source (Ada.Characters.Latin_1.LF, (5, 7));
      Next_Character;

      --

      Assert_Source ('A', (6, 1));
      Next_Character;

      Assert_Source ('B', (6, 2));
      Next_Character;

      Assert_Source ('C', (6, 3));
      Next_Character;

      Assert_Source ('D', (6, 4));
      Next_Character;

      Assert_Source ('E', (6, 5));
      Next_Character;

      Assert_Source ('F', (6, 6));
      Next_Character;

      Assert_Source ('G', (6, 7));
      Next_Character;

      Assert_Source ('H', (6, 8));
      Next_Character;

      Assert_Source ('I', (6, 9));
      Next_Character;

      Assert_Source ('J', (6, 10));
      Next_Character;

      Assert_Source ('K', (6, 11));
      Next_Character;

      Assert_Source ('L', (6, 12));
      Next_Character;

      Assert_Source ('M', (6, 13));
      Next_Character;

      Assert_Source ('N', (6, 14));
      Next_Character;

      Assert_Source ('O', (6, 15));
      Next_Character;

      Assert_Source ('P', (6, 16));
      Next_Character;

      Assert_Source ('Q', (6, 17));
      Next_Character;

      Assert_Source ('R', (6, 18));
      Next_Character;

      Assert_Source ('S', (6, 19));
      Next_Character;

      Assert_Source ('T', (6, 20));
      Next_Character;

      Assert_Source ('U', (6, 21));
      Next_Character;

      Assert_Source ('V', (6, 22));
      Next_Character;

      Assert_Source ('W', (6, 23));
      Next_Character;

      Assert_Source ('X', (6, 24));
      Next_Character;

      Assert_Source ('Y', (6, 25));
      Next_Character;

      Assert_Source ('Z', (6, 26));
      Next_Character;

      Assert_Source (Ada.Characters.Latin_1.LF, (6, 27));
      Next_Character;

      --

      Assert_Source ('{', (7, 1));
      Next_Character;

      Assert_Source ('|', (7, 2));
      Next_Character;

      Assert_Source ('}', (7, 3));
      Next_Character;

      Assert_Source ('~', (7, 4));

      Close;
   end Test_Source;

end Source_Package.Source_Test;
