-- BCS Boolean Compiler System
-- Copyright (c) 2019 Paul Estep

with Ada.Unchecked_Deallocation;
--
with AUnit.Assertions; use AUnit.Assertions;
--
with Test_Package; use Test_Package;
with System_Package;  use System_Package;
--

package body Pool_Package.Pool_Test is

   The_Pool : Storage_Pool;

   type Integer_Pointer is access Integer;
   for Integer_Pointer'Storage_Pool use The_Pool;

   ----------
   -- Name --
   ----------

   function Name (The_Test : in Test) return AUnit.Test_String is
   begin
      return AUnit.Format ("pool_package.pool_test!");
   end Name;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (The_Test : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      -- Repeat for each test routine:
      Register_Routine (The_Test, Test_Allocations'Access, "test_allocations!");
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
      null;
   end Tear_Down;

   ----------------------
   -- Test_Allocations --
   ----------------------

   procedure Test_Allocations  (The_Test : in out Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      A_Pointer : Integer_Pointer;

      procedure Deallocate is new Ada.Unchecked_Deallocation
        (Integer, Integer_Pointer);

   begin
      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (The_Pool) = 0,
         "Test zero allocations.");

      A_Pointer := new Integer'(10);

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (The_Pool) = 1,
         "Test one allocation.");

      Deallocate (A_Pointer);

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (The_Pool) = 0,
         "Test deallocation.");

      A_Pointer := new Integer'(10);

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (The_Pool) = 1,
         "Test second allocations.");

      Pool_Package.Mark_Allocations(The_Pool);

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (The_Pool) = 0,
         "Test marked allocations.");

      A_Pointer := new Integer'(10);

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (The_Pool) = 1,
         "Test second allocations.");

      Pool_Package.Mark_Allocations(The_Pool);

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (The_Pool) = 0,
         "Test marked allocations.");

   end Test_Allocations;

end Pool_Package.Pool_Test;
