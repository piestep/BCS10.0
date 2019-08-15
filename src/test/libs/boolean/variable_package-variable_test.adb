-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with AUnit.Assertions; use AUnit.Assertions;
--
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
--
with Test_Package; use Test_Package;
--

package body Variable_Package.Variable_Test is

   -- Return string of all varaibels.

   function All_Variables return String is
      The_String : Unbounded_String := Null_Unbounded_String;
   begin
      for I in Variable_Type'Range loop
         The_String := The_String & Variable_Type'Image (I);
      end loop;
      return To_String (The_String);
   end All_Variables;

   ALL_VARIABLES_IMAGE : constant String := All_Variables;

   ----------
   -- Name --
   ----------

   function Name (The_Test : in Test) return AUnit.Test_String is
   begin
      return AUnit.Format ("Variable_Package.Variable_Test.");
   end Name;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (The_Test : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      -- Repeat for each test routine:
      Register_Routine(The_Test, Test_Is_Included'Access, "Test_Is_Included.");
      Register_Routine(The_Test, Test_Include'Access, "Test_Include.");
      Register_Routine(The_Test, Test_Exclude'Access, "Test_Exclude.");
      Register_Routine(The_Test, Test_Array_Of'Access, "Test_Array_Of.");
      Register_Routine(The_Test, Test_Not'Access, "Test_Not.");
      Register_Routine(The_Test, Test_And'Access, "Test_And.");
      Register_Routine(The_Test, Test_Or'Access, "Test_Or.");
      Register_Routine(The_Test, Test_Xor'Access, "Test_Xor.");
      Register_Routine(The_Test, Test_Image_Of_Variable'Access, "Test_Image_Of_Variable.");
      Register_Routine(The_Test, Test_Not_Image_Of'Access, "Test_Not_Image_Of.");
      Register_Routine(The_Test, Test_Boolean_Of'Access, "Test_Boolean_Of.");
      Register_Routine(The_Test, Test_Image_Of_Set_Of_Variables'Access, "Test_Image_Of_Set_Of_Variables.");
   end Register_Tests;

   --------------
   -- Image_Of --
   --------------

   function Image_Of (The_Array : Variable_Array_Type) return String is
      The_Unbounded : Unbounded_String := Null_Unbounded_String;
   begin
      if The_Array'Length = 0 then
         return "*";
      else
         for I in The_Array'Range loop
            The_Unbounded :=
              The_Unbounded & Variable_Type'Image (The_Array (I));
         end loop;
         return To_String (The_Unbounded);
      end if;
   end Image_Of;

   ----------------------
   -- Test_Is_Included --
   ----------------------

   procedure Test_Is_Included
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      The_Variables : constant Variable_Set_Type :=
        (0                                        => True,
         1 .. MAX_VARIABLE / 2 - 1                => False,
         MAX_VARIABLE / 2                         => True,
         MAX_VARIABLE / 2 + 1 .. MAX_VARIABLE - 1 => False,
         MAX_VARIABLE                             => True);

   begin
      -- test 1st member
      Assert
        (The_Result   => Is_Included (0, The_Variables),
         The_Expected => True,
         The_Test     => "(1) is included.");

      -- test 2nd member
      Assert
        (The_Result   => Is_Included (1, The_Variables),
         The_Expected => False,
         The_Test     => "(2) is included.");

      -- test arbitrary member
      Assert
        (The_Result   => Is_Included (MAX_VARIABLE / 2 - 1, The_Variables),
         The_Expected => False,
         The_Test     => "(3) is included.");

      -- test arbitrary member
      Assert
        (The_Result   => Is_Included (MAX_VARIABLE / 2, The_Variables),
         The_Expected => True,
         The_Test     => "(4) is included.");

      -- test arbitrary member
      Assert
        (The_Result   => Is_Included (MAX_VARIABLE / 2 + 1, The_Variables),
         The_Expected => False,
         The_Test     => "(5) is included.");

      -- test 2nd to last member
      Assert
        (The_Result   => Is_Included (MAX_VARIABLE - 1, The_Variables),
         The_Expected => False,
         The_Test     => "(6) is included.");

      -- test last member
      Assert
        (The_Result   => Is_Included (MAX_VARIABLE, The_Variables),
         The_Expected => True,
         The_Test     => "(7) is included.");
   end Test_Is_Included;

   ------------------
   -- Test_Include --
   ------------------

   procedure Test_Include
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      The_Variables : Variable_Set_Type;
   begin
      -- test all members
      The_Variables := Variable_Set_Type'(others => False);

      for I in Variable_Set_Type'Range loop
         Include (I, The_Variables);
      end loop;

      Assert
        (The_Result   => Image_Of (Boolean_Array_Type (The_Variables)),
         The_Expected => (1 .. VARIABLES => '1'),
         The_Test     => "(8) include.");

      -- test first and last members only
      The_Variables := Variable_Set_Type'(others => False);

      Include (0, The_Variables);
      Include (MAX_VARIABLE, The_Variables);

      Assert
        (The_Result   => Image_Of (Boolean_Array_Type (The_Variables)),
         The_Expected =>
           (1 => '1', 2 .. VARIABLES - 1 => '0', VARIABLES => '1'),
         The_Test => "(9) include.");
   end Test_Include;

   ------------------
   -- Test_Exclude --
   ------------------

   procedure Test_Exclude
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      The_Variables : Variable_Set_Type;
   begin
      -- test all members
      The_Variables := Variable_Set_Type'(others => True);

      for I in Variable_Set_Type'Range loop
         Exclude (I, The_Variables);
      end loop;

      Assert
        (The_Result   => Image_Of (Boolean_Array_Type (The_Variables)),
         The_Expected => (1 .. VARIABLES => '0'),
         The_Test     => "(10) exclude.");

      -- test first and last members only
      The_Variables := Variable_Set_Type'(others => True);

      Exclude (0, The_Variables);
      Exclude (MAX_VARIABLE, The_Variables);

      Assert
        (The_Result   => Image_Of (Boolean_Array_Type (The_Variables)),
         The_Expected =>
           (1 => '0', 2 .. VARIABLES - 1 => '1', VARIABLES => '0'),
         The_Test => "(11) exclude.");
   end Test_Exclude;

   -------------------
   -- Test_Array_Of --
   -------------------

   procedure Test_Array_Of
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      The_Variables : Variable_Set_Type;
   begin
      -- test empty set
      The_Variables := Variable_Set_Type'(Variable_Set_Type'Range => False);
      Assert
        (The_Result   => Image_Of (Array_Of (The_Variables)),
         The_Expected => Image_Of (Variable_Array_Type'(2 .. 1 => 0)),
         The_Test     => "(12) array of.");

      -- test set of first member only
      The_Variables := Variable_Set_Type'(0 => True, others => False);
      Assert
        (The_Result   => Image_Of (Array_Of (The_Variables)),
         The_Expected => Image_Of (Variable_Array_Type'(1 => 0)),
         The_Test     => "(13) array of.");

      -- test arbitrary set of members
      The_Variables :=
        Variable_Set_Type'(True, False, True, True, others => False);
      Assert
        (The_Result   => Image_Of (Array_Of (The_Variables)),
         The_Expected => Image_Of (Variable_Array_Type'(0, 2, 3)),
         The_Test     => "(14) array of.");

      -- test set of all members
      The_Variables := Variable_Set_Type'(Variable_Set_Type'Range => True);
      Assert
        (The_Result   => Image_Of (Array_Of (The_Variables)),
         The_Expected => ALL_VARIABLES_IMAGE,
         The_Test     => "(15) array of.");

      -- test set of last member only
      The_Variables :=
        Variable_Set_Type'
          (0 .. Variable_Set_Type'Last - 1 => False,
           Variable_Set_Type'Last          => True);
      Assert
        (The_Result   => Image_Of (Array_Of (The_Variables)),
         The_Expected => Integer'Image (MAX_VARIABLE),
         The_Test     => "(16) array of.");
   end Test_Array_Of;

   --------------
   -- Test_Not --
   --------------

   procedure Test_Not (The_Test : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (The_Test);

      The_Right  : Variable_Set_Type;
      The_Result : Variable_Set_Type;
   begin
      -- test set all false values
      The_Right  := (Variable_Set_Type'Range => False);
      The_Result := not The_Right;
      Assert
        (The_Result   => Image_Of (Boolean_Array_Type (The_Result)),
         The_Expected => (1 .. VARIABLES => '1'),
         The_Test     => "(17) not.");

      -- test set all true values
      The_Right  := (Variable_Set_Type'Range => True);
      The_Result := not The_Right;
      Assert
        (The_Result   => Image_Of (Boolean_Array_Type (The_Result)),
         The_Expected => (1 .. VARIABLES => '0'),
         The_Test     => "(18) not.");

      -- test set first, last true and others false
      The_Right :=
        (Variable_Set_Type'First                                   => True,
         Variable_Set_Type'First + 1 .. Variable_Set_Type'Last - 1 => False,
         Variable_Set_Type'Last                                    => True);
      The_Result := not The_Right;
      Assert
        (The_Result   => Image_Of (Boolean_Array_Type (The_Result)),
         The_Expected =>
           (1 => '0', 2 .. VARIABLES - 1 => '1', VARIABLES => '0'),
         The_Test => "(19) not.");
   end Test_Not;

   --------------
   -- Test_And --
   --------------

   procedure Test_And (The_Test : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (The_Test);

      The_Left   : Variable_Set_Type;
      The_Right  : Variable_Set_Type;
      The_Result : Variable_Set_Type;
   begin
      -- test set all false values and all false values
      The_Left   := (Variable_Set_Type'Range => False);
      The_Right  := (Variable_Set_Type'Range => False);
      The_Result := The_Left and The_Right;
      Assert
        (The_Result   => Image_Of (Boolean_Array_Type (The_Result)),
         The_Expected => (1 .. VARIABLES => '0'),
         The_Test     => "(20) and.");

      -- test set all false values and all true values
      The_Left   := (Variable_Set_Type'Range => False);
      The_Right  := (Variable_Set_Type'Range => True);
      The_Result := The_Left and The_Right;
      Assert
        (The_Result   => Image_Of (Boolean_Array_Type (The_Result)),
         The_Expected => (1 .. VARIABLES => '0'),
         The_Test     => "(21) and.");

      -- test set all true values and all false values
      The_Left   := (Variable_Set_Type'Range => True);
      The_Right  := (Variable_Set_Type'Range => False);
      The_Result := The_Left and The_Right;
      Assert
        (The_Result   => Image_Of (Boolean_Array_Type (The_Result)),
         The_Expected => (1 .. VARIABLES => '0'),
         The_Test     => "(22) and.");

      -- test set all true values and all true values
      The_Left   := (Variable_Set_Type'Range => True);
      The_Right  := (Variable_Set_Type'Range => True);
      The_Result := The_Left and The_Right;
      Assert
        (The_Result   => Image_Of (Boolean_Array_Type (The_Result)),
         The_Expected => (1 .. VARIABLES => '1'),
         The_Test     => "(23) and.");

      -- test set first, last true and others false
      The_Left  := (Variable_Set_Type'Range => True);
      The_Right :=
        (Variable_Set_Type'First                                   => True,
         Variable_Set_Type'First + 1 .. Variable_Set_Type'Last - 1 => False,
         Variable_Set_Type'Last                                    => True);
      The_Result := The_Left and The_Right;
      Assert
        (The_Result   => Image_Of (Boolean_Array_Type (The_Result)),
         The_Expected =>
           (1 => '1', 2 .. VARIABLES - 1 => '0', VARIABLES => '1'),
         The_Test => "(24) and.");
   end Test_And;

   -------------
   -- Test_Or --
   -------------

   procedure Test_Or (The_Test : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (The_Test);

      The_Left   : Variable_Set_Type;
      The_Right  : Variable_Set_Type;
      The_Result : Variable_Set_Type;
   begin
      -- test set all false values and all false values
      The_Left   := (Variable_Set_Type'Range => False);
      The_Right  := (Variable_Set_Type'Range => False);
      The_Result := The_Left or The_Right;
      Assert
        (The_Result   => Image_Of (Boolean_Array_Type (The_Result)),
         The_Expected => (1 .. VARIABLES => '0'),
         The_Test     => "(25) or.");

      -- test set all false values and all true values
      The_Left   := (Variable_Set_Type'Range => False);
      The_Right  := (Variable_Set_Type'Range => True);
      The_Result := The_Left or The_Right;
      Assert
        (The_Result   => Image_Of (Boolean_Array_Type (The_Result)),
         The_Expected => (1 .. VARIABLES => '1'),
         The_Test     => "(26) or.");

      -- test set all true values and all false values
      The_Left   := (Variable_Set_Type'Range => True);
      The_Right  := (Variable_Set_Type'Range => False);
      The_Result := The_Left or The_Right;
      Assert
        (The_Result   => Image_Of (Boolean_Array_Type (The_Result)),
         The_Expected => (1 .. VARIABLES => '1'),
         The_Test     => "(27) or.");

      -- test set all true values and all true values
      The_Left   := (Variable_Set_Type'Range => True);
      The_Right  := (Variable_Set_Type'Range => True);
      The_Result := The_Left or The_Right;
      Assert
        (The_Result   => Image_Of (Boolean_Array_Type (The_Result)),
         The_Expected => (1 .. VARIABLES => '1'),
         The_Test     => "(28) or.");

      -- test set first, last true and others false
      The_Left  := (Variable_Set_Type'Range => False);
      The_Right :=
        (Variable_Set_Type'First                                   => True,
         Variable_Set_Type'First + 1 .. Variable_Set_Type'Last - 1 => False,
         Variable_Set_Type'Last                                    => True);
      The_Result := The_Left or The_Right;
      Assert
        (The_Result   => Image_Of (Boolean_Array_Type (The_Result)),
         The_Expected =>
           (1 => '1', 2 .. VARIABLES - 1 => '0', VARIABLES => '1'),
         The_Test => "(29) or.");
   end Test_Or;

   --------------
   -- Test_Xor --
   --------------

   procedure Test_Xor (The_Test : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (The_Test);

      The_Left   : Variable_Set_Type;
      The_Right  : Variable_Set_Type;
      The_Result : Variable_Set_Type;
   begin
      -- test set all false values and all false values
      The_Left   := (Variable_Set_Type'Range => False);
      The_Right  := (Variable_Set_Type'Range => False);
      The_Result := The_Left xor The_Right;
      Assert
        (The_Result   => Image_Of (Boolean_Array_Type (The_Result)),
         The_Expected => (1 .. VARIABLES => '0'),
         The_Test     => "(30) xor.");

      -- test set all false values and all true values
      The_Left   := (Variable_Set_Type'Range => False);
      The_Right  := (Variable_Set_Type'Range => True);
      The_Result := The_Left xor The_Right;
      Assert
        (The_Result   => Image_Of (Boolean_Array_Type (The_Result)),
         The_Expected => (1 .. VARIABLES => '1'),
         The_Test     => "(31) xor.");

      -- test set all true values and all false values
      The_Left   := (Variable_Set_Type'Range => True);
      The_Right  := (Variable_Set_Type'Range => False);
      The_Result := The_Left xor The_Right;
      Assert
        (The_Result   => Image_Of (Boolean_Array_Type (The_Result)),
         The_Expected => (1 .. VARIABLES => '1'),
         The_Test     => "(32) xor.");

      -- test set all true values and all true values
      The_Left   := (Variable_Set_Type'Range => True);
      The_Right  := (Variable_Set_Type'Range => True);
      The_Result := The_Left xor The_Right;
      Assert
        (The_Result   => Image_Of (Boolean_Array_Type (The_Result)),
         The_Expected => (1 .. VARIABLES => '0'),
         The_Test     => "(33) xor.");

      -- test set first, last true and others false
      The_Left  := (Variable_Set_Type'Range => True);
      The_Right :=
        (Variable_Set_Type'First                                   => True,
         Variable_Set_Type'First + 1 .. Variable_Set_Type'Last - 1 => False,
         Variable_Set_Type'Last                                    => True);
      The_Result := The_Left xor The_Right;
      Assert
        (The_Result   => Image_Of (Boolean_Array_Type (The_Result)),
         The_Expected =>
           (1 => '0', 2 .. VARIABLES - 1 => '1', VARIABLES => '0'),
         The_Test => "(34) xor.");
   end Test_Xor;

   ----------------------------
   -- Test_Image_Of_Variable --
   ----------------------------

   procedure Test_Image_Of_Variable
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

   begin
      -- test 1st variable
      Assert
        (The_Result   => Image_Of (0),
         The_Expected => "a0",
         The_Test     => "(35) image of (variable).");

      -- test 2nd variable
      Assert
        (The_Result   => Image_Of (1),
         The_Expected => "b0",
         The_Test     => "(36) image of (variable).");

      -- test 8th variable
      Assert
        (The_Result   => Image_Of (7),
         The_Expected => "h0",
         The_Test     => "(37) image of (variable).");

      -- test maximum variable
      Assert
        (The_Result   => Image_Of (MAX_VARIABLE),
         The_Expected =>
           IMAGE_OF_ALL_VARIABLES
             (3 * MAX_VARIABLE + 2 .. 3 * MAX_VARIABLE + 3),
         The_Test => "(38) image of (variable).");
   end Test_Image_Of_Variable;

   -----------------------
   -- Test_Not_Image_Of --
   -----------------------

   procedure Test_Not_Image_Of
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

   begin
      -- test 1st variable
      Assert
        (The_Result   => Not_Image_Of (0),
         The_Expected => "A0",
         The_Test     => "(39) image of (variable).");

      -- test 2nd variable
      Assert
        (The_Result   => Not_Image_Of (1),
         The_Expected => "B0",
         The_Test     => "(40) image of (variable).");

      -- test 8th variable
      Assert
        (The_Result   => Not_Image_Of (7),
         The_Expected => "H0",
         The_Test     => "(41) image of (variable).");

      -- test maximum variable
      Assert
        (The_Result   => Not_Image_Of (MAX_VARIABLE),
         The_Expected =>
           Translate
             (IMAGE_OF_ALL_VARIABLES
                  (3 * MAX_VARIABLE + 2 .. 3 * MAX_VARIABLE + 3),
              Upper_Case_Map),
         The_Test => "(42) image of (variable).");
   end Test_Not_Image_Of;

   ---------------------
   -- Test_Boolean_Of --
   ---------------------

   procedure Test_Boolean_Of
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      The_Set : Variable_Set_Type;
   begin
      -- test empty
      The_Set := Variable_Set_Type'(Variable_Set_Type'Range => False);
      Assert
        (The_Result   => Boolean_Of (The_Set),
         The_Expected => (1 .. VARIABLES => '0'),
         The_Test     => "(43) boolean of (set of variables).");

      -- test set with 1st member only
      The_Set := Variable_Set_Type'(0 => True, others => False);
      Assert
        (The_Result   => Boolean_Of (The_Set),
         The_Expected => (1 => '1', 2 .. VARIABLES => '0'),
         The_Test     => "(44) boolean of (set of variables).");

      -- test set with arbitrary members
      The_Set := Variable_Set_Type'(True, False, True, True, others => False);
      Assert
        (The_Result   => Boolean_Of (The_Set),
         The_Expected => "1011" & (5 .. VARIABLES => '0'),
         The_Test     => "(45) boolean of (set of variables).");
   end Test_Boolean_Of;

   ------------------------------------
   -- Test_Image_Of_Set_Of_Variables --
   ------------------------------------

   procedure Test_Image_Of_Set_Of_Variables
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      IMAGE_OF_VARIABLES : constant String :=
        IMAGE_OF_ALL_VARIABLES (1 .. 3 * VARIABLES);
      --             " a0 b0 c0 d0 e0 f0 g0 h0 i0 j0 k0 l0 m0 n0 o0 p0";

      The_Set : Variable_Set_Type;
   begin
      -- test empty set
      The_Set := Variable_Set_Type'(Variable_Set_Type'Range => False);
      Assert
        (The_Result   => Image_Of (The_Set),
         The_Expected => "",
         The_Test     => "(46) image of (set of variables).");

      -- test set with 1st member
      The_Set := Variable_Set_Type'(0 => True, others => False);
      Assert
        (The_Result   => Image_Of (The_Set),
         The_Expected => " a0",
         The_Test     => "(47) image of (set of variables).");

      -- test set of arbitrary members
      The_Set := Variable_Set_Type'(True, False, True, True, others => False);
      Assert
        (The_Result   => Image_Of (The_Set),
         The_Expected => " a0 c0 d0",
         The_Test     => "(48) image of (set of variables).");

      -- test set of all members
      The_Set := Variable_Set_Type'(Variable_Type'Range => True);
      Assert
        (The_Result   => Image_Of (The_Set),
         The_Expected => IMAGE_OF_VARIABLES,
         The_Test     => "(49) image of (set of variables).");

   end Test_Image_Of_Set_Of_Variables;

end Variable_Package.Variable_Test;
