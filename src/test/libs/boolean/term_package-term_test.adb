-- BC Boolean Compiler
-- Copyright (c) 2015 Paul Estep

with Ada.Text_IO;
--
with AUnit.Assertions; use AUnit.Assertions;
--
with Test_Package; use Test_Package;
--

package body Term_Package.Term_Test is

   -- create test fpt using definitions

   procedure Create_Test_FPT
     (The_Fpt   : out Term_Type;
      The_Array :     Boolean_Array_Type)
   is
   begin
      The_Fpt :=
        (The_Length => The_Array'Length, The_Array => (others => False));
      The_Fpt.The_Array (0 .. The_Array'Length - 1) :=
        The_Array (0 .. The_Array'Length - 1);
   end Create_Test_FPT;

   -- return string of fpt using definitions and not package routines.

   function Image_Of_Test_Fpt (The_Fpt : Term_Type) return String is
   begin
      return Image_Of
          (Boolean_Array_Type
             (The_Fpt.The_Array (0 .. The_Fpt.The_Length - 1)));
   end Image_Of_Test_Fpt;

   ----------
   -- Name --
   ----------

   overriding function Name (The_Test : in Test) return AUnit.Test_String is
   begin
      return AUnit.Format ("Term.");
   end Name;

   --------------------
   -- Register_Tests --
   --------------------

   overriding procedure Register_Tests (The_Test : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      -- Repeat for each test routine:
      Register_Routine (The_Test, Test_To_Index'Access, "To_Index.");
      Register_Routine (The_Test, Test_Create'Access, "Create.");
      Register_Routine
        (The_Test,
         Test_Create_With_Value'Access,
         "Create_With_Value.");
      Register_Routine (The_Test, Test_Set'Access, "Set.");
      Register_Routine (The_Test, Test_Is_Set'Access, "Is_Set.");
      Register_Routine (The_Test, Test_Is_Equal'Access, "Is_Equal.");
      Register_Routine (The_Test, Test_Index_Of'Access, "Index_Of.");
      Register_Routine (The_Test, Test_Length_Of'Access, "Length_Of.");
      Register_Routine (The_Test, Test_Image_Of'Access, "Image_Of.");
      Register_Routine
        (The_Test,
         Test_Image_Of_With_Variables'Access,
         "Image_Of_With_Variables.");
      Register_Routine
        (The_Test,
         Test_To_Boolean_Array'Access,
         "To_Boolean_Array.");
      Register_Routine (The_Test, Test_Boolean_Of'Access, "Boolean_Of.");
   end Register_Tests;

   -------------------
   -- Test_To_Index --
   -------------------

   procedure Test_To_Index
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      The_Fpt : Term_Type;
   begin
      -- test null term
      Assert
        (The_Result   => To_Index (NULL_TERM),
         The_Expected => 0,
         The_Test     => "(1) to index.");

      -- test single not variable
      Create_Test_FPT (The_Fpt, (0 => True));
      Assert
        (The_Result   => To_Index (The_Fpt),
         The_Expected => 1,
         The_Test     => "(2) to index.");

      -- test single variable
      Create_Test_FPT (The_Fpt, (0 => False));
      Assert
        (The_Result   => To_Index (The_Fpt),
         The_Expected => 0,
         The_Test     => "(3) to index.");

      -- test A0B0 FPT
      Create_Test_FPT (The_Fpt, (False, False));
      Assert
        (The_Result   => To_Index (The_Fpt),
         The_Expected => 0,
         The_Test     => "(4) to index.");

      -- test one variable and one not variable
      Create_Test_FPT (The_Fpt, (True, False));
      Assert
        (The_Result   => To_Index (The_Fpt),
         The_Expected => 1,
         The_Test     => "(5) to index.");

      -- test one not variable and one variable
      Create_Test_FPT (The_Fpt, (False, True));
      Assert
        (The_Result   => To_Index (The_Fpt),
         The_Expected => 2,
         The_Test     => "(6) to index.");

      -- test two variables
      Create_Test_FPT (The_Fpt, (True, True));
      Assert
        (The_Result   => To_Index (The_Fpt),
         The_Expected => 3,
         The_Test     => "(7) to index.");

      -- test arbitrary variables
      Create_Test_FPT (The_Fpt, (True, False, True, False));
      Assert
        (The_Result   => To_Index (The_Fpt),
         The_Expected => 5,
         The_Test     => "(8) to index.");

      -- test maximum variables
      Create_Test_FPT (The_Fpt, (Term_Index_Type'Range => True));
      Assert
        (The_Result   => To_Index (The_Fpt),
         The_Expected => MAX_TERM,
         The_Test     => "(9) to index.");
   end Test_To_Index;

   -----------------
   -- Test_Create --
   -----------------

   procedure Test_Create (The_Test : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (The_Test);

      The_Fpt : Term_Type;
   begin
      -- test null term
      Create (The_Fpt, 0);
      Assert
        (The_Result   => Image_Of_Test_Fpt (The_Fpt),
         The_Expected => "*",
         The_Test     => "(10) create.");

      -- test single variable
      Create (The_Fpt, 1);
      Assert
        (The_Result   => Image_Of_Test_Fpt (The_Fpt),
         The_Expected => "0",
         The_Test     => "(11) create.");

      -- test arbitrary variables
      Create (The_Fpt, 4);
      Assert
        (The_Result   => Image_Of_Test_Fpt (The_Fpt),
         The_Expected => "0000",
         The_Test     => "(12) create.");

      -- test maximum variables
      Create (The_Fpt, TERMS);
      Assert
        (The_Result   => Image_Of_Test_Fpt (The_Fpt),
         The_Expected => String'(1 .. TERMS => '0'),
         The_Test     => "(13) create.");
   end Test_Create;

   ----------------------------
   -- Test_Create_With_Value --
   ----------------------------

   procedure Test_Create_With_Value
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      The_Fpt : Term_Type;
   begin
      -- test null term
      Create (The_Fpt, 0, 0);
      Assert
        (The_Result   => Image_Of_Test_Fpt (The_Fpt),
         The_Expected => "*",
         The_Test     => "(14) create.");

      -- test single not variable
      Create (The_Fpt, 1, 0);
      Assert
        (The_Result   => Image_Of_Test_Fpt (The_Fpt),
         The_Expected => "0",
         The_Test     => "(15) create with value.");

      -- test single variable
      Create (The_Fpt, 1, 1);
      Assert
        (The_Result   => Image_Of_Test_Fpt (The_Fpt),
         The_Expected => "1",
         The_Test     => "(16) create with value.");

      -- test arbitrary variables, A0b0c0D0
      Create (The_Fpt, 4, 9);
      Assert
        (The_Result   => Image_Of_Test_Fpt (The_Fpt),
         The_Expected => "1001",
         The_Test     => "(17) create with value.");

      -- test maximum variables
      Create (The_Fpt, TERMS, 0);
      Assert
        (The_Result   => Image_Of_Test_Fpt (The_Fpt),
         The_Expected => String'(1 .. TERMS => '0'),
         The_Test     => "(18) create with value.");
   end Test_Create_With_Value;

   --------------
   -- Test_Set --
   --------------

   procedure Test_Set (The_Test : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (The_Test);

      The_Fpt : Term_Type;
   begin
      -- test single not variable to variable
      Create_Test_FPT (The_Fpt, (0 => False));
      Set (0, The_Fpt, True);
      Assert
        (The_Result   => Image_Of_Test_Fpt (The_Fpt),
         The_Expected => "1",
         The_Test     => "(19) set.");

      -- test single variable to not variable
      Create_Test_FPT (The_Fpt, (0 => True));
      Set (0, The_Fpt, False);
      Assert
        (The_Result   => Image_Of_Test_Fpt (The_Fpt),
         The_Expected => "0",
         The_Test     => "(20) set.");

      -- test set arbitrary variables to not variables
      Create_Test_FPT (The_Fpt, (True, True, True, True));
      Set (1, The_Fpt, False);
      Set (2, The_Fpt, False);
      Assert
        (The_Result   => Image_Of_Test_Fpt (The_Fpt),
         The_Expected => "1001",
         The_Test     => "(21) set.");

      -- test set arbitrary not variables to variables
      Create_Test_FPT (The_Fpt, (False, False, False, False));
      Set (1, The_Fpt, True);
      Set (3, The_Fpt, True);
      Assert
        (The_Result   => Image_Of_Test_Fpt (The_Fpt),
         The_Expected => "0101",
         The_Test     => "(22) set.");

      -- test set maximum not variables to variables
      Create_Test_FPT (The_Fpt, (Term_Index_Type'Range => False));
      Set (TERMS - 1, The_Fpt, True);
      Assert
        (The_Result   => Image_Of_Test_Fpt (The_Fpt),
         The_Expected => String'(1 .. TERMS - 1 => '0', TERMS => '1'),
         The_Test     => "(23) set.");
   end Test_Set;

   -----------------
   -- Test_Is_Set --
   -----------------

   procedure Test_Is_Set (The_Test : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (The_Test);

      The_Fpt : Term_Type;
   begin
      -- test single not variable
      Create_Test_FPT (The_Fpt, (0 => False));
      Assert
        (The_Result   => Is_Set (0, The_Fpt),
         The_Expected => False,
         The_Test     => "(24) is set.");

      -- test single variable
      Create_Test_FPT (The_Fpt, (0 => True));
      Assert
        (The_Result   => Is_Set (0, The_Fpt),
         The_Expected => True,
         The_Test     => "(25) is set.");

      -- test arbitrary variables
      Create_Test_FPT (The_Fpt, (True, True, True, True));
      Assert
        (The_Result =>
           Boolean'Image (Is_Set (0, The_Fpt)) &
           "," &
           Boolean'Image (Is_Set (1, The_Fpt)) &
           "," &
           Boolean'Image (Is_Set (2, The_Fpt)) &
           "," &
           Boolean'Image (Is_Set (3, The_Fpt)),
         The_Expected => "TRUE,TRUE,TRUE,TRUE",
         The_Test     => "(26) is set.");

      -- test arbitrary not variables
      Create_Test_FPT (The_Fpt, (False, False, False, False));
      Assert
        (The_Result =>
           Boolean'Image (Is_Set (0, The_Fpt)) &
           "," &
           Boolean'Image (Is_Set (1, The_Fpt)) &
           "," &
           Boolean'Image (Is_Set (2, The_Fpt)) &
           "," &
           Boolean'Image (Is_Set (3, The_Fpt)),
         The_Expected => "FALSE,FALSE,FALSE,FALSE",
         The_Test     => "(27) is set.");

      -- test arbitrary variables
      Create_Test_FPT (The_Fpt, (True, False, False, True));
      Assert
        (The_Result =>
           Boolean'Image (Is_Set (0, The_Fpt)) &
           "," &
           Boolean'Image (Is_Set (1, The_Fpt)) &
           "," &
           Boolean'Image (Is_Set (2, The_Fpt)) &
           "," &
           Boolean'Image (Is_Set (3, The_Fpt)),
         The_Expected => "TRUE,FALSE,FALSE,TRUE",
         The_Test     => "(28) is set.");

      -- test last variable
      Create_Test_FPT
        (The_Fpt,
         (Term_Index_Type'First .. Term_Index_Type'Last - 1 => False,
          Term_Index_Type'Last                              => True));
      Assert
        (The_Result   => Boolean'Image (Is_Set (TERMS - 1, The_Fpt)),
         The_Expected => "TRUE",
         The_Test     => "(29) is set.");
   end Test_Is_Set;

   -------------------
   -- Test_Is_Equal --
   -------------------

   procedure Test_Is_Equal
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      The_Left  : Term_Type;
      The_Right : Term_Type;
   begin
      -- test null term
      Assert
        (The_Result   => Boolean'Image (Is_Equal (NULL_TERM, NULL_TERM)),
         The_Expected => "TRUE",
         The_Test     => "(30) is equal.");

      -- test single not variable
      Create_Test_FPT (The_Left, (0 => False));
      Create_Test_FPT (The_Right, (0 => False));
      Assert
        (The_Result   => Boolean'Image (Is_Equal (The_Left, The_Right)),
         The_Expected => "TRUE",
         The_Test     => "(31) is equal.");

      -- test single not variable and single variable
      Create_Test_FPT (The_Left, (0 => False));
      Create_Test_FPT (The_Right, (0 => True));
      Assert
        (The_Result   => Boolean'Image (Is_Equal (The_Left, The_Right)),
         The_Expected => "FALSE",
         The_Test     => "(32) is equal.");

      -- test single variable and single not variable
      Create_Test_FPT (The_Left, (0 => True));
      Create_Test_FPT (The_Right, (0 => False));
      Assert
        (The_Result   => Boolean'Image (Is_Equal (The_Left, The_Right)),
         The_Expected => "FALSE",
         The_Test     => "(33) is equal.");

      -- test single variable
      Create_Test_FPT (The_Left, (0 => True));
      Create_Test_FPT (The_Right, (0 => True));
      Assert
        (The_Result   => Boolean'Image (Is_Equal (The_Left, The_Right)),
         The_Expected => "TRUE",
         The_Test     => "(34) is equal.");

      -- test arbitrary single not variables
      Create_Test_FPT (The_Left, (0 .. 4 => False));
      Create_Test_FPT (The_Right, (0 .. 4 => False));
      Assert
        (The_Result   => Boolean'Image (Is_Equal (The_Left, The_Right)),
         The_Expected => "TRUE",
         The_Test     => "(35) is equal.");

      -- test arbitrary not variables and single variables
      Create_Test_FPT (The_Left, (0 .. 4 => False));
      Create_Test_FPT (The_Right, (0 .. 4 => True));
      Assert
        (The_Result   => Boolean'Image (Is_Equal (The_Left, The_Right)),
         The_Expected => "FALSE",
         The_Test     => "(36) is equal.");

      -- test arbitrary variables and not variables
      Create_Test_FPT (The_Left, (0 .. 4 => True));
      Create_Test_FPT (The_Right, (0 .. 4 => False));
      Assert
        (The_Result   => Boolean'Image (Is_Equal (The_Left, The_Right)),
         The_Expected => "FALSE",
         The_Test     => "(37) is equal.");

      -- test arbitrary variables
      Create_Test_FPT (The_Left, (0 .. 4 => True));
      Create_Test_FPT (The_Right, (0 .. 4 => True));
      Assert
        (The_Result   => Boolean'Image (Is_Equal (The_Left, The_Right)),
         The_Expected => "TRUE",
         The_Test     => "(38) is equal.");

      -- test arbitrary variables and not variables
      Create_Test_FPT (The_Left, (True, False, True, False, True));
      Create_Test_FPT (The_Right, (True, False, True, False, True));
      Assert
        (The_Result   => Boolean'Image (Is_Equal (The_Left, The_Right)),
         The_Expected => "TRUE",
         The_Test     => "(39) is equal.");

      -- test arbitrary variables and not variables
      Create_Test_FPT (The_Left, (True, False, True, False, True));
      Create_Test_FPT (The_Right, (True, False, False, False, True));
      Assert
        (The_Result   => Boolean'Image (Is_Equal (The_Left, The_Right)),
         The_Expected => "FALSE",
         The_Test     => "(40) is equal.");

      -- test maximum not variables
      Create_Test_FPT (The_Left, (Term_Index_Type'Range => False));
      Create_Test_FPT (The_Right, (Term_Index_Type'Range => False));
      Assert
        (The_Result   => Boolean'Image (Is_Equal (The_Left, The_Right)),
         The_Expected => "TRUE",
         The_Test     => "(41) is equal.");

      -- test maximum variables
      Create_Test_FPT (The_Left, (Term_Index_Type'Range => True));
      Create_Test_FPT (The_Right, (Term_Index_Type'Range => True));
      Assert
        (The_Result   => Boolean'Image (Is_Equal (The_Left, The_Right)),
         The_Expected => "TRUE",
         The_Test     => "(42) is equal.");

      -- test maximum variables and not variables
      Create_Test_FPT (The_Left, (Term_Index_Type'Range => False));
      Create_Test_FPT (The_Right, (Term_Index_Type'Range => True));
      Assert
        (The_Result   => Boolean'Image (Is_Equal (The_Left, The_Right)),
         The_Expected => "FALSE",
         The_Test     => "(43) is equal.");
   end Test_Is_Equal;

   --------------------
   -- Test_Index_Of --
   --------------------

   procedure Test_Index_Of
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      The_Result : Index_Type;
   begin
      -- test null mask
      -- result should always be 0
      The_Result := Index_Of (0, EMPTY_MASK);
      Assert
        (The_Result   => The_Result,
         The_Expected => 0,
         The_Test     =>
           "(44) index of. " &
           Image_Of (Index_Type_To_Term_Array_Type (The_Result) (0 .. 0)));

      -- test null mask
      -- result should always be 0
      The_Result := Index_Of (1, EMPTY_MASK);
      Assert
        (The_Result   => The_Result,
         The_Expected => 0,
         The_Test     =>
           "(45) index of. " &
           Image_Of (Index_Type_To_Term_Array_Type (The_Result) (0 .. 0)));

      -- test initial index 0 and remove no variables result should be the same
      -- as initial value
      The_Result := Index_Of (0, (1, (False, others => False)));
      Assert
        (The_Result   => The_Result,
         The_Expected => 0,
         The_Test     =>
           "(46) index of. " &
           Image_Of (Index_Type_To_Term_Array_Type (The_Result) (0 .. 0)));

      -- test initial index 1 and remove no variables result should be the same
      -- as initial value
      The_Result := Index_Of (1, (1, (False, others => False)));
      Assert
        (The_Result   => The_Result,
         The_Expected => 1,
         The_Test     =>
           "(47) index of. " &
           Image_Of (Index_Type_To_Term_Array_Type (The_Result) (0 .. 0)));

      -- test iniital index 7 (111) and remove the 1st and 2nd variables
      The_Result := Index_Of (7, (3, (True, True, False, others => False)));
      Assert
        (The_Result   => The_Result,
         The_Expected => 1,
         The_Test     =>
           "(48) index of. " &
           Image_Of (Index_Type_To_Term_Array_Type (The_Result) (0 .. 0)));

      -- test iniital index 7 (111) and remove the 1st variable
      The_Result := Index_Of (7, (3, (True, False, False, others => False)));
      Assert
        (The_Result   => The_Result,
         The_Expected => 3,
         The_Test     =>
           "(49) index of. " &
           Image_Of (Index_Type_To_Term_Array_Type (The_Result) (0 .. 1)));

      -- test iniital index 7 (111) and remove no variable
      The_Result := Index_Of (7, (3, (False, False, False, others => False)));
      Assert
        (The_Result   => The_Result,
         The_Expected => 7,
         The_Test     =>
           "(50) index of. " &
           Image_Of (Index_Type_To_Term_Array_Type (The_Result) (0 .. 2)));

      -- test iniital index 4 (001) and remove the 1st and 2nd variables
      The_Result := Index_Of (4, (3, (True, True, False, others => False)));
      Assert
        (The_Result   => The_Result,
         The_Expected => 1,
         The_Test     =>
           "(51) index of. " &
           Image_Of (Index_Type_To_Term_Array_Type (The_Result) (0 .. 0)));

      -- test iniital index 4 (001) and remove the 1st variable
      The_Result := Index_Of (4, (3, (True, False, False, others => False)));
      Assert
        (The_Result   => The_Result,
         The_Expected => 2,
         The_Test     =>
           "(52) index of. " &
           Image_Of (Index_Type_To_Term_Array_Type (The_Result) (0 .. 1)));

      -- test iniital index 4 (001) and remove no variable
      The_Result := Index_Of (4, (3, (False, False, False, others => False)));
      Assert
        (The_Result   => The_Result,
         The_Expected => 4,
         The_Test     =>
           "(53) index of. " &
           Image_Of (Index_Type_To_Term_Array_Type (The_Result) (0 .. 2)));
   end Test_Index_Of;

   --------------------
   -- Test_Length_Of --
   --------------------

   procedure Test_Length_Of
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      The_Fpt : Term_Type;
   begin
      -- test null term
      Assert
        (The_Result   => Length_Of (NULL_TERM),
         The_Expected => 0,
         The_Test     => "(54) length of.");

      -- test single not variable
      Create_Test_FPT (The_Fpt, (0 => False));
      Assert
        (The_Result   => Length_Of (The_Fpt),
         The_Expected => 1,
         The_Test     => "(55) length of.");

      -- test arbitrary variables
      Create_Test_FPT (The_Fpt, (False, False, False, True));
      Assert
        (The_Result   => Length_Of (The_Fpt),
         The_Expected => 4,
         The_Test     => "(56) length of.");

      -- test maximum variables
      Create_Test_FPT (The_Fpt, (Term_Index_Type'Range => False));
      Assert
        (The_Result   => Length_Of (The_Fpt),
         The_Expected => TERMS,
         The_Test     => "(57) length of.");
   end Test_Length_Of;

   -------------------
   -- Test_Image_Of --
   -------------------

   procedure Test_Image_Of
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      IMAGE_OF_TERMS : constant String :=
        IMAGE_OF_ALL_TERMS (1 .. 2 * VARIABLES);
--        "a0b0c0d0e0f0g0h0i0j0k0l0m0n0o0p0";

      The_Fpt : Term_Type;
   begin
      -- test null term
      Assert
        (The_Result   => Image_Of (NULL_TERM),
         The_Expected => "",
         The_Test     => "(58) image of.");

      -- test single not variable
      Create_Test_FPT (The_Fpt, (0 => False));
      Assert
        (The_Result   => Image_Of (The_Fpt),
         The_Expected => "A0",
         The_Test     => "(59) image of.");

      -- test single variable
      Create_Test_FPT (The_Fpt, (0 => True));
      Assert
        (The_Result   => Image_Of (The_Fpt),
         The_Expected => "a0",
         The_Test     => "(60) image of.");

      -- test with arbitrary variables
      Create_Test_FPT (The_Fpt, (True, False, False, True));
      Assert
        (The_Result   => Image_Of (The_Fpt),
         The_Expected => "a0B0C0d0",
         The_Test     => "(61) image of.");

      -- test with maximum variables
      Create_Test_FPT (The_Fpt, (Term_Index_Type'Range => True));
      Assert
        (The_Result   => Image_Of (The_Fpt),
         The_Expected => IMAGE_OF_TERMS,
         The_Test     => "(62) image of.");
   end Test_Image_Of;

   ----------------------------------
   -- Test_Image_Of_With_Variables --
   ----------------------------------

   procedure Test_Image_Of_With_Variables
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      The_Fpt : Term_Type;
   begin
      -- test null term with the 1st variable
      Assert
        (The_Result   => Image_Of (NULL_TERM, Variable_Array_Type'(1 => 0)),
         The_Expected => "",
         The_Test     => "(63) image of with variables.");

      -- test single not variable with the 1st variable
      Create_Test_FPT (The_Fpt, (0 => False));
      Assert
        (The_Result   => Image_Of (The_Fpt, Variable_Array_Type'(1 => 0)),
         The_Expected => "A0",
         The_Test     => "(64) image of with variables.");

      -- test single variable with the 1st variable
      Create_Test_FPT (The_Fpt, (0 => True));
      Assert
        (The_Result   => Image_Of (The_Fpt, Variable_Array_Type'(1 => 0)),
         The_Expected => "a0",
         The_Test     => "(65) image of with variables.");

      -- test single not variable with the 2nd variable
      Create_Test_FPT (The_Fpt, (0 => False));
      Assert
        (The_Result   => Image_Of (The_Fpt, Variable_Array_Type'(1 => 1)),
         The_Expected => "B0",
         The_Test     => "(66) image of with variables.");

      -- test single variable with the 2nd variable
      Create_Test_FPT (The_Fpt, (0 => True));
      Assert
        (The_Result   => Image_Of (The_Fpt, Variable_Array_Type'(1 => 1)),
         The_Expected => "b0",
         The_Test     => "(67) image of with variables.");

      -- test arbitrary variables with an arbitrary list of variables
      Create_Test_FPT (The_Fpt, (True, False, False, True));
      Assert
        (The_Result   => Image_Of (The_Fpt, Variable_Array_Type'(1, 2, 4, 5)),
         The_Expected => "b0C0E0f0",
         The_Test     => "(68) image of with variables.");
   end Test_Image_Of_With_Variables;

   ---------------------------
   -- Test_To_Boolean_Array --
   ---------------------------

   procedure Test_To_Boolean_Array
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      The_Fpt : Term_Type;
   begin
      -- test null term
      Assert
        (The_Result   => Image_Of (To_Boolean_Array (NULL_TERM)),
         The_Expected => "*",
         The_Test     => "(69) to boolean array.");

      -- test not variable
      Create_Test_FPT (The_Fpt, (0 => False));
      Assert
        (The_Result   => Image_Of (To_Boolean_Array (The_Fpt)),
         The_Expected => "0",
         The_Test     => "(70) to boolean array.");

      -- test variable
      Create_Test_FPT (The_Fpt, (0 => True));
      Assert
        (The_Result   => Image_Of (To_Boolean_Array (The_Fpt)),
         The_Expected => "1",
         The_Test     => "(71) to boolean array.");

      -- test arbitrary variables
      Create_Test_FPT (The_Fpt, (True, False, False, True));
      Assert
        (The_Result   => Image_Of (To_Boolean_Array (The_Fpt)),
         The_Expected => "1001",
         The_Test     => "(72) to boolean array.");

      -- test maximum variables
      Create_Test_FPT (The_Fpt, (Term_Index_Type'Range => True));
      Assert
        (The_Result   => Image_Of (To_Boolean_Array (The_Fpt)),
         The_Expected => (1 .. TERMS => '1'),
         The_Test     => "(73) to boolean array.");
   end Test_To_Boolean_Array;

   ---------------------
   -- Test_Boolean_Of --
   ---------------------

   procedure Test_Boolean_Of
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      The_Fpt : Term_Type;
   begin
      -- test null term
      Assert
        (The_Result   => Boolean_Of (NULL_TERM),
         The_Expected => "",
         The_Test     => "(74) boolean of.");

      -- test single not variable
      Create_Test_FPT (The_Fpt, (0 => False));
      Assert
        (The_Result   => Boolean_Of (The_Fpt),
         The_Expected => "0",
         The_Test     => "(75) boolean of.");

      -- test single variable
      Create_Test_FPT (The_Fpt, (0 => True));
      Assert
        (The_Result   => Boolean_Of (The_Fpt),
         The_Expected => "1",
         The_Test     => "(76) boolean of.");

      -- test arbitrary variables
      Create_Test_FPT (The_Fpt, (True, False, False, True));
      Assert
        (The_Result   => Boolean_Of (The_Fpt),
         The_Expected => "1001",
         The_Test     => "(77) boolean of.");

      -- test maximum variables
      Create_Test_FPT (The_Fpt, (Term_Index_Type'Range => True));
      Assert
        (The_Result   => Boolean_Of (The_Fpt),
         The_Expected => (1 .. TERMS => '1'),
         The_Test     => "(78) boolean of.");
   end Test_Boolean_Of;

end Term_Package.Term_Test;
