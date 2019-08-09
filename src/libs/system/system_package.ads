-- BCS Boolean Compiler System
-- Copyright (c) 2018 Paul Estep

with Ada.Unchecked_Conversion;
with Ada.Text_IO;
--

-- A package to define System constants.

--  VARIABLES
--    see Variables_Package.ads
--
--  WORDS
--    see Word_Package.ads
--
--  PARAMETERS
--    see Prameters_Package.ads
--
--  sysinteger (si)
--
--  sysmodular (sm)
--
--  bcsinteger (bi)
--  	range: - (2 ** VARIABLES) ..  (2 ** VARIABLES) -1
--  	size: VARIABLES + 1
--
--  bcsmodular (bm)
--  	range (bm): 0 ..  2 ** VARIABLES
--  	size: VARIABLES + 1
--
--  boolean array
--  	count (bm): 0 .. 2 ** VARIABLES
--  	index (bm): 0 .. (2 ** VARIABLES) -1
--  	array length (bm): 0 .. (2 ** VARIABLES) -1 (index)
--
--  variable (bm): 0 .. VARIABLES - 1
--          count (bm): 0 .. VARIABLES
--
--  term
--  	term index (bm): 0 .. (2 ** VARIABLES) -1
--  	----------
--  	count (bm): 0 .. VARIABLES
--  	index (bm): 1 .. VARIABLES
--  	array length (bm): 0 .. VARIABLES -1 (index)
--
--  number
--  	count (bm): 0 .. 2 ** VARIABLES
--  	index (bm): 0 .. (2 ** VARIABLES) -1
--  	array length (bm): 0 .. (2 ** VARIABLES) -1 (index)
--
--  mask
--  	count (bm): 0 .. VARIABLES
--  	index (bm): 1 .. VARIABLES
--  	array length (bm): 1 .. VARIABLES (index)
--
--  variable set
--  	count (bm): 0 .. VARIABLES
--  	index (bm): 0 .. VARIABLES -1
--  	array length (bm): 0 .. VARIABLES -1 (index)
--
--  variable vector
--  	count (bm): 0 .. VARIABLES
--  	index (bm): 1 .. VARIABLES
--  	array length (bm): 1 .. VARIABLES (index)
--
--  equation
--
--  word
--  	count (bm):  0 .. WORDS
--  	index (bm):  0 .. WORDS -1
--  	array length (bm): 0 .. WORDS -1
--
--  parameter
--  	size (bi): 0 .. VARIABLES + 1
--  	first (bi): - (2 ** VARIABLES)
--  	last (bi): (2 ** VARIABLES) -1
--
--  parameter scalar
--
--  parameter array
--  	length (bm):  0 .. 2 ** VARIABLES
--
--  parameter vector
--  	count (bm):  0 .. PARAMETERS
--  	index (bm):  0 .. PARAMETERS -1
--  	array length (bm):  0 .. PARAMETERS -1

package System_Package with
Spark_Mode => Off is

   -- BC Ada System Integer definitions

   SYSINTEGER_SIZE : constant := Integer'Size;

   -- BC Ada System Modular definition

   MIN_SYSINTEGER : constant := -(2 ** (SYSINTEGER_SIZE - 1));
   MAX_SYSINTEGER : constant := 2 ** (SYSINTEGER_SIZE - 1) - 1;

   type SYSModular is mod 2 ** SYSINTEGER_SIZE;

   subtype SYSInteger is Integer range MIN_SYSINTEGER .. MAX_SYSINTEGER;
   subtype SYSNatural is SYSInteger range 0 .. MAX_SYSINTEGER;
   subtype SYSPositive is SYSInteger range 1 .. MAX_SYSINTEGER;

   -- BC Compiler definitions

   BCINTEGER_SIZE : constant := 16;
   MIN_BCINTEGER  : constant := -(2 ** (BCINTEGER_SIZE - 1));
   MAX_BCINTEGER  : constant := 2 ** (BCINTEGER_SIZE - 1) - 1;

   BCMODULAR_SIZE : constant := BCINTEGER_SIZE;

   MAX_BCMODULAR : constant := 2 ** BCMODULAR_SIZE - 1;

   -- BC Compiler Modular definition

   subtype BCModular is SYSModular range 0 .. MAX_BCMODULAR;

   -- BC Compiler Integer definitions

   subtype BCInteger is SYSInteger range MIN_BCINTEGER .. MAX_BCINTEGER;
   subtype BCNatural is BCInteger range 0 .. MAX_BCINTEGER;
   subtype BCPositive is BCInteger range 1 .. MAX_BCINTEGER;

   -- BC Compiler Integer/Modular size definitions

   subtype BCModular_Size_Type is SYSNatural range 1 .. BCMODULAR_SIZE;
   subtype BCInteger_Size_Type is SYSNatural range 1 .. BCINTEGER_SIZE;

   -- Conversions

   function SYSModular_To_SYSInteger is new Ada.Unchecked_Conversion
     (SYSModular,
      SYSInteger);

   function SYSInteger_To_SYSModular is new Ada.Unchecked_Conversion
     (SYSInteger,
      SYSModular);

   function BCModular_To_BCInteger is new Ada.Unchecked_Conversion
     (BCModular,
      BCInteger);

   function BCInteger_To_BCModular is new Ada.Unchecked_Conversion
     (BCInteger,
      BCModular);

   -- Modular IO

   package SYSModular_IO is new Ada.Text_IO.Modular_IO (SYSModular);

   -- BC file extensions.

   SOURCE_EXTENSION : constant String := ".bc";
   PCODE_EXTENSION  : constant String := ".pbc";
   BCODE_EXTENSION  : constant String := ".bbc";
   PBTEST_EXTENSION : constant String := ".pbt";
   ADA_EXTENSION            : constant String := ".abc";
   SPECIFICATION_EXTENSION  : constant String := ".ads";
   IMPLEMENTATION_EXTENSION : constant String := ".adb";

end System_Package;
