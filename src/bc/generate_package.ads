-- BC Boolean Compiler
-- Copyright (c) 2007 Paul Estep

with BC_Package;        use BC_Package;
with Graph_Package;     use Graph_Package;
with PCode_Package;     use PCode_Package;
with BCode_Package;     use BCode_Package;
with Parameter_Package; use Parameter_Package;
--

-- A package to generate bcode, pcode and acode.

package Generate_Package is

   -- Debug pcode generate switch.

   PCode_Debug : Boolean := False;

   -- Dump pcode switch.

   PCode_Dump : Boolean := False;

   -- Generate pcode switch.

   Generate_PCode : Boolean := False;

   -- Debug pcode generate switch.

   BCode_Debug : Boolean := False;

   -- Generate pcode switch.

   Generate_BCode : Boolean := False;

   -- Generate ada test code switch.

   Generate_ACode : Boolean := False;

   -- Generate pcode from program graph.

   procedure PCode
     (The_Unit   :        Compilation_Unit_Graph;
      The_Code   : in out Code;
      The_Length :    out Address);

   -- Generate pcode from program graph.

   procedure BCode
     (The_Unit      :     Compilation_Unit_Graph;
      The_Variables : out Natural;
      The_Words     : out BCode_Words);

   -- Generate ada test code from program graph.

   procedure ACode (The_Unit : Compilation_Unit_Graph; The_Name : String);

   -- Generate prameter data from program graph.

   procedure Parameters
     (The_Unit    :     Compilation_Unit_Graph;
      The_Inputs  : out Parameter_List_Type;
      The_Outputs : out Parameter_List_Type);

end Generate_Package;
