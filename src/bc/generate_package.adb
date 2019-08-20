-- BC Boolean Compiler
-- Copyright (c) 2007 Paul Estep

with Ada.Text_IO; use Ada.Text_IO;
--
with Debug_Package;  use Debug_Package;
with Error_Package;  use Error_Package;
with Source_Package; use Source_Package;
with Graph_Package;  use Graph_Package;
--

package body Generate_Package is

   -- Generate pcode from program graph.

   procedure PCode
     (The_Unit   :        Compilation_Unit_Graph;
      The_Code   : in out Code;
      The_Length :    out Address) is separate;

   -- Generate bcode from program graph.

   procedure BCode
     (The_Unit      :     Compilation_Unit_Graph;
      The_Variables : out Natural;
      The_Words     : out BCode_Words) is separate;

   -- Generate ada test code from program graph.

   procedure ACode
     (The_Unit : Compilation_Unit_Graph;
      The_Name : String) is separate;

   -- Generate prameter data from program graph.

   procedure Parameters
     (The_Unit    :     Compilation_Unit_Graph;
      The_Inputs  : out Parameter_List_Type;
      The_Outputs : out Parameter_List_Type) is separate;

begin
   Debug (Debug_Initialization, "Generate_Package");
end Generate_Package;
