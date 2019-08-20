-- BC Boolean Compiler
-- Copyright (c) 2007 Paul Estep

with Ada.Tags; use Ada.Tags;
--
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--
with System_Package;     use System_Package;
with Debug_Package;      use Debug_Package;
with Error_Package;      use Error_Package;
with Graph_Package;      use Graph_Package;
with Identifier_Package; use Identifier_Package;
with Type_Package;       use Type_Package;
with Parameter_Package;  use Parameter_Package;
--

separate (Generate_Package)
procedure Parameters
  (The_Unit    :     Compilation_Unit_Graph;
   The_Inputs  : out Parameter_List_Type;
   The_Outputs : out Parameter_List_Type)
is

   -- BC Language constructs for pcode generation.

   procedure Generate (The_Unit : Compilation_Unit_Graph);
   procedure Generate (The_Package : Package_Body_Graph);
   procedure Generate (The_Procedure : Procedure_Body_Graph);
   procedure Generate (The_Parameters : Parameter_Graph);

   procedure Generate (The_Unit : Compilation_Unit_Graph) is
   begin
      Generate (The_Unit.The_Package);
   end Generate;

   procedure Generate (The_Package : Package_Body_Graph) is
   begin
      Generate (The_Package.The_Procedure);
   end Generate;

   procedure Generate (The_Procedure : Procedure_Body_Graph) is
   begin
      Generate (The_Procedure.The_Parameters);
   end Generate;

   procedure Generate (The_Parameters : Parameter_Graph) is
      The_Parameter : Parameter_Graph := The_Parameters;

      procedure Generate_Parameter (The_Parameter : Parameter_Graph) is
         The_Type    : Type_Pointer;
         The_Element : Type_Pointer;
      begin
         The_Type :=
           Type_Identifier
             (Parameter_Node (The_Parameter.all).The_Definition
              .The_Pointer.all)
           .The_Type;

         if The_Parameter.Is_In then
            if Is_Array (The_Type) then
               The_Element := Array_Type (The_Type.all).The_Element;
               if Is_Signed (The_Element) then
                  Append
                    (The_Parameters => The_Inputs,
                     The_Size       => Size_Of (The_Element),
                     The_First      => BCModular (First_Of (The_Element)),
                     The_Last       => BCModular (Last_Of (The_Element)),
                     Is_Signed      => True,
                     The_Length     =>
                       Last_Of (The_Type) - First_Of (The_Type) + 1);
               else
                  Append
                    (The_Parameters => The_Inputs,
                     The_Size       => Size_Of (The_Element),
                     The_First      => BCModular (First_Of (The_Element)),
                     The_Last       => BCModular (Last_Of (The_Element)),
                     Is_Signed      => False,
                     The_Length     =>
                       Last_Of (The_Type) - First_Of (The_Type) + 1);
               end if;

            else
               if Is_Signed
                 (Parameter_Identifier
                    (The_Parameter.The_Identifier.The_Pointer.all)
                  .The_Type)
               then
                  Append
                    (The_Parameters => The_Inputs,
                     The_Size       =>
                       Size_Of
                         (Parameter_Identifier
                              (The_Parameter.The_Identifier.The_Pointer.all)
                          .The_Type),
                     The_First      =>
                       BCModular (First_Of
                         (Parameter_Identifier
                              (The_Parameter.The_Identifier.The_Pointer.all)
                              .The_Type)),
                     The_Last       =>
                       BCModular (Last_Of
                         (Parameter_Identifier
                              (The_Parameter.The_Identifier.The_Pointer.all)
                              .The_Type)),
                     Is_Signed      => True);
               else
                  Append
                    (The_Parameters => The_Inputs,
                     The_Size       =>
                       Size_Of
                         (Parameter_Identifier
                              (The_Parameter.The_Identifier.The_Pointer.all)
                          .The_Type),
                     The_First      =>
                       BCModular (First_Of
                         (Parameter_Identifier
                              (The_Parameter.The_Identifier.The_Pointer.all)
                              .The_Type)),
                     The_Last       =>
                       BCModular (Last_Of
                         (Parameter_Identifier
                              (The_Parameter.The_Identifier.The_Pointer.all)
                              .The_Type)),
                     Is_Signed      => False);
               end if;
            end if;
         end if;

         if The_Parameter.Is_Out then
            if Is_Array (The_Type) then
               The_Element := Array_Type (The_Type.all).The_Element;
               if Is_Signed (The_Element) then
                  Append
                    (The_Parameters => The_Outputs,
                     The_Size       => Size_Of (The_Element),
                     The_First      => BCModular (First_Of (The_Element)),
                     The_Last       => BCModular (Last_Of (The_Element)),
                     Is_Signed      => True,
                     The_Length     =>
                       Last_Of (The_Type) - First_Of (The_Type) + 1);
               else
                  Append
                    (The_Parameters => The_Outputs,
                     The_Size       => Size_Of (The_Element),
                     The_First      => BCModular (First_Of (The_Element)),
                     The_Last       => BCModular (Last_Of (The_Element)),
                     Is_Signed      => False,
                     The_Length     =>
                       Last_Of (The_Type) - First_Of (The_Type) + 1);
               end if;

            else
               if Is_Signed
                 (Parameter_Identifier
                    (The_Parameter.The_Identifier.The_Pointer.all)
                  .The_Type)
               then
                  Append
                    (The_Parameters => The_Outputs,
                     The_Size       =>
                       Size_Of
                         (Parameter_Identifier
                              (The_Parameter.The_Identifier.The_Pointer.all)
                          .The_Type),
                     The_First      =>
                       BCModular (First_Of
                         (Parameter_Identifier
                              (The_Parameter.The_Identifier.The_Pointer.all)
                              .The_Type)),
                     The_Last       =>
                       BCModular (Last_Of
                         (Parameter_Identifier
                              (The_Parameter.The_Identifier.The_Pointer.all)
                              .The_Type)),
                     Is_Signed      => True);
               else
                  Append
                    (The_Parameters => The_Outputs,
                     The_Size       =>
                       Size_Of
                         (Parameter_Identifier
                              (The_Parameter.The_Identifier.The_Pointer.all)
                          .The_Type),
                     The_First      =>
                       BCModular (First_Of
                         (Parameter_Identifier
                              (The_Parameter.The_Identifier.The_Pointer.all)
                              .The_Type)),
                     The_Last       =>
                       BCModular (Last_Of
                         (Parameter_Identifier
                              (The_Parameter.The_Identifier.The_Pointer.all)
                              .The_Type)),
                     Is_Signed      => False);
               end if;
            end if;
         end if;

      end Generate_Parameter;

   begin
      while The_Parameter /= null loop
         Generate_Parameter (The_Parameter);
         The_Parameter := The_Parameter.The_Next;
      end loop;
   end Generate;

begin
   Generate (The_Unit);
end Parameters;
