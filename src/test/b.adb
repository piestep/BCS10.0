-- BCS Boolean Compiler System
-- Copyright (c) 2017 Paul Estep

with AUnit.Reporter.Text;
with AUnit.Run;
--
with AUnit.Test_Filters; use AUnit.Test_Filters;
with AUnit.Options;      use AUnit.Options;
--
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Command_Line;      use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--
with Test_Suite;
--

procedure B is

   Usage_Error : exception;

   The_Argument : Positive         := 1;
   The_Suite    : Unbounded_String := Null_Unbounded_String;
   The_Case     : Unbounded_String := Null_Unbounded_String;

   The_Filter  : Test_Filter_Access := null;
   The_Options : AUnit_Options;

   Dump_Flag     : Boolean := False;
   Generate_Flag : Boolean := False;

   procedure Run is new AUnit.Run.Test_Runner (Test_Suite.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;


begin

   The_Filter := new Name_Filter;
   Set_Name (Name_Filter (The_Filter.all), "Variable_Package.Io_Package.Io_Test.");

   The_Options :=
     (Global_Timer     => False,
      Test_Case_Timer  => False,
      Report_Successes => True,
      Filter           => The_Filter);

   Run (Reporter, The_Options);
end B;
