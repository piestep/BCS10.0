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
with Test_Package;
with Test_Suite;
--

procedure Bct is

   Usage_Error : exception;

   The_Argument : Positive         := 1;
   The_Suite    : Unbounded_String := Null_Unbounded_String;
   The_Case     : Unbounded_String := Null_Unbounded_String;

   The_Filter  : Test_Filter_Access := null;
   The_Options : AUnit_Options;

   procedure Run is new AUnit.Run.Test_Runner (Test_Suite.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;

   -- Print usage information.

   procedure Put_Usage is
   begin
      Put_Line ("usage:  test [ switches ]");
      Put_Line ("  -h,  --help       - print usage");
      Put_Line ("  -s,  --suite      - run test suite");
      Put_Line ("  -c,  --case       - run test case of suite");
      Put_Line ("  -d,  --dump       - dump test listings to output");
      Put_Line ("  -g,  --generate   - generate xml test file");
      Put_Line ("  -r,  --replace    - replace xml test with generated file");
   end Put_Usage;

begin

   while The_Argument <= Argument_Count loop
      if Argument (The_Argument) = "-h" or
        Argument (The_Argument) = "--help"
      then
         Put_Usage;
         return;

      elsif Argument (The_Argument) = "-s" or
        Argument (The_Argument) = "--suite"
      then
         The_Argument := The_Argument + 1;
         The_Suite    := To_Unbounded_String (Argument (The_Argument));

      elsif Argument (The_Argument) = "-c" or
        Argument (The_Argument) = "--case"
      then
         The_Argument := The_Argument + 1;
         The_Case     := To_Unbounded_String (Argument (The_Argument));

      elsif Argument (The_Argument) = "-d" or
        Argument (The_Argument) = "--dump"
      then
         if Test_Package.Generate_Flag then
            Put_Line ("-d, --dump can not be used with -g, --generate");
            Put_Usage;
            return;
         end if;

         Test_Package.Dump_Flag := True;

      elsif Argument (The_Argument) = "-g" or
        Argument (The_Argument) = "--generate"
      then
         if Test_Package.Dump_Flag then
            Put_Line ("-g, --generate can not be used with -d, --dump");
            Put_Usage;
            return;
         end if;

         Test_Package.Generate_Flag := True;

      elsif Argument (The_Argument) = "-r" or
        Argument (The_Argument) = "--replace"
      then
         if not Test_Package.Generate_Flag then
            Put_Line ("-r, --replace can only be used with -g, --generate");
            Put_Usage;
            return;
         end if;

         Test_Package.Replace_Flag := True;

      else
         Put_Line ("Unknown switch: " & Argument (The_Argument));
         Put_Usage;
         return;
      end if;

      The_Argument := The_Argument + 1;
   end loop;

   if The_Suite /= Null_Unbounded_String then
      The_Filter := new Name_Filter;

      if The_Case /= Null_Unbounded_String then
         Set_Name
           (Name_Filter (The_Filter.all),
            To_String (The_Suite) & " : " & To_String (The_Case));
         Put_Line (To_String (The_Suite) & " : " & To_String (The_Case));
      else
         Set_Name (Name_Filter (The_Filter.all), To_String (The_Suite));
         Put_Line (To_String (The_Suite));
      end if;
   end if;

   The_Options :=
     (Global_Timer     => False,
      Test_Case_Timer  => False,
      Report_Successes => True,
      Filter           => The_Filter);

   Run (Reporter, The_Options);
end Bct;
