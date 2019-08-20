-- BC Boolean Compiler
-- Copyright (c) 2007 Paul Estep

--      with "f:/bc/cvs/src/bc/bc_package";
--      with "f:/bc/cvs/src/compiler/compiler";
--      with "f:/bc/cvs/src/brun";
--      with "f:/bc/cvs/src/prun";
--      project test is
--
--         Build_Dir := external ("BUILD_DIR", ".");
--
--         for Source_Files use
--           ("apbtest.adb", "arun_package.ads",
--           "test1-arun_package.abc", "test.ads", "test.adb");
--
--         for Source_Dirs use (".","f:/bc/cvs/src/apbtest");
--         for Object_Dir use Build_Dir;
--         for Exec_Dir use ".";
--
--         for Main use ("apbtest");
--
--    package Builder is
--        for Default_Switches ("Ada") use ("-g");
--        for Executable ("apbtest.adb") use "test";
--    end Builder;
--
--         package Naming is
--            for Body ("apbtest") use "apbtest.adb";
--            for Spec ("arun_package") use "arun_package.ads";
--            for Body ("arun_package") use "test-arun_package.abc";
--            for Spec ("test") use "test.ads";
--            for Body ("test") use "test.adb";
--         end Naming;
--
--      end test;

separate (Generate_Package.ACode)
procedure Gnat_File
  (The_Name    : String;
   The_Package : Unbounded_String;
   The_Date    : String)
is

   QUOTE : constant Character := '"';

   The_Source : Unbounded_String;
   The_File   : File_Type;

begin
   if Exists ("BC_SOURCE_PATH") then
      The_Source :=
        To_Unbounded_String
          (Ada.Environment_Variables.Value ("BC_SOURCE_PATH"));
   else
      The_Source := To_Unbounded_String (".");
   end if;

   Create (The_File, Out_File, The_Name & ".gpr");

   Put_Line (The_File, "-- BC Boolean Compiler Ada Test Generator.");
   Put_Line
     (The_File,
      "-- Generated from '" &
      The_Name &
      SOURCE_EXTENSION &
      "', " &
      The_Date &
      ".");
   New_Line (The_File);
   Put_Line
     (The_File,
      "with " & QUOTE & To_String (The_Source) & "/bc" & QUOTE & ";");
   Put_Line
     (The_File,
      "with " & QUOTE & To_String (The_Source) & "/brun" & QUOTE & ";");
   Put_Line
     (The_File,
      "with " & QUOTE & To_String (The_Source) & "/prun" & QUOTE & ";");
   New_Line (The_File);
   Put_Line
     (The_File,
      "project " & To_Lower (To_String (The_Package)) & " is");
   New_Line (The_File);
   Put_Line
     (The_File,
      "    Build_Dir := external (" &
      QUOTE &
      "BUILD_DIR" &
      QUOTE &
      ", " &
      QUOTE &
      "." &
      QUOTE &
      ");");
   Put_Line
     (The_File,
      "    BC_Source_Path := external (" &
      QUOTE &
      "BC_SOURCE_PATH" &
      QUOTE &
      ", " &
      QUOTE &
      To_String (The_Source) &
      QUOTE &
      ");");
   New_Line (The_File);
   Put_Line (The_File, "    for Source_Files use");
   Put_Line (The_File, "        (" & QUOTE & "apbtest.adb" & QUOTE & ", ");
   Put_Line (The_File, "        " & QUOTE & "arun_package.ads" & QUOTE & ", ");
   Put_Line
     (The_File,
      "        " & QUOTE & The_Name & "-arun_package.abc" & QUOTE & ", ");
   Put_Line (The_File, "        " & QUOTE & The_Name & ".ads" & QUOTE & ", ");
   Put_Line (The_File, "        " & QUOTE & The_Name & ".adb" & QUOTE & ");");
   New_Line (The_File);
   Put_Line
     (The_File,
      "    for Source_Dirs use (" &
      QUOTE &
      "." &
      QUOTE &
      ", " &
      "BC_Source_Path&" &
      QUOTE &
      "/apbtest" &
      QUOTE &
      ");");
   New_Line (The_File);
   Put_Line (The_File, "    for Object_Dir use Build_Dir;");
   Put_Line (The_File, "    for Exec_Dir use " & QUOTE & "." & QUOTE & ";");
   New_Line (The_File);
   Put_Line
     (The_File,
      "    for Main use (" & QUOTE & "apbtest" & QUOTE & ");");
   New_Line (The_File);
   Put_Line (The_File, "    package Builder is");
   Put_Line
     (The_File,
      "        for Default_Switches (" &
      QUOTE &
      "Ada" &
      QUOTE &
      ") use (" &
      QUOTE &
      "-g" &
      QUOTE &
      ");");
   Put_Line
     (The_File,
      "        for Executable (" &
      QUOTE &
      "apbtest.adb" &
      QUOTE &
      ") use " &
      QUOTE &
      The_Name &
      QUOTE &
      ";");
   Put_Line (The_File, "    end Builder;");
   New_Line (The_File);
   Put_Line (The_File, "    package Naming is");
   Put_Line
     (The_File,
      "        for Body (" &
      QUOTE &
      "apbtest" &
      QUOTE &
      ") use " &
      QUOTE &
      "apbtest.adb" &
      QUOTE &
      ";");
   Put_Line
     (The_File,
      "        for Spec (" &
      QUOTE &
      "arun_package" &
      QUOTE &
      ") use " &
      QUOTE &
      "arun_package.ads" &
      QUOTE &
      ";");
   Put_Line
     (The_File,
      "        for Body (" &
      QUOTE &
      "arun_package" &
      QUOTE &
      ") use " &
      QUOTE &
      The_Name &
      "-arun_package.abc" &
      QUOTE &
      ";");
   Put_Line
     (The_File,
      "        for Spec (" &
      QUOTE &
      To_Lower (To_String (The_Package)) &
      QUOTE &
      ") use " &
      QUOTE &
      The_Name &
      ".ads" &
      QUOTE &
      ";");
   Put_Line
     (The_File,
      "        for Body (" &
      QUOTE &
      To_Lower (To_String (The_Package)) &
      QUOTE &
      ") use " &
      QUOTE &
      The_Name &
      ".adb" &
      QUOTE &
      ";");
   Put_Line (The_File, "    end Naming;");
   New_Line (The_File);
   Put_Line (The_File, "end " & To_Lower (To_String (The_Package)) & ";");

   Close (The_File);

end Gnat_File;
