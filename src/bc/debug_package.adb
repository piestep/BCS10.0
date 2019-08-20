-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with Ada.Text_IO; use Ada.Text_IO;
--

package body Debug_Package is

   -- Raise critical error if flag is true.

   procedure Check (The_Flag : Boolean) is
   begin
      if not The_Flag then
         raise Critical_Error;
      end if;
   end Check;

   -- Print message if flag is true.

   procedure Debug (The_Flag : Boolean; The_Message : String) is
   begin
      if The_Flag then
         Put_Line (The_Message);
      end if;
   end Debug;

end Debug_Package;
