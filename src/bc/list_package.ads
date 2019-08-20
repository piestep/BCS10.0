-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with BC_Package; use BC_Package;
--

-- A package to list a Boolean Compiler (BC) source program.

package List_Package is

   -- List Boolean Compiler (BC) source program switch.

   List_Source : Boolean := True;

   -- List the BC source program and errors.

   procedure List
     (The_Source_File_Name : String;
      XML_Format           : Boolean := False);

end List_Package;
