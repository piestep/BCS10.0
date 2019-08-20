-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

procedure Error_Package.Clear is
begin

   Message_Vector.Clear (The_Messages);
   The_Number_Of_Errors   := 0;
   The_Number_Of_Warnings := 0;

end Error_Package.Clear;
