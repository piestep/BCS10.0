-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with AUnit;
with AUnit.Test_Fixtures;
with AUnit.Test_Cases;
--

package Variable_Package.Variable_Test is

   -- Image of variables to create test results for variable set image. NOTE:
   -- index into image for a variable is 3*variable with a length of 3.

   IMAGE_OF_ALL_VARIABLES : constant String :=
     " a0 b0 c0 d0 e0 f0 g0 h0 i0 j0 k0 l0 m0 n0 o0 p0 q0 r0 s0 t0 u0 v0 w0 x0 y0 z0" &
     " a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1 r1 s1 t1 u1 v1 w1 x1 y1 z1" &
     " a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 l2 m2 n2 o2 p2 q2 r2 s2 t2 u2 v2 w2 x2 y2 z2" &
     " a3 b3 c3 d3 e3 f3 g3 h3 i3 j3 k3 l3 m3 n3 o3 p3 q3 r3 s3 t3 u3 v3 w3 x3 y3 z3" &
     " a4 b4 c4 d4 e4 f4 g4 h4 i4 j4 k4 l4 m4 n4 o4 p4 q4 r4 s4 t4 u4 v4 w4 x4";

   type Test is new AUnit.Test_Cases.Test_Case with null record;

   overriding function Name (The_Test : in Test) return AUnit.Test_String;
   overriding procedure Register_Tests (The_Test : in out Test);

   procedure Test_Is_Included
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Include (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Exclude (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Array_Of (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Not (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_And (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Or (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Xor (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Image_Of_Variable
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Not_Image_Of
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Boolean_Of
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Image_Of_Set_Of_Variables
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class);

end Variable_Package.Variable_Test;