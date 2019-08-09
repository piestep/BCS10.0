-- BC Boolean Compiler
-- Copyright (c) 2015 Paul Estep

with AUnit;
with AUnit.Test_Fixtures;
with AUnit.Test_Cases;
--

package Term_Package.Term_Test is

   -- Image of variables to create test results for term images. NOTE: index
   -- into image for a variable is 2 * variable with a length of 2.

   IMAGE_OF_ALL_TERMS : constant String :=
     "a0b0c0d0e0f0g0h0i0j0k0l0m0n0o0p0q0r0s0t0u0v0w0x0y0z0" &
     "a1b1c1d1e1f1g1h1i1j1k1l1m1n1o1p1q1r1s1t1u1v1w1x1y1z1" &
     "a2b2c2d2e2f2g2h2i2j2k2l2m2n2o2p2q2r2s2t2u2v2w2x2y2z2" &
     "a3b3c3d3e3f3g3h3i3j3k3l3m3n3o3p3q3r3s3t3u3v3w3x3y3z3" &
     "a4b4c4d4e4f4g4h4i4j4k4l4m4n4o4p4q4r4s4t4u4v4w4x4";

   -- Image of not variables to create test results for term images. NOTE: index
   -- into image for a variable is 2 * variable with a length of 2.

   NOT_IMAGE_OF_ALL_TERMS : constant String :=
     "A0B0C0D0E0F0G0H0I0J0K0L0M0N0O0P0Q0R0S0T0U0V0W0X0Y0Z0" &
     "A1B1C1D1E1F1G1H1I1J1K1L1M1N1O1P1Q1R1S1T1U1V1W1X1Y1Z1" &
     "A2B2C2D2E2F2G2H2I2J2K2L2M2N2O2P2Q2R2S2T2U2V2W2X2Y2Z2" &
     "A3B3C3D3E3F3G3H3I3J3K3L3M3N3O3P3Q3R3S3T3U3V3W3X3Y3Z3" &
     "A4B4C4D4E4F4G4H4I4J4K4L4M4N4O4P4Q4R4S4T4U4V4W4X4";

   type Test is new AUnit.Test_Cases.Test_Case with null record;

   overriding function Name (The_Test : in Test) return AUnit.Test_String;
   overriding procedure Register_Tests (The_Test : in out Test);

   procedure Test_To_Index (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Create (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Create_With_Value
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Set (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Is_Set (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Is_Equal (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Index_Of (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Length_Of
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Image_Of (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Image_Of_With_Variables
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_To_Boolean_Array
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Boolean_Of
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class);

end Term_Package.Term_Test;
