with STM32.Board;           use STM32.Board;
with Ada.Numerics.Generic_Elementary_Functions;


package body Calculus is
   package Math is new Ada.Numerics.Generic_Elementary_Functions(Float);

   function Vector_To_Point(V: My_Vector) return Point is
      Ret : Point := (Natural(V.X), Natural(V.Y));
   begin
      return ret;
   end;

   function Calculate_Normal_Angle(Delta_X : Integer) return Float is
      Radius : Integer := LCD_Natural_Width / 2;
      F : Float := Float (Delta_X) / Float (Radius);
   begin
      return Math.Arcsin(F);
   end;

   function Angle_To_Direction(angle: Float) return My_Vector is
      dir : My_Vector;
   begin
      dir.X := Math.sin(angle);
      dir.Y := Math.cos(angle);
      return dir;
   end;

   procedure Mult_Vector(vec: in out My_Vector; factor: Float) is
   begin
      vec.X := vec.X * factor;
      vec.Y := vec.Y * factor;
   end;

   function Calculate_Norm(vec: My_Vector) return Float is
   begin
      return Math.sqrt(Float(vec.X * vec.X + vec.Y * vec.Y));
   end;

   function Vector_To_Angle(vec: My_Vector) return Float is
      norm : Float := Calculate_Norm(vec);
   begin
      return Math.Arctan(Float(vec.x / vec.y));
   end;
end Calculus;
