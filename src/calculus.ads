with Ada.Numerics.Generic_Elementary_Functions; --use Ada.Numerics.Elementary_Functions;
with HAL.Bitmap;            use HAL.Bitmap;

package Calculus is

   type My_Vector is record
      X : Float;
      Y : Float;
   end record;
   
   function Vector_To_Point(V: My_Vector) return Point;
   function Calculate_Normal_Angle(pos_x: Integer) return Float;
   function Angle_To_Direction(angle: Float) return My_Vector;
   procedure Mult_Vector(vec: in out My_Vector; factor: Float);
   function Calculate_Norm(vec: My_Vector) return Float;
   function Vector_To_Angle(vec: My_Vector) return Float;

end Calculus;
