package body Calculus is

   function VectorToPoint(V: My_Vector) return Point is
      ret : Point := (Natural(V.X), Natural(V.Y));
   begin
      return ret;
   end;

   function calculateNormalAngle(pos_x: Integer) return Float is
      ret : Float;
      radius : Integer := LCD_Natural_Width / 2;
      f : Float;
   begin
      f := Float(pos_x - radius);
      f := f / Float(radius);
      ret := Math.Arcsin(f);
      return ret / 2.0;
   end;

   function angleToDirection(angle: Float) return My_Vector is
      dir : My_Vector;
   begin
      dir.X := Math.sin(angle);
      dir.Y := Math.cos(angle);
      return dir;
   end;
   
   procedure multVector(vec: in out My_Vector; factor: Float) is
   begin
      vec.X := vec.X * factor;
      vec.Y := vec.Y * factor;
   end;
   
   function calculateNorm(vec: My_Vector) return Float is
   begin
      return Math.sqrt(Float(vec.X * vec.X + vec.Y * vec.Y));
   end;

   function vectorToAngle(vec: My_Vector) return Float is
      norm : Float := calculateNorm(vec);
   begin
      return Math.Arctan(Float(vec.x / vec.y));
   end;
end Calculus;
