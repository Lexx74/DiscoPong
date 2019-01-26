with Ada.Numerics.Generic_Elementary_Functions;

package body Ball_Package is
   procedure Draw(This : Ball) is
   begin
      Display.Hidden_Buffer (1).Fill_Circle (Vector_To_Point(This.Pos), Integer(This.Radius));
   end Draw;

   procedure Update(This : in out Ball) is
      Old_Ball : Ball := This;
   begin
     This.Pos.x := This.Pos.x + This.Direction.x;
     This.Pos.y := This.Pos.y + This.Direction.y;

      while This.Bounce(Old_Ball) loop
        null;
      end loop;
   end Update;

   function Bounce(This : in out Ball; Old_Ball : in out Ball) return Boolean is
      Max_Angle : constant Float := 80.0 * Ada.Numerics.Pi / 180.0; -- radian
      Normal_Angle : Float;
      Ball_Angle : Float;
      New_Ball_Angle : Float;
      Norm : Float;
   begin
      if (This.Pos.x + This.Radius > LCD_Natural_Width_f) then
         This.Direction.X := - This.Direction.X;
         This.Pos.x := 2.0 * (LCD_Natural_Width_f - This.Radius) - This.Pos.x;
         Old_Ball.Pos.x := 2.0 * (LCD_Natural_Width_f - This.Radius) - Old_Ball.Pos.x; -- mirror x of old pos;
         return True;
      end if;
      if (This.Pos.x < This.Radius) then
         This.Direction.X := - This.Direction.X;
         This.Pos.x := This.Radius + (This.Radius - This.Pos.X);
         Old_Ball.Pos.x := 2.0 * This.Radius - Old_Ball.Pos.x;
         return True;
      end if;
      if (This.Pos.y + This.Radius > LCD_Natural_Height_f) then
         This.Direction.Y := - This.Direction.Y;
         This.Pos.y := LCD_Natural_Height_f - (This.Pos.y + This.Radius - LCD_Natural_Height_f) - This.Radius;
         return True;
      end if;
      if (This.Pos.y < This.Radius and then This.Direction.y < 0.0) then
         declare
            Ratio_Before_Impact : Float;
            Ratio_After_Impact : Float;
            Impact_X : Float;
         begin
            Ratio_Before_Impact := -(Old_Ball.Pos.y - This.Radius) / This.Direction.y;
            Ratio_After_Impact := 1.0 - Ratio_Before_Impact;
            Impact_X := Old_Ball.Pos.x + Ratio_Before_Impact * This.Direction.x;

            Norm := calculus.Calculate_Norm(This.Direction);
            Normal_Angle := calculus.Calculate_Normal_Angle(Integer(Impact_X));
            Ball_Angle := calculus.Vector_To_Angle(This.Direction);
            New_Ball_Angle := Normal_Angle + (Normal_Angle - Ball_Angle);

            -- If the ball rebound at a near-flat angle, it can rebounce but still going down
            if New_Ball_Angle < -Max_Angle then
               New_Ball_Angle := -Max_Angle;
            elsif New_Ball_Angle > Max_Angle then
               New_Ball_Angle := Max_Angle;
            end if;

            This.Direction := calculus.Angle_To_Direction(New_Ball_Angle);
            calculus.Mult_Vector(This.Direction, Norm);
            This.Pos := (Impact_X + This.Direction.x * Ratio_After_Impact, This.Radius + This.Direction.y * Ratio_After_Impact);
         end;
        return True;
      end if;
      return False;
   end bounce;
end Ball_Package;
