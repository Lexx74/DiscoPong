with Ada.Numerics.Generic_Elementary_Functions;

package body Ball_Package is

   procedure Reset_Ball (This : in out Ball) is
   begin
      This.Pos := Default_Pos;
      This.Direction := Default_Direction;
   end Reset_Ball;

   procedure Draw(This : Ball) is
   begin
      Display.Hidden_Buffer (1).Fill_Circle (Vector_To_Point(This.Pos), Integer(This.Radius));
   end Draw;

   procedure Update(This : in out Ball; Pad : Paddle) is
      Old_Ball : Ball := This;
   begin
     This.Pos.x := This.Pos.x + This.Direction.x;
     This.Pos.y := This.Pos.y + This.Direction.y;

      while This.Bounce(Old_Ball, Pad) loop
        null;
      end loop;
   end Update;

   procedure Switch_Screen (This : in out Ball) is
      Diff : Float := This.Pos.Y - LCD_Natural_Width_f;
   begin
      This.Pos.Y := LCD_Natural_Width_f - Diff;
      This.Direction.X := - This.Direction.X;
      This.Direction.Y := - This.Direction.Y;
   end Switch_Screen;

   function Bounce(This : in out Ball; Old_Ball : in out Ball; Pad : Paddle) return Boolean is
   begin
      if (This.Pos.X + This.Radius > LCD_Natural_Width_f or else This.Pos.X < This.Radius) then
         This.Bounce_On_Edge (Old_Ball);
         return True;
      end if;
      if (This.Pos.Y + This.Radius > LCD_Natural_Height_f) then
         This.Bounce_On_Transition_Edge;
         return True;
      end if;
      if (This.Pos.Y < This.Radius and then This.Direction.y < 0.0) then
         This.Bounce_On_Goal_Line;
        return True;
      end if;
      if (This.Pos.Y < Float (Pad.Get_Y) + This.Radius
          and then Old_Ball.Pos.Y >= Float(Pad.Get_Y) + This.Radius
          and then This.Pos.X >= Float (Pad.Get_Low_Edge_X)
          and then This.Pos.X <= Float (Pad.Get_High_Edge_X)) then
         This.Bounce_On_Paddle (Old_Ball, Pad);
         return True;
      end if;
      return False;
   end Bounce;

   procedure Bounce_On_Goal_Line (This : in out Ball) is
   begin
      This.Reset_Ball;
   end Bounce_On_Goal_Line;

   procedure Bounce_On_Edge (This : in out Ball; Old_Ball : in out Ball) is
   begin
      if (This.Pos.x + This.Radius > LCD_Natural_Width_f) then
         -- Collision on left side of screen
         This.Direction.X := - This.Direction.X;
         This.Pos.X := 2.0 * (LCD_Natural_Width_f - This.Radius) - This.Pos.X;
         Old_Ball.Pos.X := 2.0 * (LCD_Natural_Width_f - This.Radius) - Old_Ball.Pos.X; -- mirror x of old pos;
      elsif (This.Pos.X < This.Radius) then
         -- Collision on right side of screen
         This.Direction.X := - This.Direction.X;
         This.Pos.X := This.Radius + (This.Radius - This.Pos.X);
         Old_Ball.Pos.X := 2.0 * This.Radius - Old_Ball.Pos.X;
      end if;
   end Bounce_On_Edge;

   procedure Bounce_On_Transition_Edge (This : in out Ball) is
   begin
         -- Collision on transition edge of screen
      This.Direction.Y := - This.Direction.Y;
      This.Pos.Y := LCD_Natural_Height_f - (This.Pos.Y + This.Radius - LCD_Natural_Height_f) - This.Radius;
   end Bounce_On_Transition_Edge;

   procedure Bounce_On_Paddle (This : in out Ball; Old_Ball : in out Ball; Pad : Paddle) is
      Max_Angle : constant Float := 80.0 * Ada.Numerics.Pi / 180.0; -- radian

      Normal_Angle : Float;
      Ball_Angle : Float;
      New_Ball_Angle : Float;
      Norm : Float;

      Ratio_Before_Impact : Float;
      Ratio_After_Impact : Float;
      Impact_X : Float;
   begin
      Ratio_Before_Impact := -(Old_Ball.Pos.Y - This.Radius - Float (Pad.Get_Y)) / This.Direction.Y;
      Ratio_After_Impact := 1.0 - Ratio_Before_Impact;
      Impact_X := Old_Ball.Pos.X + Ratio_Before_Impact * This.Direction.X;

      Norm := Calculus.Calculate_Norm(This.Direction);
      Normal_Angle := Calculus.Calculate_Normal_Angle(Integer(Impact_X - Float (Pad.Get_X)));
      Ball_Angle := Calculus.Vector_To_Angle(This.Direction);
      New_Ball_Angle := Normal_Angle + (Normal_Angle - Ball_Angle);

      -- If the ball rebound at a near-flat angle, it can rebounce but still going down
      if New_Ball_Angle < -Max_Angle then
         New_Ball_Angle := -Max_Angle;
      elsif New_Ball_Angle > Max_Angle then
         New_Ball_Angle := Max_Angle;
      end if;

      This.Direction := Calculus.Angle_To_Direction(New_Ball_Angle);
      Calculus.Mult_Vector(This.Direction, Norm);
      This.Pos := (Impact_X + This.Direction.X * Ratio_After_Impact,
                   This.Radius + Float (Pad.Get_Y) + This.Direction.Y * Ratio_After_Impact);
   end Bounce_On_Paddle;

end Ball_Package;
