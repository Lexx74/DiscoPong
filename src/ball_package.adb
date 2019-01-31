with Ada.Numerics.Generic_Elementary_Functions;

package body Ball_Package is

   procedure Reset_Ball (This : in out Ball) is
   begin
      This.Pos := Default_Pos;
      This.Direction := Default_Direction;
   end Reset_Ball;

   procedure Draw(This : Ball) is
   begin
      Display.Hidden_Buffer (1).Set_Source (HAL.Bitmap.Green);
      Display.Hidden_Buffer (1).Fill_Circle (Vector_To_Point(This.Pos), Integer(Radius));
   end Draw;

   procedure Update(This : in out Local_Ball; Pad : Paddle; Other_Score : in out Score) is
      Old_Ball : Ball := This;
   begin
     This.Pos.x := This.Pos.x + This.Direction.x;
     This.Pos.y := This.Pos.y + This.Direction.y;

      while This.Bounce(Old_Ball, Pad, Other_Score) loop
        null;
      end loop;
   end Update;

   procedure Global_To_Local(G : in Global_Ball; L : out Local_Ball; Player : Integer) is
   begin
      if Player = 1 then
         L := G;
      else
         L.Pos.X := LCD_Natural_Width_f - G.Pos.X;
         L.Pos.Y := LCD_Natural_Height_f * 2.0 - G.Pos.Y;
         L.Direction.X := -G.Direction.X;
         L.Direction.Y := -G.Direction.Y;
      end if;
   end Global_To_Local;

   procedure Local_To_Global(L : in Local_Ball; G : out Global_Ball; Player : Integer) is
   begin
      if Player = 1 then
         G := L;
      else
         G.Pos.X := LCD_Natural_Width_f - L.Pos.X;
         G.Pos.Y := LCD_Natural_Height_f * 2.0 - L.Pos.Y;
         G.Direction.X := -L.Direction.X;
         G.Direction.Y := -L.Direction.Y;
      end if;
   end Local_To_Global;

   function Do_I_Have_Ball(G : in Global_Ball; Player : Integer) return Boolean is
   begin
      if Player = 1 then
         return G.Pos.Y < LCD_Natural_Height_f;
      else
         return G.Pos.Y >= LCD_Natural_Height_f;
      end if;
   end Do_I_Have_Ball;

   function Bounce(This : in out Ball; Old_Ball : in out Ball; Pad : Paddle; Other_Score : in out Score) return Boolean is
   begin
      if (This.Pos.X + Radius > LCD_Natural_Width_f or else This.Pos.X < Radius) then
         This.Bounce_On_Edge (Old_Ball);
         return True;
      end if;
      if (This.Pos.Y < Radius and then This.Direction.y < 0.0) then
         This.Bounce_On_Goal_Line(Other_Score);
        return True;
      end if;
      if (This.Pos.Y < Float (Pad.Get_Y) + Radius
          and then Old_Ball.Pos.Y >= Float(Pad.Get_Y) + Radius
          and then This.Pos.X >= Float (Pad.Get_Low_Edge_X)
          and then This.Pos.X <= Float (Pad.Get_High_Edge_X)) then
         This.Bounce_On_Paddle (Old_Ball, Pad);
         return True;
      end if;
      return False;
   end Bounce;

   procedure Bounce_On_Goal_Line (This : in out Ball; Other_Score : in out Score) is
   begin
      This.Reset_Ball;
      Other_Score := Other_Score + 1;
   end Bounce_On_Goal_Line;

   procedure Bounce_On_Edge (This : in out Ball; Old_Ball : in out Ball) is
   begin
      if (This.Pos.x + Radius > LCD_Natural_Width_f) then
         -- Collision on left side of screen
         This.Direction.X := - This.Direction.X;
         This.Pos.X := 2.0 * (LCD_Natural_Width_f - Radius) - This.Pos.X;
         Old_Ball.Pos.X := 2.0 * (LCD_Natural_Width_f - Radius) - Old_Ball.Pos.X; -- mirror x of old pos;
      elsif (This.Pos.X < Radius) then
         -- Collision on right side of screen
         This.Direction.X := - This.Direction.X;
         This.Pos.X := Radius + (Radius - This.Pos.X);
         Old_Ball.Pos.X := 2.0 * Radius - Old_Ball.Pos.X;
      end if;
   end Bounce_On_Edge;

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
      Ratio_Before_Impact := -(Old_Ball.Pos.Y - Radius - Float (Pad.Get_Y)) / This.Direction.Y;
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
                   Radius + Float (Pad.Get_Y) + This.Direction.Y * Ratio_After_Impact);
   end Bounce_On_Paddle;

end Ball_Package;
