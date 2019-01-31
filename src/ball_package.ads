with STM32.Board; use STM32.Board;
with Calculus; use Calculus;
with Paddle_Package; use Paddle_Package;
with HAL.Bitmap; use HAL.Bitmap;

package Ball_Package is
   pragma Assertion_Policy(Check);

   LCD_Natural_Width_f : constant Float := Float(LCD_Natural_Width);
   LCD_Natural_Height_f : constant Float := Float(LCD_Natural_Height);

   Radius : constant Float := 10.0;

   Default_Pos : constant My_Vector := (Float(LCD_Natural_Width / 2), Float(250));
   Default_Direction : constant My_Vector := (0.0, -3.0);

   subtype X_Range is Float range Radius .. LCD_Natural_Width_f - Radius;

   subtype Y_Local_Range is Float range Radius .. LCD_Natural_Height_f;
   subtype Y_Global_Range is Float range Radius .. LCD_Natural_Height_f * 2.0 - Radius;

   type Ball is tagged record
      Pos : My_Vector := Default_Pos;
      Direction : My_Vector := Default_Direction;
   end record;

   subtype Local_Ball is Ball;

   subtype Global_Ball is Ball;

   procedure Reset_Ball (This : in out Ball)
      with Post => (This.Pos = Default_Pos and then This.Direction = Default_Direction);

   procedure Update (This : in out Local_Ball; Pad : Paddle)
     with Pre  => (This.Pos.X in X_Range
                   and then This.Pos.Y in Y_Local_Range),
          Post => (This.Pos.X in X_Range
                   and then This.Pos.Y in Y_Global_Range);

   procedure Draw (This : Ball);

   procedure Global_To_Local(G : in Global_Ball; L : out Local_Ball; Player : Integer);

   procedure Local_To_Global(L : in Local_Ball; G : out Global_Ball; Player : Integer);

   function Do_I_Have_Ball(G : in Global_Ball; Player : Integer) return Boolean;

private

   function Bounce (This : in out Ball; Old_Ball : in out Ball; Pad : Paddle) return Boolean;
   procedure Bounce_On_Goal_Line (This : in out Ball)
      with Pre => (This.Pos.Y < Radius and then This.Direction.Y < 0.0),
           Post => This.Pos = Default_Pos and then This.Direction = Default_Direction;

   procedure Bounce_On_Edge (This : in out Ball; Old_Ball : in out Ball)
      with Pre => (This.Pos.X not in X_Range);

   procedure Bounce_On_Paddle (This : in out Ball; Old_Ball : in out Ball; Pad : Paddle)
      with Pre => (This.Pos.Y < Float (Pad.Get_Y) + Radius
                   and then This.Pos.X >= Float (Pad.Get_X - Pad.Get_Width / 2)
                   and then This.Pos.X <= Float (Pad.Get_X + Pad.Get_Width / 2));

end Ball_Package; 
