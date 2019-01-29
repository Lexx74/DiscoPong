with STM32.Board; use STM32.Board;
with Calculus; use Calculus;
with Paddle_Package; use Paddle_Package;

package Ball_Package is
   pragma Assertion_Policy(Check);

   Default_Pos : constant My_Vector := (Float(LCD_Natural_Width / 2), Float(250));
   Default_Direction : constant My_Vector := (0.0, -2.0);

   LCD_Natural_Width_f : constant Float := Float(LCD_Natural_Width);
   LCD_Natural_Height_f : constant Float := Float(LCD_Natural_Height);

   type Ball is tagged record
      Pos : My_Vector := Default_Pos;
      Direction : My_Vector := Default_Direction;
      Radius : Float := 10.0;
   end record;

   procedure Reset_Ball (This : in out Ball)
      with Post => (This.Pos = Default_Pos and then This.Direction = Default_Direction);

   procedure Update (This : in out Ball; Pad : Paddle)
      with Post => (This.Pos.x >= This.Radius
                    and then Integer(This.Pos.x + This.Radius) <= LCD_Natural_Width
                    and then This.Pos.y >= This.radius);
   procedure Draw (This : Ball);
   procedure Switch_Screen (This : in out Ball)
      with Pre => This.Pos.Y > LCD_Natural_Height_f,
           Post => (This.Pos.Y < LCD_Natural_Height_f and then
                    This'Old.Direction.X = - This.Direction.X and then
                    This'Old.Direction.Y = - This.Direction.Y);

private

   function Bounce (This : in out Ball; Old_Ball : in out Ball; Pad : Paddle) return Boolean;
   procedure Bounce_On_Goal_Line (This : in out Ball)
      with Pre => (This.Pos.Y < This.Radius and then This.Direction.Y < 0.0),
           Post => This.Pos = Default_Pos and then This.Direction = Default_Direction;
   procedure Bounce_On_Edge (This : in out Ball; Old_Ball : in out Ball)
      with Pre => (This.Pos.X + This.Radius > LCD_Natural_Width_f
                   or else This.Pos.X < This.Radius);
   procedure Bounce_On_Transition_Edge (This : in out Ball)
      with Pre => (This.Pos.Y + This.Radius > LCD_Natural_Height_f);

   procedure Bounce_On_Paddle (This : in out Ball; Old_Ball : in out Ball; Pad : Paddle)
      with Pre => (This.Pos.Y < Float (Pad.Get_Y) + This.Radius
                   and then This.Pos.X >= Float (Pad.Get_X - Pad.Get_Width / 2)
                   and then This.Pos.X <= Float (Pad.Get_X + Pad.Get_Width / 2));

end Ball_Package; 
