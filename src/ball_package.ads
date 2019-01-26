with STM32.Board; use STM32.Board;
with Calculus; use Calculus;

package Ball_Package is
   pragma Assertion_Policy(Check);

   LCD_Natural_Width_f : constant Float := Float(LCD_Natural_Width);
   LCD_Natural_Height_f : constant Float := Float(LCD_Natural_Height);

   type Ball is tagged record
      Pos : My_Vector := (Float(LCD_Natural_Width / 2), Float(150));
      Direction : My_Vector := (8.0, -2.0);
      Radius : Float := 10.0;
   end record;

   procedure Update(This : in out Ball)
      with Post => (This.Pos.x >= This.Radius
                    and then Integer(This.Pos.x + This.Radius) <= LCD_Natural_Width
                    and then This.Pos.y >= This.radius);
   procedure Draw(This : Ball);

private

   function Bounce(This : in out Ball; Old_Ball : in out Ball) return Boolean;

end Ball_Package;
