with STM32.Board; use STM32.Board;
with Calculus; use Calculus;

package Ball_Package is

   LCD_Natural_Width_f : constant Float := Float(LCD_Natural_Width);
   LCD_Natural_Height_f : constant Float := Float(LCD_Natural_Height);

   type Ball is tagged private;

   procedure Update(This : in out Ball);
   procedure Draw(This : Ball);

private
   type Ball is tagged record
      Pos : My_Vector := (Float(LCD_Natural_Width / 2), Float(15));
      Direction : My_Vector := (2.0, -2.0);
      Radius : Float := 10.0;
   end record;

   function Bounce(This : in out Ball; Old_Ball : in out Ball) return Boolean;

end Ball_Package;
