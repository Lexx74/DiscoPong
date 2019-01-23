with STM32.Board; use STM32.Board;

package Paddle is
   type Paddle is limited private;

   function Get_X (This : Paddle) return Positive;
   function Get_Y (This : Paddle) return Positive;
   function Get_Width (This : Paddle) return Positive;

   procedure Set_X (This : in out Paddle; X : Positive)
      with Pre => X < Positive(LCD_Natural_Width);
   procedure Set_Y (This : in out Paddle; Y : Positive)
      with Pre => Y < Positive(LCD_Natural_Height);
   procedure Set_Width (This : in out Paddle; W : Positive)
      with Pre => W >= 1 and W < LCD_Natural_Width;

private
   type Paddle is record
      X : Positive;
      Y : Positive;
      Width : Positive;
   end record;
end Paddle;
