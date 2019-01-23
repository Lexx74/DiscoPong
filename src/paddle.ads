with STM32.Board; use STM32.Board;
with HAL.Touch_Panel; use HAL.Touch_Panel;

package Paddle is
   type Game_Paddle is private;

   Paddle_Default_Y : constant Positive := 10;

   function Get_X (This : Game_Paddle) return Positive;
   function Get_Y (This : Game_Paddle) return Positive;
   function Get_Width (This : Game_Paddle) return Positive;

   procedure Set_X (This : in out Game_Paddle; X : Positive)
      with Pre => X < Positive(LCD_Natural_Width);
   procedure Set_Y (This : in out Game_Paddle; Y : Positive)
      with Pre => Y < Positive(LCD_Natural_Height);
   procedure Set_Width (This : in out Game_Paddle; W : Positive)
      with Pre => W >= 1 and W < LCD_Natural_Width;

   procedure Update_Paddle (This : in out Game_Paddle);

private
   type Game_Paddle is record
      X : Positive := Positive(LCD_Natural_Width / 2);
      Y : Positive := 20;
      Width : Positive := 50;
   end record;
end Paddle;
