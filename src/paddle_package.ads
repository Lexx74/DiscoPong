with STM32.Board; use STM32.Board;
with HAL.Touch_Panel; use HAL.Touch_Panel;

package Paddle_Package is
   type Paddle is tagged private;

   Paddle_Default_Y : constant Natural := 10;
   Paddle_Default_Width : constant Natural := 50;

   function Get_Min_X (This : Paddle) return Natural;
   function Get_Max_X (This : Paddle) return Natural;

   function Get_X (This : Paddle) return Natural;
   function Get_Y (This : Paddle) return Natural;
   function Get_Width (This : Paddle) return Natural;

   procedure Set_X (This : in out Paddle; X : Natural)
      with Pre => X < Natural(LCD_Natural_Width),
           Post => X >= Get_Min_X (This) and X <= Get_Max_X (This);

   procedure Update_Paddle (This : in out Paddle);

private
   type Paddle is tagged record
      X : Natural := Natural(LCD_Natural_Width / 2);
      Y : Natural := Paddle_Default_Y;
      Width : Natural := Paddle_Default_Width;
   end record;
end Paddle_Package;
