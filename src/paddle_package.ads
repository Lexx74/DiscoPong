with STM32.Board; use STM32.Board;
with HAL.Touch_Panel; use HAL.Touch_Panel;

package Paddle_Package is
   type Paddle is tagged private;

   Paddle_Default_Y : constant Natural := 100;
   Paddle_Default_Width : constant Natural := 50;
   Paddle_Default_Thickness : constant Natural := 6;

   function Get_Min_X (This : Paddle) return Natural;
   function Get_Max_X (This : Paddle) return Natural;

   function Get_X (This : Paddle) return Natural;
   function Get_Y (This : Paddle) return Natural;
   function Get_Width (This : Paddle) return Natural;
   function Get_Thickness (This : Paddle) return Natural;

   function Get_High_Edge_X (This : Paddle) return Natural
      with Post => Get_High_Edge_X'Result <= LCD_Natural_Width;
   function Get_Low_Edge_X (This : Paddle) return Natural
      with Post => Get_Low_Edge_X'Result <= LCD_Natural_Width;

   procedure Set_X (This : in out Paddle; X : Natural)
      with Post => (This.Get_X >= This.Get_Min_X and This.Get_X <= This.Get_Max_X);

   procedure Update (This : in out Paddle);
   procedure Draw (This : Paddle);

private
   type Paddle is tagged record
      X : Natural := Natural(LCD_Natural_Width / 2);
      Y : Natural := Paddle_Default_Y;
      Width : Natural := Paddle_Default_Width;
      Thickness : Natural := Paddle_Default_Thickness;
   end record;
end Paddle_Package;
