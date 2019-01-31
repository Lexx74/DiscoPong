with Last_Chance_Handler;  pragma Unreferenced (Last_Chance_Handler);

with STM32.Board;           use STM32.Board;
with HAL.Bitmap;            use HAL.Bitmap;
pragma Warnings (Off, "referenced");
with HAL.Touch_Panel;       use HAL.Touch_Panel;
with STM32.User_Button;     use STM32;
with BMP_Fonts;
with LCD_Std_Out;
with Calculus; use Calculus;
with Ball_Package; use Ball_Package;
with Paddle_Package; use Paddle_Package;
with Game_Display; use Game_Display;
with Communication; use Communication;
with Ada.Exceptions;  use Ada.Exceptions;

procedure Tests
is
   LCD_Natural_Width_f : Float := Float(LCD_Natural_Width);
   LCD_Natural_Height_f : Float := Float(LCD_Natural_Height);
   BG_Color : Bitmap_Color := (Alpha => 255, others => 0);

   Ball : Local_Ball;
   GBall : Global_Ball;
   Pad : Paddle;
begin

   --  Initialize LCD
   Display.Initialize;
   Display.Initialize_Layer (1, ARGB_8888);

   --  Initialize touch panel
   Touch_Panel.Initialize;

   --  Initialize button
   User_Button.Initialize;

   LCD_Std_Out.Set_Font (BMP_Fonts.Font16x24);
   LCD_Std_Out.Current_Background_Color := BG_Color;

   --  Clear LCD (set background)
   Draw_Background (BG_Color);

   LCD_Std_Out.Clear_Screen;

   -- Test suite

   LCD_Std_Out.Put_Line("All tests done");

   loop
      exit when 0 = 1;
   end loop;

end Tests;
