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

   -- Ball tests
   Ball.Pos := (Radius, 250.0);
   Ball.Direction := (-1.0, 0.0);
   Update(Ball, Pad);
   if Ball.Pos.X /= Radius + 1.0 or else Ball.Pos.Y /= 250.0
      or else Ball.Direction.X /= 1.0 or else Ball.Direction.Y /= 0.0 then
      LCD_Std_Out.Put_Line("Error Ball update bounce left");
   end if;

   Ball.Pos := (LCD_Natural_Width_f - Radius, 250.0);
   Ball.Direction := (1.0, 0.0);
   Update(Ball, Pad);
   if Ball.Pos.X /= LCD_Natural_Width_f - Radius - 1.0 or else Ball.Pos.Y /= 250.0
      or else Ball.Direction.X /= -1.0 or else Ball.Direction.Y /= 0.0 then
      LCD_Std_Out.Put_Line("Error Ball update bounce right");
   end if;

   Ball.Pos := (LCD_Natural_Width_f / 2.0, Float(Paddle_Default_Y) + Radius);
   Ball.Direction := (0.0, -1.0);
   Update(Ball, Pad);
   if Ball.Pos.X /= LCD_Natural_Width_f / 2.0
      or else Ball.Pos.Y /= Float(Paddle_Default_Y) + Radius + 1.0
      or else Ball.Direction.X /= 0.0 or else Ball.Direction.Y /= 1.0 then
      LCD_Std_Out.Put_Line("Error Ball update bounce paddle");
   end if;

   -- Paddle tests
   Set_X(Pad, 0);
   if Get_X(Pad) /= Get_Min_X(Pad) then
     LCD_Std_Out.Put_Line("Error Pad left");
   end if;
   Set_X(Pad, LCD_Natural_Width);
   if Get_X(Pad) /= Get_Max_X(Pad) then
     LCD_Std_Out.Put_Line("Error Pad right");
   end if;

   --LCD_Std_Out.Put_Line(Ball.Pos.X'Image);
   --LCD_Std_Out.Put_Line(Ball.Pos.Y'Image);
   --LCD_Std_Out.Put_Line(Ball.Direction.X'Image);
   --LCD_Std_Out.Put_Line(Ball.Direction.Y'Image);

   LCD_Std_Out.Put_Line("All tests done");

   loop
      exit when 0 = 1;
   end loop;

end Tests;
