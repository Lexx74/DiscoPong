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

procedure Main
is
   LCD_Natural_Width_f : Float := Float(LCD_Natural_Width);
   LCD_Natural_Height_f : Float := Float(LCD_Natural_Height);
   BG_Color : Bitmap_Color := (Alpha => 255, others => 0);

   Ball : Local_Ball;
   GBall : Global_Ball;
   Pad : Paddle;
   Player_No : Integer;

   Game_Status : Status_Message;
   Has_Ball : Boolean;

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

   -- Initialize Coms
   Initialize_Communication;
   Player_No := Determine_Player_Number;

   --  Clear LCD (set background)
   Draw_Background (BG_Color);

   LCD_Std_Out.Clear_Screen;
   Display.Update_Layer (1, Copy_Back => True);

   Has_Ball := Player_No = 1;

   Game_Status.Scores(1) := 0;
   Game_Status.Scores(2) := 0;

   loop
      Draw_Background (BG_Color);
      if Player_No = 1 then
         Display.Hidden_Buffer (1).Set_Source (HAL.Bitmap.Blue);
      else
         Display.Hidden_Buffer (1).Set_Source (HAL.Bitmap.Red);
      end if;
      if Has_Ball then
         Ball.Update(Pad, Game_Status.Scores(3 - Player_No));
         Local_To_Global(Ball, GBall, Player_No);
         Game_Status.Ball_Data := GBall;
         Send_Status_Message (Game_Status);
         if not Do_I_Have_Ball(GBall, Player_No) then
            for i in 1 .. 2 loop
               Send_Status_Message (Game_Status);
               delay 0.01;
            end loop;
         end if;
      else
         while not Receive_Status_Message(Game_Status) loop
            null;
         end loop;
         GBall := Game_Status.Ball_Data;
         Global_To_Local(GBall, Ball, Player_No);

      end if;

      Has_Ball := Do_I_Have_Ball(GBall, Player_No);

      Pad.Update;
      Pad.Draw;

      Ball.Draw;

      Draw_Score(Game_Status.Scores(Player_No));

      --  Update screen
      Display.Update_Layer (1, Copy_Back => True);

   end loop;
end Main;
