------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2016, AdaCore                     --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--     1. Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--     2. Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--     3. Neither the name of the copyright holder nor the names of its     --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS    --
--   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT      --
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR  --
--   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   --
--   HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, --
--   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT       --
--   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,  --
--   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY  --
--   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT    --
--   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE  --
--   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   --
--                                                                          --
------------------------------------------------------------------------------

with Last_Chance_Handler;  pragma Unreferenced (Last_Chance_Handler);
--  The "last chance handler" is the user-defined routine that is called when
--  an exception is propagated. We need it in the executable, therefore it
--  must be somewhere in the closure of the context clauses.

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

   Ball_Status : Status_Message;
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


   loop
      Draw_Background (BG_Color);
      if Player_No = 1 then
         Display.Hidden_Buffer (1).Set_Source (HAL.Bitmap.Blue);
      else
         Display.Hidden_Buffer (1).Set_Source (HAL.Bitmap.Red);
      end if;
      if Has_Ball then
         Ball.Update(Pad);
         Local_To_Global(Ball, GBall, Player_No);
         Ball_Status.Ball_Data := GBall;
         Send_Status_Message (Ball_Status);
      else
         Ball_Status := Receive_Status_Message;
         GBall := Ball_Status.Ball_Data;
         Global_To_Local(GBall, Ball, Player_No);
      end if;

      Has_Ball := Do_I_Have_Ball(GBall, Player_No);

      Pad.Update;
      Pad.Draw;

      Ball.Draw;

      --  Update screen
      Display.Update_Layer (1, Copy_Back => True);

   end loop;
exception
   when E : others =>
      Send_Debug (Exception_Message (E));
end Main;
