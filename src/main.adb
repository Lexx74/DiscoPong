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
with Calculus;                  use Calculus;
with Paddle; use Paddle;
with Game_Display; use Game_Display;

procedure Main
is
   LCD_Natural_Width_f : Float := Float(LCD_Natural_Width);
   LCD_Natural_Height_f : Float := Float(LCD_Natural_Height);

   BG_Color : Bitmap_Color := (Alpha => 255, others => 0);
   Ball_Pos   : My_Vector := (Float(LCD_Natural_Width / 2), Float(15));
   Ball_Direction: My_Vector := (2.0, -2.0);
   radius : Float := 10.0;
   normalAngle : Float;
   ballAngle : Float;
   newBallAngle : Float;
   norm : Float;

   Pad : Game_Paddle;
begin

   --  Initialize LCD
   Display.Initialize;
   Display.Initialize_Layer (1, ARGB_8888);

   --  Initialize touch panel
   Touch_Panel.Initialize;

   --  Initialize button
   User_Button.Initialize;

   LCD_Std_Out.Set_Font (BMP_Fonts.Font8x8);
   LCD_Std_Out.Current_Background_Color := BG_Color;

   --  Clear LCD (set background)
   Draw_Background (BG_Color);

   LCD_Std_Out.Clear_Screen;
   Display.Update_Layer (1, Copy_Back => True);

   loop
      if User_Button.Has_Been_Pressed then
         BG_Color := HAL.Bitmap.Dark_Orange;
      end if;

      Draw_Background (BG_Color);
      Display.Hidden_Buffer (1).Set_Source (HAL.Bitmap.Blue);
      Display.Hidden_Buffer (1).Fill_Circle (Vector_To_Point(Ball_Pos), Integer(radius));

      Update_Paddle (Pad);
      Draw_Paddle (Pad);

      -- TODO: Read voltage from pin

      -- Ball_Pos.x := (Ball_Pos.x + Ball_Direction.x);
      -- if (Ball_Pos.x + radius > LCD_Natural_Width_f) then
      --    Ball_Direction.X := - Ball_Direction.X;
      --    Ball_Pos.x := LCD_Natural_Width_f - (Ball_Pos.x + radius - LCD_Natural_Width_f) - radius;
      -- end if;
      -- if (Ball_Pos.x < radius) then
      --    Ball_Direction.X := - Ball_Direction.X;
      --    Ball_Pos.x := radius + (radius - Ball_Pos.X);
      -- end if;
      -- 
      -- Ball_Pos.y := (Ball_Pos.y + Ball_Direction.Y);
      -- if (Ball_Pos.y + radius > LCD_Natural_Height_f) then
      --    Ball_Direction.Y := - Ball_Direction.Y;
      --    Ball_Pos.y := LCD_Natural_Height_f - (Ball_Pos.y + radius - LCD_Natural_Height_f) - radius;
      -- end if;
      -- if (Ball_Pos.y < radius and then Ball_Direction.y < 0.0) then
      --    --Ball_Direction.y := - Ball_Direction.y;
      --    --Ball_Pos.y := radius + (radius - Ball_Pos.y);
      --    norm := Calculate_Norm(Ball_Direction);
      --    normalAngle := Calculate_Normal_Angle(Integer(Ball_Pos.X));
      --    ballAngle := Vector_To_Angle(Ball_Direction);
      --    newBallAngle := normalAngle + (normalAngle - ballAngle);
      --    Ball_Direction := Angle_To_Direction(newBallAngle);
      --    Mult_Vector(Ball_Direction, norm);
      -- end if;
      
      --  Update screen
      Display.Update_Layer (1, Copy_Back => True);

   end loop;
end Main;
