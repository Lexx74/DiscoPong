package body Game_Display is

   procedure Draw_Background (Color : Bitmap_Color) is
   begin
      Display.Hidden_Buffer(1).Set_Source(Color);
      Display.Hidden_Buffer(1).Fill;
   end Draw_Background;

   procedure Draw_Paddle(P : Game_Paddle) is
      Start_X : Positive;
      Start_Y : Positive := Paddle_Default_Y;
      Max_X : constant Positive := LCD_Natural_Width - Get_Width (P) / 2;
      Min_X : constant Positive := Get_Width (P) / 2 + 1;
   begin
      if Get_X (P) <= Min_X then
         Start_X := 1;
      elsif Get_X (P) >= Max_X then
         Start_X := LCD_Natural_Width - Get_Width (P);
      else
         Start_X := Get_X (P) - Get_Width (P) / 2;
      end if;

      Display.Hidden_Buffer(1).Draw_Line(Start => (Start_X, Start_Y),
                                         Stop  => (Start_X + Get_Width (P), Start_Y),
                                         Thickness => 6);
   end Draw_Paddle;

   procedure Draw_Ball is
   begin
      -- FIXME
      null;
   end Draw_Ball;

end Game_Display;
