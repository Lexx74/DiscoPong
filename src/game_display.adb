package body Game_Display is

   procedure Draw_Background (Color : Bitmap_Color) is
   begin
      Display.Hidden_Buffer(1).Set_Source(Color);
      Display.Hidden_Buffer(1).Fill;
   end Draw_Background;
end Game_Display;
