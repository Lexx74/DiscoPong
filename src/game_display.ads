with Paddle_Package; use Paddle_Package;
with STM32.Board; use STM32.Board;
with HAL.Bitmap; use HAL.Bitmap;
with LCD_Std_Out;

package Game_Display is
   procedure Draw_Background (Color : Bitmap_Color)
      with Pre => Display.Initialized;

   procedure Draw_Score (Score : Natural)
      with Pre => Display.Initialized;

   procedure Draw_Victory
      with Pre => Display.Initialized;
   procedure Draw_Lose
      with Pre => Display.Initialized;
end Game_Display;
