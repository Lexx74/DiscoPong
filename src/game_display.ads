with Paddle; use Paddle;
with STM32.Board; use STM32.Board;
with HAL.Bitmap; use HAL.Bitmap;

package Game_Display is
   procedure Draw_Background (Color : Bitmap_Color)
      with Pre => Display.Initialized;
   procedure Draw_Paddle (P : Game_Paddle)
      with Pre => Display.Initialized;
   -- TODO: Create Ball class and add the parameter
   procedure Draw_Ball
      with Pre => Display.Initialized;
end Game_Display;
