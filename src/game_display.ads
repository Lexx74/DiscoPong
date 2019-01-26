with Paddle_Package; use Paddle_Package;
with STM32.Board; use STM32.Board;
with HAL.Bitmap; use HAL.Bitmap;

package Game_Display is
   procedure Draw_Background (Color : Bitmap_Color)
      with Pre => Display.Initialized;
end Game_Display;
