with STM32.Board; use STM32.Board;
with STM32.Device_Id; use STM32.Device_Id;
with Ball_Package; use Ball_Package;
with Peripherals; use Peripherals;
with ball_package; use ball_package;
with Game;

with STM32;                        use STM32;
with STM32.GPIO;                   use STM32.GPIO;
with STM32.USARTs;                 use STM32.USARTs;

with STM32.Device;                 use STM32.Device;

with Ada.Synchronous_Task_Control; use Ada.Synchronous_Task_Control;

with Peripherals;                  use Peripherals;
with Serial_Port;                  use Serial_Port;


package Communication is

   type Status_Message is tagged record
      Ball_Data : Ball;
      Scores : Game.Scores;
   end record;

   Terminator : constant Character := ';';
   Separator : constant Character := '#';

   procedure Initialize_Communication;
   procedure Send_Message (M : String);
   procedure Send_Status_Message (M : Status_Message);
   function Receive_Status_Message(Ret : out Status_Message) return Boolean;
   function Determine_Player_Number return Integer;

   Same_Board_Id : exception;
   Connection_Timeout : exception;

end Communication;
