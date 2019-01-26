with STM32.Board; use STM32.Board;
with STM32.Device_Id; use STM32.Device_Id;
with Ball_Package; use Ball_Package;
with Peripherals_Nonblocking; use Peripherals_Nonblocking;
with Serial_IO.Nonblocking; use Serial_IO.Nonblocking;
with Message_Buffers; use Message_Buffers;

package Communication is

   type Status_Message is tagged private;

   procedure Initialize_Communication
      with Post => Initialized (COM);
   procedure Send_Message (M : String)
      with Pre => Initialized (COM);
   procedure Send_Debug (M : String)
      with Pre => Initialized (COM);
   function Determine_Player_Number return Integer
      with Pre => Initialized (COM);

private
   type Status_Message is tagged record
      Ball_Data : Ball;
      Score_1 : Natural;
      Score_2 : Natural;
   end record;
end Communication;
