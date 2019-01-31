with Ada.Exceptions;  use Ada.Exceptions;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Communication is
   CR : constant Character := Character'Val(13);
   LF : constant Character := Character'Val(10);

   CRLF : constant String := CR & LF;

   procedure Initialize_Communication is
   begin
      Enable_Clock (Transceiver);
      Enable_Clock (RX_Pin & TX_Pin);

      Configure_IO
        (RX_Pin & TX_Pin,
         (Mode           => Mode_AF,
          AF             => Transceiver_AF,
          Resistors      => Pull_Up,
          AF_Output_Type => Push_Pull,
          AF_Speed       => Speed_50MHz));

      Disable (Transceiver);

      Set_Baud_Rate    (Transceiver, 115_200);
      Set_Mode         (Transceiver, Tx_Rx_Mode);
      Set_Stop_Bits    (Transceiver, Stopbits_1);
      Set_Word_Length  (Transceiver, Word_Length_8);
      Set_Parity       (Transceiver, No_Parity);
      Set_Flow_Control (Transceiver, No_Flow_Control);

      Enable (Transceiver);

   end Initialize_Communication;

   procedure Send_Message (M : String) is
      Buf : aliased Message (Physical_Size => 1024);
   begin
      Set(Buf, To => M & Terminator);
      Peripherals.COM.Start_Sending(Buf'Unchecked_Access);
      Suspend_Until_True (Buf.Transmission_Complete);
   end Send_Message;

   procedure Send_Status_Message (M : Status_Message) is
   begin
      Send_Message ("                     " &
                    M.Ball_Data.Pos.X'Image & Separator &
                    M.Ball_Data.Pos.Y'Image & Separator &
                    M.Ball_Data.Direction.X'Image & Separator &
                    M.Ball_Data.Direction.Y'Image & Separator &
                    M.Scores(1)'Image & Separator &
                    M.Scores(2)'Image);
   end Send_Status_Message;

   procedure Skip_Next_Character(Recv_Buf : Message; I : in out Integer; First : in out Integer; Last : in out Integer; C : Character) is
   begin
      I := I + 1;
      First := I;
      while I <= Recv_Buf.Logical_Size and then Recv_Buf.Content(I) /= C loop
         I := I + 1;
      end loop;
      Last := I - 1;
   end;

   function Receive_Status_Message(Ret : out Status_Message) return Boolean is
      Recv_Buf : aliased Message (Physical_Size => 1024);
      Reached_Term : Boolean := False;
      First : Integer := 0;
      Last : Integer := 1;
      I : Integer := 1;
   begin
      Recv_Buf.Terminator := Terminator;
      Peripherals.COM.Start_Receiving (Recv_Buf'Unchecked_Access);
      Suspend_Until_True (Recv_Buf.Reception_Complete);

      Skip_Next_Character(Recv_Buf, I, First, Last, Separator);
      Ret.Ball_Data.Pos.X := Float'Value(Recv_Buf.Content (First .. Last));

      Skip_Next_Character(Recv_Buf, I, First, Last, Separator);
      Ret.Ball_Data.Pos.Y := Float'Value(Recv_Buf.Content (First .. Last));

      Skip_Next_Character(Recv_Buf, I, First, Last, Separator);
      Ret.Ball_Data.Direction.X := Float'Value(Recv_Buf.Content (First .. Last));

      Skip_Next_Character(Recv_Buf, I, First, Last, Separator);
      Ret.Ball_Data.Direction.Y := Float'Value(Recv_Buf.Content (First .. Last));

      Skip_Next_Character(Recv_Buf, I, First, Last, Separator);
      Ret.Scores(1) := Game.Score'Value(Recv_Buf.Content (First .. Last));

      Skip_Next_Character(Recv_Buf, I, First, Last, Terminator);
      Ret.Scores(2) := Game.Score'Value(Recv_Buf.Content (First .. Last));

      return Ret.Ball_Data.Pos.X in X_Range and then Ret.Ball_Data.Pos.Y in Y_Global_Range;

   exception
      when E : others =>
         return False;
   end Receive_Status_Message;

   function Determine_Player_Number return Integer is

      subtype Identity_String is String (1..30);

      function Device_Id_to_Identity(Tuple : Device_Id_Tuple) return Identity_String is
         subtype Identity_Number is String (1..10);

         Ids : array (1..3) of Identity_Number;
      begin
         for I in Tuple'Range loop
            Move(Trim(Tuple(I)'Image, Ada.Strings.Left),
                 Ids(I), Justify => Right, Pad => '0');
         end loop;

         return Ids(1) & Ids(2) & Ids(3);
      end Device_Id_to_Identity;

      Own_Id : Identity_String := Device_Id_to_Identity(Unique_Id);
      Foreign_Id : Identity_String;
      Recv_Buf : aliased Message (Physical_Size => 1024);
      Received_Ack : Boolean := False;
      Received_Id : Boolean := False;
      I : Integer := 0;
   begin
      Recv_Buf.Terminator := Terminator;
      Peripherals.COM.Start_Receiving (Recv_Buf'Unchecked_Access);

      while not (Received_Ack and Received_Id) loop
         if (Received_Id) then
            Send_Message (Own_Id & "ACK");
         else
            Send_Message (Own_Id);
         end if;

         Suspend_Until_True (Recv_Buf.Reception_Complete);
         if Recv_Buf.Logical_Size = Own_Id'Length + 4 then
            Foreign_Id := Recv_Buf.Content(Foreign_Id'Range);
            Received_Id := True;
            Send_Message (Own_Id & "ACK");
            Received_Ack := True;
         elsif Recv_Buf.Logical_Size = Own_Id'Length + 1 then
            Foreign_Id := Recv_Buf.Content(Foreign_Id'Range);
            Received_Id := True;
         end if;
         Peripherals.COM.Start_Receiving (Recv_Buf'Unchecked_Access);
         delay 0.5;
      end loop;
      for I in Own_Id'Range loop
         if Own_Id(I) /= Foreign_Id(I) then
            if Own_Id(I) < Foreign_Id(I) then
               return 1;
            else
               return 2;
            end if;
         end if;
      end loop;

      return 0;
   end Determine_Player_Number;
end Communication;
