with Ada.Text_IO; use Ada.Text_IO;

with Simple_Blockchain;

function Simple_Blockchain_Demo return Integer is
    package Blockchain is new Simple_Blockchain(Difficulty => 6);
begin
   Put_Line ("Simple blockchain demo");
   New_Line;

   Put_Line ("Mining first block...");
   Blockchain.Mine_Block (Data => "First block");
   Put_Line ("Block mined.");
   New_Line;

   Put_Line ("Mining second block...");
   Blockchain.Mine_Block (Data => "Second block");
   Put_Line ("Block mined.");
   New_Line;

   Put_Line ("Mining third block...");
   Blockchain.Mine_Block (Data => "Third block");
   Put_Line ("Block mined.");
   New_Line;

   Put ("Is blockchain valid? ");
   Put_Line (Blockchain.Is_Valid'Img);
   New_Line;

   Put_Line ("Printing blockchain...");
   Blockchain.Print_Blockchain;

   return 0;
end Simple_Blockchain_Demo;
