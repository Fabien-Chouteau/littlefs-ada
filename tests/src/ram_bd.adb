with Littlefs; use Littlefs;
with System.Storage_Elements; use System.Storage_Elements;

with Interfaces.C; use Interfaces.C;
with Interfaces; use Interfaces;

with System;

package body RAM_BD is

   function Read (C      : access constant LFS_Config;
                  Block  : LFS_Block;
                  Off    : LFS_Offset;
                  Buffer : System.Address;
                  Size   : LFS_Size)
                  return int
     with Convention => C;

   function Prog (C      : access constant LFS_Config;
                  Block  : LFS_Block;
                  Off    : LFS_Offset;
                  Buffer : System.Address;
                  Size   : LFS_Size)
                  return int
     with Convention => C;
   function Erase (C     : access constant LFS_Config;
                   Block : LFS_Block)
                   return int
     with Convention => C;
   function Sync (C : access constant LFS_Config) return int
     with Convention => C;

   ----------
   -- Read --
   ----------

   function Read (C      : access constant LFS_Config;
                  Block  : LFS_Block;
                  Off    : LFS_Offset;
                  Buffer : System.Address;
                  Size   : LFS_Size)
                  return int
   is
      Offset : constant LFS_Offset := Off + C.Block_Size * LFS_Size (Block);

      Dev_Buf : Storage_Array (1 .. Storage_Offset (Size))
        with Address => To_Address
          (To_Integer (C.Context) + Integer_Address (Offset));

      Read_Buf : Storage_Array (1 .. Storage_Offset (Size))
        with Address => Buffer;
   begin
      Read_Buf := Dev_Buf;
      return 0;
   end Read;

   function Prog (C      : access constant LFS_Config;
                  Block  : LFS_Block;
                  Off    : LFS_Offset;
                  Buffer : System.Address;
                  Size   : LFS_Size)
                  return int
   is
      Offset : constant LFS_Offset := Off + C.Block_Size * LFS_Size (Block);

      Dev_Buf : Storage_Array (1 .. Storage_Offset (Size))
        with Address => To_Address
          (To_Integer (C.Context) + Integer_Address (Offset));

      Read_Buf : Storage_Array (1 .. Storage_Offset (Size))
        with Address => Buffer;
   begin
      Dev_Buf := Read_Buf;
      return 0;
   end Prog;

   function Erase (C : access constant LFS_Config;
                   Block : LFS_Block)
                   return int
   is
      pragma Unreferenced (Block, C);
   begin
      return 0;
   end Erase;

   function Sync (C : access constant LFS_Config) return int is
      pragma Unreferenced (C);
   begin
      return 0;
   end Sync;

   type LFS_Config_Access is access all Littlefs.LFS_Config;
   type Storage_Array_Access is access all Storage_Array;

   ------------
   -- Create --
   ------------

   function Create (Size : Littlefs.LFS_Size)
                    return access constant Littlefs.LFS_Config
   is
      Ret : constant LFS_Config_Access := new LFS_Config;
      Buf : constant Storage_Array_Access :=
        new Storage_Array (1 .. Storage_Offset (Size));
   begin
      Ret.Context := Buf (Buf'First)'Address;
      Ret.Read := Read'Access;
      Ret.Prog := Prog'Access;
      Ret.Erase := Erase'Access;
      Ret.Sync := Sync'Access;
      Ret.Read_Size := 2048;
      Ret.Prog_Size := 2048;
      Ret.Block_Size := 2048;
      Ret.Block_Count := Size / 2048;
      Ret.Block_Cycles := 700;
      Ret.Cache_Size := Ret.Block_Size;
      Ret.Lookahead_Size := Ret.Block_Size;
      Ret.Read_Buffer := System.Null_Address;
      Ret.Prog_Buffer := System.Null_Address;
      Ret.Lookahead_Buffer := System.Null_Address;
      Ret.Name_Max := 0;
      Ret.File_Max := 0;
      Ret.Attr_Max := 0;
      return Ret;
   end Create;

end RAM_BD;
