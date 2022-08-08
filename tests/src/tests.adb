pragma Assertion_Policy (Check);

with Ada.Text_IO; use Ada.Text_IO;

with Littlefs; use Littlefs;
with RAM_BD;
with Interfaces;   use Interfaces;
with Interfaces.C; use Interfaces.C;
with System.Storage_Elements; use System.Storage_Elements;

procedure Tests is

   FS    : aliased LFS_T;
   Block : constant access constant LFS_Config := RAM_BD.Create (2048 * 200);

   procedure Create_File (Path : String);
   procedure Read_File (Path : String);
   procedure Tree (Path : String);

   -----------------
   -- Create_File --
   -----------------

   procedure Create_File (Path : String) is
      FD : aliased LFS_File;
      Data : Storage_Array (1 .. 42);
   begin
      pragma Assert (Open (FS, FD, Path, LFS_O_CREAT + LFS_O_RDWR) = 0);

      Data := (others => 42);

      pragma Assert (Write (FS, FD, Data (Data'First)'Address,
                     Data'Length) = Data'Length);

      pragma Assert (Close (FS, FD) = 0);
   end Create_File;

   ---------------
   -- Read_File --
   ---------------

   procedure Read_File (Path : String) is
      FD : aliased LFS_File;
      Data : Storage_Array (1 .. 42);
   begin
      pragma Assert (Open (FS, FD, Path, LFS_O_RDONLY) = 0);

      pragma Assert (Read (FS, FD, Data (Data'First)'Address,
                     Data'Length + 20) = Data'Length);

      for Elt of Data loop
         pragma Assert (Elt = 42);
      end loop;

      pragma Assert (Close (FS, FD) = 0);
   end Read_File;

   ----------
   -- Tree --
   ----------

   procedure Tree (Path : String) is
      Dir : aliased LFS_Dir;
      Err : int;
      Info : aliased Entry_Info;
   begin
      Err := Open (FS, Dir, Path);

      if Err = 0 then
         while Read (FS, Dir, Info) > 0 loop
            declare
               Name : constant String := Littlefs.Name (Info);
               Sub  : constant String := (if Path = "/"
                                          then "/" & Name
                                          else Path & "/" & Name);
            begin
               if Name /= "." and then Name /= ".." then
                  Put_Line (Sub);
                  if Kind (Info) = Directory then
                     Tree (Sub);
                  end if;
               end if;
            end;
         end loop;
         Err := Close (FS, Dir);
      end if;
   end Tree;

begin

   declare
      function Config_Size return int;
      pragma Import (C, Config_Size, "config_size");
      function Info_Size return int;
      pragma Import (C, Info_Size, "info_size");
      function Attr_Size return int;
      pragma Import (C, Attr_Size, "attr_size");
      function File_Config_Size return int;
      pragma Import (C, File_Config_Size, "file_config_size");
      function Dir_Size return int;
      pragma Import (C, Dir_Size, "dir_size");
      function File_Size return int;
      pragma Import (C, File_Size, "file_size");
      function LFS_Size return int;
      pragma Import (C, LFS_Size, "lfs_size");

      procedure My_Assert (A, B : int; Msg : String) is
      begin
         Ada.Text_IO.Put_Line ("Checking size of " & Msg & " (size in C:" &
                                 A'Img & ", Size in Ada:" & B'Img & ")");
         pragma Assert (A = B, Msg & " size mismatch");
      end My_Assert;
   begin
      My_Assert (Info_Size, Entry_Info'Size / 8, "Entry_Info");
      My_Assert (Attr_Size, lfs_attr'Size / 8, "lfs_attr");
      My_Assert (File_Config_Size, lfs_file_config'Size / 8,
                 "lfs_file_config");
      My_Assert (Dir_Size, LFS_Dir'Size / 8, "LFS_Dir");
      My_Assert (File_Size, LFS_File'Size / 8, "LFS_File");
      My_Assert (Config_Size, LFS_Config'Size / 8, "LFS_Config");
      My_Assert (LFS_Size, LFS_T'Size / 8, "LFS_T");
   end;

   pragma Assert (Format (FS, Block.all) = 0);
   pragma Assert (Mount (FS, Block.all) = 0);

   pragma Assert (Mkdir (FS, "/dir1") = 0);
   pragma Assert (Mkdir (FS, "/dir2") = 0);
   pragma Assert (Mkdir (FS, "/dir1/sub1") = 0);
   pragma Assert (Mkdir (FS, "/dir1/sub2") = 0);
   pragma Assert (Mkdir (FS, "/dir1/sub2/subsub1") = 0);

   Create_File ("/test1.txt");
   Create_File ("/test2.txt");
   Create_File ("/test3.txt");
   Create_File ("/dir1/test1.txt");
   Create_File ("/dir1/sub2/subsub1/test1.txt");

   Read_File ("/test1.txt");
   Read_File ("/test2.txt");
   Read_File ("/test3.txt");
   Read_File ("/dir1/test1.txt");
   Read_File ("/dir1/sub2/subsub1/test1.txt");

   declare
      FD : aliased LFS_File;
   begin
      pragma Assert
        (Open (FS, FD, "/doesnt_exists", LFS_O_RDONLY) = LFS_ERR_NOENT);
      pragma Assert
        (Open (FS, FD, "/dir1/doesnt_exists", LFS_O_RDONLY) = LFS_ERR_NOENT);
   end;
   Tree ("/");
end Tests;
