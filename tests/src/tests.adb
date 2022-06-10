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
