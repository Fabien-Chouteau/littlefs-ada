package body Littlefs is

   ------------
   -- C_Type --
   ------------

   function Kind (Info : Entry_Info) return File_Kind
   is (File_Kind'Enum_Val (Info.c_type));

   ----------
   -- Size --
   ----------

   function Size (Info : Entry_Info) return LFS_Size
   is (Info.size);

   ----------
   -- Name --
   ----------

   function Name (Info : Entry_Info) return String is

      Count : Natural;
      From  : Positive := Info.name'First;
   begin
      while Info.name (From) /= ASCII.NUL loop
         From := From + 1;
      end loop;

      Count := Natural (From) - Natural (Info.name'First);

      return Info.name (Info.name'First .. Info.name'First + Count - 1);
   end Name;

   ------------
   -- Format --
   ------------

   function Format
     (LFS : aliased in out LFS_T;
      Config : aliased LFS_Config)
      return int
   is
      function Format (lfs : access LFS_T;
                       config : access constant LFS_Config)
                       return int
        with Import => True, Convention => C, External_Name => "lfs_format";

   begin
      return Format (LFS'Access, Config'Access);
   end Format;

   -----------
   -- Mount --
   -----------

   function Mount
     (LFS : aliased in out LFS_T; Config : aliased LFS_Config) return int
   is
      function Mount (lfs : access LFS_T;
                      config : access constant LFS_Config)
                      return int
        with Import => True, Convention => C, External_Name => "lfs_mount";
   begin
      return Mount (LFS'Access, Config'Access);
   end Mount;

   -------------
   -- Unmount --
   -------------

   function Unmount (LFS : aliased in out LFS_T) return int is
      function Unmount (lfs : access LFS_T) return int  -- lfs.h:443
        with Import => True,  Convention => C, External_Name => "lfs_unmount";
   begin
      return Unmount (LFS'Access);
   end Unmount;

   ------------
   -- Remove --
   ------------

   function Remove (LFS : aliased in out LFS_T; Path : String) return int is
      function Remove (lfs : access LFS_T;
                       path : System.Address)
                       return int  -- lfs.h:452
        with Import => True, Convention => C, External_Name => "lfs_remove";

      C_Path : constant String (1 .. Path'Length + 1) :=
        Path & ASCII.NUL;

   begin
      return Remove (LFS'Access, C_Path'Address);
   end Remove;

   ------------
   -- Rename --
   ------------

   function Rename
     (LFS : aliased in out LFS_T; Oldpath : String; Newpath : String)
      return int
   is
      function Rename (lfs : access LFS_T;
                       oldpath : System.Address;
                       newpath : System.Address)
                       return int
        with Import => True, Convention => C, External_Name => "lfs_rename";

      C_Oldpath : constant String (1 .. Oldpath'Length + 1) :=
        Oldpath & ASCII.NUL;
      C_Newpath : constant String (1 .. Newpath'Length + 1) :=
        Newpath & ASCII.NUL;

   begin
      return Rename (LFS'Access, C_Oldpath'Address, C_Newpath'Address);
   end Rename;

   ----------
   -- Stat --
   ----------

   function Stat
     (LFS : aliased in out LFS_T; Path : String; Info : aliased out Entry_Info)
      return int
   is
      function Stat (lfs : access LFS_T;
                     path : System.Address;
                     info : access Entry_Info)
                     return int  -- lfs.h:469
        with Import => True, Convention => C, External_Name => "lfs_stat";

      C_Path : constant String (1 .. Path'Length + 1) :=
        Path & ASCII.NUL;

   begin
      return Stat (LFS'Access, C_Path'Address, Info'Access);
   end Stat;

   -------------
   -- Setattr --
   -------------

   function Setattr (LFS    : aliased in out LFS_T;
                     Path   : String;
                     Id     : Interfaces.Unsigned_8;
                     Buffer : System.Address;
                     Size   : LFS_Size)
                     return int
   is
      function Setattr (lfs : access LFS_T;
                        path : System.Address;
                        c_type : Interfaces.Unsigned_8;
                        buffer : System.Address;
                        size : LFS_Size)
                        return int  -- lfs.h:494
        with Import => True, Convention => C, External_Name => "lfs_setattr";

      C_Path : constant String (1 .. Path'Length + 1) :=
        Path & ASCII.NUL;
   begin
      return Setattr (LFS'Access, C_Path'Address, Id, Buffer, Size);
   end Setattr;

   ----------------
   -- Removeattr --
   ----------------

   function Removeattr (LFS  : aliased in out LFS_T;
                        Path :                String;
                        Id   :                Interfaces.Unsigned_8)
                        return int
   is
      function Removeattr (lfs : access LFS_T;
                           path : System.Address;
                           c_type : Interfaces.Unsigned_8)
                           return int  -- lfs.h:504
        with Import => True, Convention => C,
        External_Name => "lfs_removeattr";

      C_Path : constant String (1 .. Path'Length + 1) :=
        Path & ASCII.NUL;
   begin
      return Removeattr (LFS'Access, C_Path'Address, Id);
   end Removeattr;

   ----------
   -- Open --
   ----------

   function Open (LFS   : aliased in out LFS_T;
                  File  : aliased in out LFS_File;
                  Path  :                String;
                  Flags :                LFS_Open_Flags)
                  return int
   is
         function Open (lfs : access LFS_T;
                  file : access LFS_File;
                  path : System.Address;
                  flags : int)
                  return int  -- lfs.h:516
        with Import => True, Convention => C, External_Name => "lfs_file_open";

      C_Path : constant String (1 .. Path'Length + 1) :=
        Path & ASCII.NUL;

   begin
      return Open (LFS'Access, File'Access, C_Path'Address, int (Flags));
   end Open;

   -------------
   -- Opencfg --
   -------------

   function Opencfg (LFS    : aliased in out LFS_T;
                     File   : aliased in out LFS_File;
                     Path   : String;
                     Flags  : LFS_Open_Flags;
                     Config : aliased lfs_file_config)
      return int
   is
      function Opencfg (lfs : access LFS_T;
                        file : access LFS_File;
                        path : System.Address;
                        flags : int;
                        config : access constant lfs_file_config)
                        return int
        with Import => True,
        Convention => C,
        External_Name => "lfs_file_opencfg";

      C_Path : constant String (1 .. Path'Length + 1) :=
        Path & ASCII.NUL;
   begin
      return Opencfg (LFS'Access, File'Access, C_Path'Address, int (Flags),
                      Config'Access);
   end Opencfg;

   -----------
   -- Close --
   -----------

   function Close (LFS  : aliased in out LFS_T;
                   File : aliased in out LFS_File)
                   return int
   is
      function Close (lfs : access LFS_T;
                      file : access LFS_File)
                      return int  -- lfs.h:539
        with Import => True, Convention => C,
        External_Name => "lfs_file_close";
   begin
      return Close (LFS'Access, File'Access);
   end Close;

   ----------
   -- Sync --
   ----------

   function Sync
     (LFS  : aliased in out LFS_T;
      File : aliased in out LFS_File)
      return int
   is
      function Sync (lfs : access LFS_T;
                     file : access LFS_File)
                     return int
        with Import => True, Convention => C, External_Name => "lfs_file_sync";

   begin
      return Sync (LFS'Access, File'Access);
   end Sync;

   ----------
   -- Read --
   ----------

   function Read
     (LFS    : aliased in out LFS_T;
      File   : aliased in out LFS_File;
      Buffer :                System.Address;
      Size   :                LFS_Size)
      return LFS_Signed_Size
   is
      function Read (lfs : access LFS_T;
                     file : access LFS_File;
                     buffer : System.Address;
                     size : LFS_Size)
                     return LFS_Signed_Size
        with Import => True, Convention => C, External_Name => "lfs_file_read";
   begin
      return Read (LFS'Access, File'Access, Buffer, Size);
   end Read;

   -----------
   -- Write --
   -----------

   function Write
     (LFS    : aliased in out LFS_T;
      File   : aliased in out LFS_File;
      Buffer :                System.Address;
      Size   :                LFS_Size)
      return LFS_Signed_Size
   is
      function Write (lfs : access LFS_T;
                      file : access LFS_File;
                      buffer : System.Address;
                      size : LFS_Size)
                      return LFS_Signed_Size  -- lfs.h:561
        with Import => True, Convention => C,
        External_Name => "lfs_file_write";
   begin
      return Write (LFS'Access, File'Access, Buffer, Size);
   end Write;

   ----------
   -- Seek --
   ----------

   function Seek
     (LFS    : aliased in out LFS_T;
      File   : aliased in out LFS_File;
      Off    :                LFS_Signed_Offset;
      Whence :                int)
      return LFS_Signed_Offset
   is
      function Seek (lfs : access LFS_T;
                     file : access LFS_File;
                     off : LFS_Signed_Offset;
                     whence : int)
                  return LFS_Signed_Offset  -- lfs.h:569
        with Import => True, Convention => C, External_Name => "lfs_file_seek";
   begin
      return Seek (LFS'Access, File'Access, Off, Whence);
   end Seek;

   --------------
   -- Truncate --
   --------------

   function Truncate
     (LFS  : aliased in out LFS_T;
      File : aliased in out LFS_File;
      Size :                LFS_Offset)
      return int
   is
      function Truncate
        (lfs : access LFS_T;
         file : access LFS_File;
         size : LFS_Offset) return int  -- lfs.h:576
        with Import => True,
        Convention => C,
        External_Name => "lfs_file_truncate";

   begin
      return Truncate (LFS'Access, File'Access, Size);
   end Truncate;

   ----------
   -- Tell --
   ----------

   function Tell (LFS  : aliased in out LFS_T;
                  File : aliased in out LFS_File)
                  return LFS_Signed_Offset
   is
      function Tell (lfs : access LFS_T;
                     file : access LFS_File)
                     return LFS_Signed_Offset
        with Import => True, Convention => C, External_Name => "lfs_file_tell";

   begin
      return Tell (LFS'Access, File'Access);
   end Tell;

   ------------
   -- Rewind --
   ------------

   function Rewind
     (LFS : aliased in out LFS_T; File : aliased in out LFS_File) return int
   is
      function Rewind (lfs : access LFS_T;
                       file : access LFS_File)
                       return int
        with Import => True,
        Convention => C,
        External_Name => "lfs_file_rewind";
   begin
      return Rewind (LFS'Access, File'Access);
   end Rewind;

   ----------
   -- Size --
   ----------

   function Size (LFS  : aliased in out LFS_T;
                  File : aliased in out LFS_File)
                  return LFS_Signed_Offset
   is
      function Size (lfs : access LFS_T;
                     file : access LFS_File)
                     return LFS_Signed_Offset
        with Import => True, Convention => C, External_Name => "lfs_file_tell";

   begin
      return Size (LFS'Access, File'Access);
   end Size;

   -----------
   -- Mkdir --
   -----------

   function Mkdir (LFS : aliased in out LFS_T; Path : String) return int is
      function Mkdir (lfs  : access LFS_T;
                      path : System.Address)
                      return int
        with Import => True, Convention => C, External_Name => "lfs_mkdir";

      C_Path : constant String (1 .. Path'Length + 1) :=
        Path & ASCII.NUL;
   begin
      return Mkdir (LFS'Access, C_Path'Address);
   end Mkdir;

   ----------
   -- Open --
   ----------

   function Open (LFS  : aliased in out LFS_T;
                  Dir  : aliased in out LFS_Dir;
                  Path : String)
                  return int
   is
      function Open
        (lfs : access LFS_T;
         dir : access LFS_Dir;
         path : System.Address) return int  -- lfs.h:611
        with Import => True, Convention => C, External_Name => "lfs_dir_open";

      C_Path : constant String (1 .. Path'Length + 1) :=
        Path & ASCII.NUL;
   begin
      return Open (LFS'Access, Dir'Access, C_Path'Address);
   end Open;

   -----------
   -- Close --
   -----------

   function Close (LFS : aliased in out LFS_T;
                   Dir : aliased in out LFS_Dir)
                   return int
   is
      function Close (lfs : access LFS_T; dir : access LFS_Dir) return int
        with Import => True, Convention => C, External_Name => "lfs_dir_close";

   begin
      return Close (LFS'Access, Dir'Access);
   end Close;

   ----------
   -- Read --
   ----------

   function Read
     (LFS  : aliased in out LFS_T; Dir : aliased in out LFS_Dir;
      Info : aliased    out Entry_Info) return int
   is
      function Read
        (lfs  : access LFS_T;
         dir  : access LFS_Dir;
         info : access Entry_Info) return int  -- lfs.h:624
        with Import => True, Convention => C, External_Name => "lfs_dir_read";

   begin
      return Read (LFS'Access, Dir'Access, Info'Access);
   end Read;

   ----------
   -- Seek --
   ----------

   function Seek (LFS : aliased in out LFS_T;
                  Dir : aliased in out LFS_Dir;
                  Off :                LFS_Offset)
                  return int
   is
      function Seek
        (lfs : access LFS_T;
         dir : access LFS_Dir;
         off : LFS_Offset) return int  -- lfs.h:632
        with Import => True, Convention => C, External_Name => "lfs_dir_seek";

   begin
      return Seek (LFS'Access, Dir'Access, Off);
   end Seek;

   ----------
   -- Tell --
   ----------

   function Tell (LFS : aliased in out LFS_T;
                  Dir : aliased in out LFS_Dir)
      return LFS_Signed_Offset
   is
      function Tell (lfs : access LFS_T;
                     dir : access LFS_Dir)
                     return LFS_Signed_Offset
        with Import => True, Convention => C, External_Name => "lfs_dir_tell";

   begin
      return Tell (LFS'Access, Dir'Access);
   end Tell;

   ------------
   -- Rewind --
   ------------

   function Rewind (LFS : aliased in out LFS_T;
                    Dir : aliased in out LFS_Dir)
                    return int
   is
      function Rewind (lfs : access LFS_T;
                       dir : access LFS_Dir)
                       return int
        with Import => True, Convention => C,
        External_Name => "lfs_dir_rewind";

   begin
      return Rewind (LFS'Access, Dir'Access);
   end Rewind;

   ----------
   -- Size --
   ----------

   function Size (LFS : aliased in out LFS_T) return LFS_Signed_Size is
      function Size (lfs : access LFS_T)
                     return LFS_Signed_Size
        with Import => True, Convention => C, External_Name => "lfs_fs_size";
   begin
      return Size (LFS'Access);
   end Size;

   --------------
   -- Traverse --
   --------------

   function Traverse
     (LFS : aliased in out LFS_T;
      CB  :                access function
        (arg1 : System.Address; arg2 : LFS_Block) return int;
      Data : System.Address) return int
   is
      function Traverse
        (lfs : access LFS_T;
         cb : access function (arg1 : System.Address; arg2 : LFS_Block)
                     return int;
         data : System.Address) return int
        with Import => True,
        Convention => C,
        External_Name => "lfs_fs_traverse";

   begin
      return Traverse (LFS'Access, CB, Data);
   end Traverse;

   --  -------------
   --  -- Migrate --
   --  -------------
   --
   --  function Migrate (LFS : aliased in out LFS_T;
   --                    Config : aliased in out LFS_Config)
   --                    return int
   --  is
   --     function Migrate (lfs : access LFS_T;
   --                       config : access constant LFS_Config) return int
   --       with Import => True,
   --       Convention => C,
   --       External_Name => "lfs_migrate";
   --
   --  begin
   --     return Migrate (LFS'Access, Config'Access);
   --  end Migrate;
   --
   -------------
   -- Getattr --
   -------------

   function Getattr (LFS    : aliased in out LFS_T;
                     Path   : String;
                     Id     : Interfaces.Unsigned_8;
                     Buffer : System.Address;
                     Size   : LFS_Size)
                     return LFS_Signed_Size
   is
      function Getattr (lfs : access LFS_T;
                        path : System.Address;
                        c_type : Interfaces.Unsigned_8;
                        buffer : System.Address;
                        size : LFS_Size)
                        return LFS_Signed_Size  -- lfs.h:483
        with Import => True, Convention => C, External_Name => "lfs_getattr";

      C_Path : constant String (1 .. Path'Length + 1) :=
        Path & ASCII.NUL;
   begin
      return Getattr (LFS'Access, C_Path'Address, Id, Buffer, Size);
   end Getattr;

end Littlefs;
