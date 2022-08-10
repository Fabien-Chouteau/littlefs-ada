with Interfaces;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;
with System;

with Littlefs_Config;

package Littlefs is

   VERSION : constant := 16#00020003#;
   --  Software library version
   --  Major (top-nibble), incremented on backwards incompatible changes
   --  Minor (bottom-nibble), incremented on feature additions

   DISK_VERSION : constant := 16#00020000#;
   --  Version of On-disk data structures
   --  Major (top-nibble), incremented on backwards incompatible changes
   --  Minor (bottom-nibble), incremented on feature additions

   LFS_NAME_MAX : constant := Littlefs_Config.Max_Name_Size;
   --  Maximum name size in bytes, may be redefined to reduce the size of
   --  the info struct. Limited to <= 1022. Stored in superblock and must
   --  be respected by other littlefs drivers.

   LFS_FILE_MAX : constant := Littlefs_Config.Max_File_Size;
   --  Maximum size of a file in bytes, may be redefined to limit to support
   --  other drivers. Limited on disk to <= 4294967296. However, above
   --  2147483647 the functions lfs_file_seek, lfs_file_size, and lfs_file_tell
   --  will return incorrect values due to using signed integers. Stored in
   --  superblock and must be respected by other littlefs drivers.

   LFS_ATTR_MAX : constant := Littlefs_Config.Max_Attr_Size;
   --  Maximum size of custom attributes in bytes, may be redefined, but there
   --  is no real benefit to using a smaller LFS_ATTR_MAX. Limited to <= 1022.
   --  Possible error codes, these are negative to allow valid positive return
   --  values

   --  Type definitions
   subtype LFS_Size is Interfaces.Unsigned_32;
   subtype LFS_Offset is Interfaces.Unsigned_32;
   subtype LFS_Signed_Size is Interfaces.Integer_32;
   subtype LFS_Signed_Offset is Interfaces.Integer_32;
   subtype LFS_Block is Interfaces.Unsigned_32;

   pragma Style_Checks (Off);

   subtype LFS_Error is Interfaces.C.int;
   LFS_ERR_OK          : constant LFS_Error := 0;   -- No error
   LFS_ERR_IO          : constant LFS_Error := -5;  -- Error during device operation
   LFS_ERR_CORRUPT     : constant LFS_Error := -84; -- Corrupted
   LFS_ERR_NOENT       : constant LFS_Error := -2;  -- No directory entry
   LFS_ERR_EXIST       : constant LFS_Error := -17; -- Entry already exists
   LFS_ERR_NOTDIR      : constant LFS_Error := -20; -- Entry is not a dir
   LFS_ERR_ISDIR       : constant LFS_Error := -21; -- Entry is a dir
   LFS_ERR_NOTEMPTY    : constant LFS_Error := -39; -- Dir is not empty
   LFS_ERR_BADF        : constant LFS_Error := -9;  -- Bad file number
   LFS_ERR_FBIG        : constant LFS_Error := -27; -- File too large
   LFS_ERR_INVAL       : constant LFS_Error := -22; -- Invalid parameter
   LFS_ERR_NOSPC       : constant LFS_Error := -28; -- No space left on device
   LFS_ERR_NOMEM       : constant LFS_Error := -12; -- No more memory available
   LFS_ERR_NOATTR      : constant LFS_Error := -61; -- No data/attr available
   LFS_ERR_NAMETOOLONG : constant LFS_Error := -36; -- File name too long

   function Error_Img (Err : int) return String
   is (case Err is
          when LFS_ERR_OK          => "No error",
          when LFS_ERR_IO          => "Error during device operation",
          when LFS_ERR_CORRUPT     => "Corrupted",
          when LFS_ERR_NOENT       => "No directory entry",
          when LFS_ERR_EXIST       => "Entry already exists",
          when LFS_ERR_NOTDIR      => "Entry is not a dir",
          when LFS_ERR_ISDIR       => "Entry is a dir",
          when LFS_ERR_NOTEMPTY    => "Dir is not empty",
          when LFS_ERR_BADF        => "Bad file number",
          when LFS_ERR_FBIG        => "File too large",
          when LFS_ERR_INVAL       => "Invalid parameter",
          when LFS_ERR_NOSPC       => "No space left on device",
          when LFS_ERR_NOMEM       => "No more memory available",
          when LFS_ERR_NOATTR      => "No data/attr available",
          when LFS_ERR_NAMETOOLONG => "File name too long",
          when others              => "Unknown LFS error (" & Err'Img & ")");

   subtype LFS_Open_Flags is Interfaces.C.unsigned;
   LFS_O_RDONLY  : constant LFS_Open_Flags := 16#000001#; -- Open a file as read only
   LFS_O_WRONLY  : constant LFS_Open_Flags := 16#000002#; -- Open a file as write only
   LFS_O_RDWR    : constant LFS_Open_Flags := 16#000003#; -- Open a file as read and write
   LFS_O_CREAT   : constant LFS_Open_Flags := 16#000100#; -- Create a file if it does not exist
   LFS_O_EXCL    : constant LFS_Open_Flags := 16#000200#; -- Fail if a file already exists
   LFS_O_TRUNC   : constant LFS_Open_Flags := 16#000400#; -- Truncate the existing file to zero size
   LFS_O_APPEND  : constant LFS_Open_Flags := 16#000800#; -- Move to end of file on every write

   pragma Style_Checks (On);

   type LFS_Whence_Flags is
     (LFS_SEEK_SET, -- Seek relative to an absolute position
      LFS_SEEK_CUR, -- Seek relative to the current file position
      LFS_SEEK_END) -- Seek relative to the end of the file
   with Convention => C;

   --  File info structure --

   type Entry_Info is private;

   type File_Kind is (Register, Directory);

   function Kind (Info : Entry_Info) return File_Kind;
   --  Type of the file, either LFS_TYPE_REG or LFS_TYPE_DIR

   function Size (Info : Entry_Info) return LFS_Size;
   --  Size of the file, only valid for REG files. Limited to 32-bits.

   function Name (Info : Entry_Info) return String;
   --  Name of the file. Limited to LFS_NAME_MAX, which can be changed by
   --  redefining LFS_NAME_MAX to reduce RAM. LFS_NAME_MAX is stored in
   --  superblock and must be respected by other littlefs drivers.

   --  Custom attribute structure, used to describe custom attributes
   --  committed atomically during file writes.
   type lfs_attr is record
      Id : aliased Interfaces.Unsigned_8;
      --  8-bit type of attribute, provided by user and used to identify the
      --  attribute

      Buffer : System.Address;
      --  Pointer to buffer containing the attribute

      Size : aliased LFS_Size;
      --  Size of attribute in bytes, limited to LFS_ATTR_MAX

   end record
   with Convention => C_Pass_By_Copy;

   --  Optional configuration provided during Opencfg
   type lfs_file_config is record
      Buffer : System.Address;
      --  Optional statically allocated file buffer. Must be cache_size.
      --  By default lfs_malloc is used to allocate this buffer.

      Attrs : access lfs_attr;
      --  Optional list of custom attributes related to the file. If the file
      --  is opened with read access, these attributes will be read from disk
      --  during the open call. If the file is opened with write access, the
      --  attributes will be written to disk every file sync or close. This
      --  write occurs atomically with update to the file's contents. Custom
      --  attributes are uniquely identified by an 8-bit type and limited to
      --  LFS_ATTR_MAX bytes. When read, if the stored attribute is smaller
      --  than the buffer, it will be padded with zeros. If the stored
      --  attribute is larger, then it will be silently truncated. If the
      --  attribute is not found, it will be created implicitly.

      Attr_Count : aliased LFS_Size;
      --  Number of custom attributes in the list

   end record
   with Convention => C_Pass_By_Copy;

   type LFS_T is private;
   type LFS_File is private;
   type LFS_Dir is private;
   type LFS_Config;

   --------------------------
   -- Filesystem functions --
   --------------------------

   function Format (LFS    : aliased in out LFS_T;
                    Config : aliased  LFS_Config)
                    return int;
   --  Format a block device with the littlefs
   --
   --  Requires a littlefs object and config struct. This clobbers the littlefs
   --  object, and does not leave the filesystem mounted. The config struct
   --  must be zeroed for defaults and backwards compatibility.
   --
   --  Returns a negative error code on failure.

   function Mount (LFS    : aliased in out LFS_T;
                   Config : aliased  LFS_Config)
                   return int;
   --  Mounts a littlefs
   --
   --  Requires a littlefs object and config struct. Multiple filesystems may
   --  be mounted simultaneously with multiple littlefs objects. Both lfs and
   --  config must be allocated while mounted. The config struct must be zeroed
   --  for defaults and backwards compatibility.
   --
   --  Returns a negative error code on failure.

   function Unmount (LFS : aliased in out LFS_T) return int;
   --  Unmounts a littlefs
   --
   --  Does nothing besides releasing any allocated resources.
   --
   --  Returns a negative error code on failure.

   ------------------------
   -- General operations --
   ------------------------

   function Remove (LFS : aliased in out LFS_T;
                    Path : String)
                    return int;
   --  Removes a file or directory
   --
   --  If removing a directory, the directory must be empty.
   --
   --  Returns a negative error code on failure.

   function Rename (LFS : aliased in out LFS_T;
                    Oldpath : String;
                    Newpath : String)
                    return int;
   --  Rename or move a file or directory
   --
   --  If the destination exists, it must match the source in type.
   --  If the destination is a directory, the directory must be empty.
   --
   --  Returns a negative error code on failure.

   function Stat (LFS  : aliased in out LFS_T;
                  Path : String;
                  Info : aliased out Entry_Info)
                  return int;
   --  Find info about a file or directory
   --
   --  Fills out the info structure, based on the specified file or directory.
   --
   --  Returns a negative error code on failure.

   function Getattr (LFS    : aliased in out LFS_T;
                     Path   : String;
                     Id     : Interfaces.Unsigned_8;
                     Buffer : System.Address;
                     Size   : LFS_Size)
                     return LFS_Signed_Size;
   --  Get a custom attribute
   --
   --  Custom attributes are uniquely identified by an 8-bit type and limited
   --  to LFS_ATTR_MAX bytes. When read, if the stored attribute is smaller
   --  than the buffer, it will be padded with zeros. If the stored attribute
   --  is larger, then it will be silently truncated. If no attribute is found,
   --  the error LFS_ERR_NOATTR is returned and the buffer is filled with
   --  zeros. Returns the size of the attribute, or a negative error code on
   --  failure. Note, the returned size is the size of the attribute on disk,
   --  irrespective of the size of the buffer. This can be used to dynamically
   --  allocate a buffer
   --
   --  or check for existance.

   function Setattr (LFS    : aliased in out LFS_T;
                     Path   : String;
                     Id     : Interfaces.Unsigned_8;
                     Buffer : System.Address;
                     Size   : LFS_Size)
                     return int;
   --  Set custom attributes
   --
   --  Custom attributes are uniquely identified by an 8-bit type and limited
   --  to LFS_ATTR_MAX bytes. If an attribute is not found, it will be
   --  implicitly created.
   --
   --  Returns a negative error code on failure.

   function Removeattr (LFS  : aliased in out LFS_T;
                        Path : String;
                        Id   : Interfaces.Unsigned_8)
                        return int;
   --  Removes a custom attribute
   --
   --  If an attribute is not found, nothing happens.
   --
   --  Returns a negative error code on failure.

   ---------------------
   -- File operations --
   ---------------------

   function Open (LFS   : aliased in out LFS_T;
                  File  : aliased in out LFS_File;
                  Path  : String;
                  Flags : LFS_Open_Flags)
                  return int;
   --  Open a file
   --
   --  The mode that the file is opened in is determined by the flags, which
   --  are values from the enum lfs_open_flags that are bitwise-ored together.
   --
   --  Returns a negative error code on failure.

   function Opencfg (LFS    : aliased in out LFS_T;
                     File   : aliased in out LFS_File;
                     Path   : String;
                     Flags  : LFS_Open_Flags;
                     Config : aliased lfs_file_config)
                     return int;
   --  Open a file with extra configuration
   --
   --  The mode that the file is opened in is determined by the flags, which
   --  are values from the enum lfs_open_flags that are bitwise-ored together.
   --  The config struct provides additional config options per file as
   --  described above. The config struct must be allocated while the file is
   --  open, and the config struct must be zeroed for defaults and backwards
   --  compatibility.
   --
   --  Returns a negative error code on failure.

   function Close (LFS  : aliased in out LFS_T;
                   File : aliased in out LFS_File)
                   return int;
   --  Close a file
   --
   --  Any pending writes are written out to storage as though sync had been
   --  called and releases any allocated resources.
   --
   --  Returns a negative error code on failure.

   function Sync (LFS  : aliased in out LFS_T;
                  File : aliased in out LFS_File)
                  return int;
   --  Synchronize a file on storage
   --
   --  Any pending writes are written out to storage.
   --
   --  Returns a negative error code on failure.

   function Read (LFS    : aliased in out LFS_T;
                  File   : aliased in out LFS_File;
                  Buffer : System.Address;
                  Size   : LFS_Size)
                  return LFS_Signed_Size;
   --  Read data from file
   --
   --  Takes a buffer and size indicating where to store the read data.
   --
   --  Returns the number of bytes read, or a negative error code on failure.

   function Write (LFS    : aliased in out LFS_T;
                   File   : aliased in out LFS_File;
                   Buffer : System.Address;
                   Size   : LFS_Size)
                   return LFS_Signed_Size;
   --  Write data to file
   --
   --  Takes a buffer and size indicating the data to write. The file will not
   --  actually be updated on the storage until either sync or close is called.
   --
   --  Returns the number of bytes written, or a negative error code on
   --  failure.

   function Seek (LFS    : aliased in out LFS_T;
                  File   : aliased in out LFS_File;
                  Off    :                LFS_Signed_Offset;
                  Whence :                int)
                  return LFS_Signed_Offset;
   --  Change the position of the file
   --
   --  The change in position is determined by the offset and whence flag.
   --
   --  Returns the new position of the file, or a negative error code on
   --  failure.

   function Truncate (LFS  : aliased in out LFS_T;
                      File : aliased in out LFS_File;
                      Size :                LFS_Offset)
                      return int;
   --  Truncates the size of the file to the specified size
   --
   --  Returns a negative error code on failure.

   function Tell (LFS  : aliased in out LFS_T;
                  File : aliased in out LFS_File)
                  return LFS_Signed_Offset;
   --  Return the position of the file
   --
   --  Equivalent to lfs_file_seek(lfs, file, 0, LFS_SEEK_CUR)
   --
   --  Returns the position of the file, or a negative error code on failure.

   function Rewind (LFS  : aliased in out LFS_T;
                    File : aliased in out LFS_File)
                    return int;
   --  Change the position of the file to the beginning of the file
   --
   --  Equivalent to lfs_file_seek(lfs, file, 0, LFS_SEEK_SET)
   --
   --  Returns a negative error code on failure.

   function Size (LFS  : aliased in out LFS_T;
                  File : aliased in out LFS_File)
                  return LFS_Signed_Offset;
   --  Return the size of the file
   --
   --  Similar to lfs_file_seek(lfs, file, 0, LFS_SEEK_END)
   --
   --  Returns the size of the file, or a negative error code on failure.

   --------------------------
   -- Directory operations --
   --------------------------

   function Mkdir (LFS  : aliased in out LFS_T;
                   Path : String)
                   return int;
   --  Create a directory
   --
   --  Returns a negative error code on failure.

   function Open (LFS  : aliased in out LFS_T;
                  Dir  : aliased in out LFS_Dir;
                  Path : String)
                  return int;
   --  Open a directory
   --
   --  Once open a directory can be used with read to iterate over files.
   --
   --  Returns a negative error code on failure.

   function Close (LFS : aliased in out LFS_T;
                   Dir : aliased in out LFS_Dir)
                   return int;
   --  Close a directory
   --
   --  Releases any allocated resources.
   --
   --  Returns a negative error code on failure.

   function Read (LFS  : aliased in out LFS_T;
                  Dir  : aliased in out LFS_Dir;
                  Info : aliased    out Entry_Info)
                  return int;
   --  Read an entry in the directory
   --
   --  Fills out the info structure, based on the specified file or directory.
   --
   --  Returns a positive value on success, 0 at the end of directory, or a
   --  negative error code on failure.

   function Seek (LFS : aliased in out LFS_T;
                  Dir : aliased in out LFS_Dir;
                  Off :                LFS_Offset)
                  return int;
   --  Change the position of the directory
   --
   --  The new off must be a value previous returned from tell and specifies an
   --  absolute offset in the directory seek.
   --
   --  Returns a negative error code on failure.

   function Tell (LFS : aliased in out LFS_T;
                  Dir : aliased in out LFS_Dir)
                  return LFS_Signed_Offset;
   --  Return the position of the directory
   --
   --  The returned offset is only meant to be consumed by seek and may not
   --  make sense, but does indicate the current position in the directory
   --  iteration.
   --
   --  Returns the position of the directory, or a negative error code on
   --  failure.

   function Rewind (LFS : aliased in out LFS_T;
                    Dir : aliased in out LFS_Dir)
                    return int;
   --  Change the position of the directory to the beginning of the directory
   --
   --  Returns a negative error code on failure.

   --------------------------------------------
   -- Filesystem-level filesystem operations --
   --------------------------------------------

   function Size (LFS : aliased in out LFS_T) return LFS_Signed_Size;
   --  Finds the current size of the filesystem
   --
   --  Note: Result is best effort. If files share COW structures, the returned
   --  size may be larger than the filesystem actually is.
   --
   --  Returns the number of allocated blocks, or a negative error code on
   --  failure.

   function Traverse
     (LFS : aliased in out LFS_T;
      CB  : access function (arg1 : System.Address; arg2 : LFS_Block)
                   return int;
      Data : System.Address)
      return int;
   --  Traverse through all blocks in use by the filesystem
   --
   --  The provided callback will be called with each block address that is
   --  currently in use by the filesystem. This can be used to determine which
   --  blocks are in use or how much of the storage is available.
   --
   --  Returns a negative error code on failure.

   --  Configuration provided during initialization of the littlefs
   type LFS_Config is record
      Context : System.Address := System.Null_Address;
      --  Opaque user provided context that can be used to pass information to
      --  the block device operations

      Read : access function (C : access constant LFS_Config;
                              Block : LFS_Block;
                              Off : LFS_Offset;
                              Buffer : System.Address;
                              Size : LFS_Size)
                              return int := null;
      --  Read a region in a block. Negative error codes are propogated to the
      --  user.

      Prog : access function (C : access constant LFS_Config;
                              Block : LFS_Block;
                              Off : LFS_Offset;
                              Buffer : System.Address;
                              Size : LFS_Size)
                              return int := null;
      --  Program a region in a block. The block must have previously been
      --  erased. Negative error codes are propogated to the user. May return
      --  LFS_ERR_CORRUPT if the block should be considered bad.

      Erase : access function (C : access constant LFS_Config;
                               Block : LFS_Block)
                               return int := null;
      --  Erase a block. A block must be erased before being programmed. The
      --  state of an erased block is undefined. Negative error codes are
      --  propogated to the user. May return LFS_ERR_CORRUPT if the block
      --  should be considered bad.

      Sync : access function (C : access constant LFS_Config) return int :=
        null;
      --  Sync the state of the underlying block device. Negative error codes
      --  are propogated to the user.

      --  LFS_THREADSAFE not implemented
      --  Lock the underlying block device. Negative error codes
      --  are propogated to the user.
      --  Unlock the underlying block device. Negative error codes
      --  are propogated to the user.

      Read_Size : aliased LFS_Size := 0;
      --  Minimum size of a block read. All read operations will be a multiple
      --  of this value.

      Prog_Size : aliased LFS_Size := 0;
      --  Minimum size of a block program. All program operations will be a
      --  multiple of this value.

      Block_Size : aliased LFS_Size := 0;
      --  Size of an erasable block. This does not impact ram consumption and
      --  may be larger than the physical erase size. However, non-inlined
      --  files take up at minimum one block. Must be a multiple of the read
      --  and program sizes.

      Block_Count : aliased LFS_Size := 0;
      --  Number of erasable blocks on the device.

      Block_Cycles : aliased Interfaces.Integer_32 := 0;
      --  Number of erase cycles before littlefs evicts metadata logs and moves
      --  the metadata to another block. Suggested values are in the range
      --  100-1000, with large values having better performance at the cost
      --  of less consistent wear distribution.
      --
      --  Set to -1 to disable block-level wear-leveling.

      Cache_Size : aliased LFS_Size := 0;
      --  Size of block caches. Each cache buffers a portion of a block in RAM.
      --  The littlefs needs a read cache, a program cache, and one additional
      --  cache per file. Larger caches can improve performance by storing more
      --  data and reducing the number of disk accesses. Must be a multiple of
      --  the read and program sizes, and a factor of the block size.

      Lookahead_Size : aliased LFS_Size := 0;
      --  Size of the lookahead buffer in bytes. A larger lookahead buffer
      --  increases the number of blocks found during an allocation pass. The
      --  lookahead buffer is stored as a compact bitmap, so each byte of RAM
      --  can track 8 blocks. Must be a multiple of 8.

      Read_Buffer : System.Address := System.Null_Address;
      --  Optional statically allocated read buffer. Must be cache_size. By
      --  default lfs_malloc is used to allocate this buffer.

      Prog_Buffer : System.Address := System.Null_Address;
      --  Optional statically allocated program buffer. Must be cache_size. By
      --  default lfs_malloc is used to allocate this buffer.

      Lookahead_Buffer : System.Address := System.Null_Address;
      --  Optional statically allocated lookahead buffer. Must be
      --  lookahead_size and aligned to a 32-bit boundary. By default
      --  lfs_malloc is used to allocate this buffer.

      Name_Max : aliased LFS_Size := 0;
      --  Optional upper limit on length of file names in bytes. No downside
      --  for larger names except the size of the info struct which is
      --  controlled by the LFS_NAME_MAX define. Defaults to LFS_NAME_MAX when
      --  zero. Stored in superblock and must be respected by other littlefs
      --  drivers.

      File_Max : aliased LFS_Size := 0;
      --  Optional upper limit on files in bytes. No downside for larger files
      --  but must be <= LFS_FILE_MAX. Defaults to LFS_FILE_MAX when zero.
      --  Stored in superblock and must be respected by other littlefs drivers.

      Attr_Max : aliased LFS_Size := 0;
      --  Optional upper limit on custom attributes in bytes. No downside
      --  for larger attributes size but must be <= LFS_ATTR_MAX. Defaults
      --  to LFS_ATTR_MAX when zero.

   end record
   with Convention => C_Pass_By_Copy;

private

   for File_Kind use (Register => 1,
                      Directory => 2);

   type Entry_Info is record
      c_type : aliased Interfaces.Unsigned_8;
      size : aliased LFS_Size;
      name : aliased String (1 .. LFS_NAME_MAX + 1);
   end record
   with Convention => C_Pass_By_Copy;

   ---------------------------------------
   -- internal littlefs data structures --
   ---------------------------------------

   type lfs_cache_t is record
      block : aliased LFS_Block;
      off : aliased LFS_Offset;
      size : aliased LFS_Size;
      buffer : access Interfaces.Unsigned_8;
   end record
   with Convention => C_Pass_By_Copy;

   type lfs_mdir_array2006 is array (0 .. 1) of aliased LFS_Block;
   type lfs_mdir_t is record
      pair : aliased lfs_mdir_array2006;
      rev : aliased Interfaces.Unsigned_32;
      off : aliased LFS_Offset;
      etag : aliased Interfaces.Unsigned_32;
      count : aliased Interfaces.Unsigned_16;
      erased : aliased Extensions.bool;
      split : aliased Extensions.bool;
      tail : aliased lfs_mdir_array2006;
   end record
   with Convention => C_Pass_By_Copy;

   --  littlefs directory type
   type lfs_dir_array2006 is array (0 .. 1) of aliased LFS_Block;
   type LFS_Dir is record
      next : access LFS_Dir;
      id : aliased Interfaces.Unsigned_16;
      c_type : aliased Interfaces.Unsigned_8;
      m : aliased lfs_mdir_t;
      pos : aliased LFS_Offset;
      head : aliased lfs_dir_array2006;
   end record
   with Convention => C_Pass_By_Copy;

   --  littlefs file type
   type lfs_ctz is record
      head : aliased LFS_Block;
      size : aliased LFS_Size;
   end record
   with Convention => C_Pass_By_Copy;
   type LFS_File is record
      next : access LFS_File;
      id : aliased Interfaces.Unsigned_16;
      c_type : aliased Interfaces.Unsigned_8;
      m : aliased lfs_mdir_t;
      ctz : aliased lfs_ctz;
      flags : aliased Interfaces.Unsigned_32;
      pos : aliased LFS_Offset;
      block : aliased LFS_Block;
      off : aliased LFS_Offset;
      cache : aliased lfs_cache_t;
      cfg : access constant lfs_file_config;
   end record
   with Convention => C_Pass_By_Copy;

   type lfs_superblock is record
      version : aliased Interfaces.Unsigned_32;
      block_size : aliased LFS_Size;
      block_count : aliased LFS_Size;
      name_max : aliased LFS_Size;
      file_max : aliased LFS_Size;
      attr_max : aliased LFS_Size;
   end record
   with Convention => C_Pass_By_Copy;

   subtype lfs_superblock_t is lfs_superblock;

   type lfs_gstate_array2006 is array (0 .. 1) of aliased LFS_Block;
   type lfs_gstate is record
      tag : aliased Interfaces.Unsigned_32;
      pair : aliased lfs_gstate_array2006;
   end record
   with Convention => C_Pass_By_Copy;

   subtype lfs_gstate_t is lfs_gstate;

   --  The littlefs filesystem type
   type lfs_mlist;
   type lfs_mlist is record
      next : access lfs_mlist;
      id : aliased Interfaces.Unsigned_16;
      c_type : aliased Interfaces.Unsigned_8;
      m : aliased lfs_mdir_t;
   end record
   with Convention => C_Pass_By_Copy;
   type lfs_free is record
      off : aliased LFS_Block;
      size : aliased LFS_Block;
      i : aliased LFS_Block;
      ack : aliased LFS_Block;
      buffer : access Interfaces.Unsigned_32;
   end record
   with Convention => C_Pass_By_Copy;
   type lfs_array2006 is array (0 .. 1) of aliased LFS_Block;
   type LFS_T is record
      rcache : aliased lfs_cache_t;
      pcache : aliased lfs_cache_t;
      root : aliased lfs_array2006;
      mlist : access lfs_mlist;
      seed : aliased Interfaces.Unsigned_32;
      gstate : aliased lfs_gstate_t;
      gdisk : aliased lfs_gstate_t;
      gdelta : aliased lfs_gstate_t;
      free : aliased lfs_free;
      cfg : access constant LFS_Config;
      name_max : aliased LFS_Size;
      file_max : aliased LFS_Size;
      attr_max : aliased LFS_Size;
   end record
   with Convention => C_Pass_By_Copy;

   --  function Migrate (LFS : aliased in out LFS_T;
   --                    Config : aliased in out LFS_Config)
   --                    return int;
   --  --  Attempts to migrate a previous version of littlefs
   --  --
   --  --  Behaves similarly to the lfs_format function. Attempts to mount
   --  --  the previous version of littlefs and update the filesystem so it
   --  --  can be  mounted with the current version of littlefs.
   --  --  Requires a littlefs object and config struct. This clobbers the
   --  --  littlefs object, and does not leave the filesystem mounted. The
   --  --  config struct must be zeroed for defaults and backwards
   --  --  compatibility.
   --  --
   --  --  Returns a negative error code on failure.

   pragma Inline (Kind);
   pragma Inline (Name);
   pragma Inline (Size);
   pragma Inline (Format);
   pragma Inline (Mount);
   pragma Inline (Unmount);
   pragma Inline (Remove);
   pragma Inline (Rename);
   pragma Inline (Stat);
   pragma Inline (Getattr);
   pragma Inline (Setattr);
   pragma Inline (Removeattr);
   pragma Inline (Open);
   pragma Inline (Opencfg);
   pragma Inline (Close);
   pragma Inline (Sync);
   pragma Inline (Read);
   pragma Inline (Write);
   pragma Inline (Seek);
   pragma Inline (Truncate);
   pragma Inline (Tell);
   pragma Inline (Rewind);
   pragma Inline (Mkdir);
   pragma Inline (Traverse);

end Littlefs;
