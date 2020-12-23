with Littlefs;

package RAM_BD is

   function Create (Size : Littlefs.LFS_Size)
                    return access constant Littlefs.LFS_Config;

end RAM_BD;
