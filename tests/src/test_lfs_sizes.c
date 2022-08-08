#include "lfs.h"

uint32_t
lfs_h_version (void)
{
  return LFS_VERSION;
}

uint32_t
lfs_h_disk_version (void)
{
  return LFS_DISK_VERSION;
}

int
config_size (void)
{
  return sizeof(struct lfs_config);
}

int
info_size (void)
{
  return sizeof(struct lfs_info);
}

int
attr_size (void)
{
  return sizeof(struct lfs_attr);
}

int
file_config_size (void)
{
  return sizeof(struct lfs_file_config);
}

int
dir_size (void)
{
  return sizeof(struct lfs_dir);
}

int
file_size (void)
{
  return sizeof(struct lfs_file);
}

int
lfs_size (void)
{
  return sizeof(struct lfs);
}
