unit unitlibc;

{$mode objfpc}{$H+}

interface

Const
  clib = 'c';

Type
  __ptr_t = Pointer;
  P__ptr_t = ^__ptr_t;
  ptrdiff_t = Integer;
  __long_double_t = Extended;
  P__long_double_t = ^__long_double_t;
  size_t = Cardinal;
  Psize_t = ^size_t;
  UInt64 = 0..High(Int64); // Must be unsigned.
  wchar_t = widechar;
  Pwchar_t = ^wchar_t;
  PPwchar_t = ^Pwchar_t;

type
   __u_char = byte;
   __u_short = word;
   __u_int = dword;
   __u_long = dword;
   __u_quad_t = qword;
   __quad_t = int64;

   __int8_t = char;
   __uint8_t = byte;
   __int16_t = smallint;
   __uint16_t = word;
   __int32_t = longint;
   __uint32_t = dword;
   __int64_t = Int64;
   __uint64_t = Qword;

   __qaddr_t = __quad_t;
   __dev_t = __u_quad_t;
   __uid_t = __u_int;
   __gid_t = __u_int;
   __ino_t = __u_long;
   __mode_t = __u_int;
   __nlink_t = __u_int;
   __off_t = longint;
   __loff_t = __quad_t;
   __pid_t = longint;
   __ssize_t = longint;
   __rlim_t = __u_long;
   __rlim64_t = __u_quad_t;
   __id_t = __u_int;
   __fsid_t = record
        __val : array[0..1] of longint;
     end;
   __daddr_t = longint;
   __caddr_t = char;
   __time_t = longint;
   __useconds_t = dword;
   __suseconds_t = longint;
   __swblk_t = longint;
   __clock_t = longint;
   __clockid_t = longint;
   __timer_t = longint;
   __fd_mask = dWord;

type
  __key_t = longint;
  __ipc_pid_t = word;
  __blksize_t = longint;
  __blkcnt_t = longint;
  __blkcnt64_t = __quad_t;
  __fsblkcnt_t = __u_long;
  __fsblkcnt64_t = __u_quad_t;
  __fsfilcnt_t = __u_long;
  __fsfilcnt64_t = __u_quad_t;
  __ino64_t = __u_quad_t;
  __off64_t = __loff_t;
  __t_scalar_t = longint;
  __t_uscalar_t = dword;
  __intptr_t = longint;
  __socklen_t = dword;
  TFileDescriptor = integer;



type
   Pdirent = ^dirent;
   dirent = record
        d_ino : dword;
        d_off : longint;
        d_reclen : word;
        d_type : byte;
        d_name : array[0..255] of char;
     end;

type
   Pdirent64 = ^dirent64;
   dirent64 = record
        d_ino : qword;
        d_off : qword;
        d_reclen : word;
        d_type : byte;
        d_name : array[0..255] of char;
     end;

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TDirEnt = dirent;
  PPDirEnt = ^PDirEnt;
  PPPDirEnt = ^PPDirEnt;

  TDirEnt64 = dirent64;
  PPDirEnt64 = ^PDirEnt64;
  PPPDirEnt64 = ^PPDirEnt64;

const
   _STAT_VER_LINUX_OLD = 1;
   _STAT_VER_KERNEL = 1;
   _STAT_VER_SVR4 = 2;
   _STAT_VER_LINUX = 3;
   _STAT_VER = _STAT_VER_LINUX;

   _MKNOD_VER_LINUX = 1;
   _MKNOD_VER_SVR4 = 2;
   _MKNOD_VER = _MKNOD_VER_LINUX;

type
   P_stat = ^_stat;
   PStat = ^_stat;
   _stat = packed record
        st_dev : __dev_t;
        __pad1 : word;
        __align_pad1 : word;
        st_ino : __ino_t;
        st_mode : __mode_t;
        st_nlink : __nlink_t;
        st_uid : __uid_t;
        st_gid : __gid_t;
        st_rdev : __dev_t;
        __pad2 : word;
        __align_pad2 : word;
        st_size : __off_t;
        st_blksize : __blksize_t;
        st_blocks : __blkcnt_t;
        st_atime : __time_t;
        __unused1 : dword;
        st_mtime : __time_t;
        __unused2 : dword;
        st_ctime : __time_t;
        __unused3 : dword;
        __unused4 : dword;
        __unused5 : dword;
     end;

   P_stat64 = ^_stat64;
   Pstat64 = ^_stat64;
   _stat64 = record
        st_dev : __dev_t;
        __pad1 : dword;
        __st_ino : __ino_t;
        st_mode : __mode_t;
        st_nlink : __nlink_t;
        st_uid : __uid_t;
        st_gid : __gid_t;
        st_rdev : __dev_t;
        __pad2 : dword;
        st_size : __off64_t;
        st_blksize : __blksize_t;
        st_blocks : __blkcnt64_t;
        st_atime : __time_t;
        __unused1 : dword;
        st_mtime : __time_t;
        __unused2 : dword;
        st_ctime : __time_t;
        __unused3 : dword;
        st_ino : __ino64_t;
     end;

const
  __S_IFMT        = $F000;
  __S_IFDIR       = $4000;
  __S_IFCHR       = $2000;
  __S_IFBLK       = $6000;
  __S_IFREG       = $8000;
  __S_IFIFO       = $1000;
  __S_IFLNK       = $A000;
  __S_IFSOCK      = $C000;

  __S_ISUID       = $800;
  __S_ISGID       = $400;
  __S_ISVTX       = $200;
  __S_IREAD       = $100;
  __S_IWRITE      = $80;
  __S_IEXEC       = $40;

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TStatBuf = _stat;
  PStatBuf = ^TStatBuf;

  TStatBuf64 = _stat64;
  PStatBuf64 = ^TStatBuf64;

function stat(__file:Pchar; var __buf:_stat):longint;
function stat64(__file:Pchar; var __buf: _stat64):longint;

function lstat(__file:Pchar; var __buf:_stat):longint;
function lstat64(__file:Pchar; var __buf:_stat64):longint;

function __xstat(__ver:longint; __filename:Pchar; var __stat_buf: _stat):longint;cdecl;external clib name '__xstat';
function __xstat64(__ver:longint; __filename:Pchar; var __stat_buf: _stat64):longint;cdecl;external clib name '__xstat64';
function __lxstat(__ver:longint; __filename:Pchar; var __stat_buf: _stat):longint;cdecl;external clib name '__lxstat';
function __lxstat64(__ver:longint; __filename:Pchar; var __stat_buf: _stat64):longint;cdecl;external clib name '__lxstat64';

function readlink(__path:Pchar; __buf:Pchar; __len:size_t):longint;cdecl;external clib name 'readlink';

Const
  GLOB_ERR = 1 shl 0;
  GLOB_MARK = 1 shl 1;
  GLOB_NOSORT = 1 shl 2;
  GLOB_DOOFFS = 1 shl 3;
  GLOB_NOCHECK = 1 shl 4;
  GLOB_APPEND = 1 shl 5;
  GLOB_NOESCAPE = 1 shl 6;
  GLOB_PERIOD = 1 shl 7;
  GLOB_MAGCHAR = 1 shl 8;
  GLOB_ALTDIRFUNC = 1 shl 9;
  GLOB_BRACE = 1 shl 10;
  GLOB_NOMAGIC = 1 shl 11;
  GLOB_TILDE = 1 shl 12;
  GLOB_ONLYDIR = 1 shl 13;
  GLOB_TILDE_CHECK = 1 shl 14;
  __GLOB_FLAGS = ((((((((((((GLOB_ERR or GLOB_MARK) or GLOB_NOSORT) or GLOB_DOOFFS) or GLOB_NOESCAPE) or GLOB_NOCHECK) or GLOB_APPEND) or GLOB_PERIOD) or GLOB_ALTDIRFUNC) or GLOB_BRACE) or GLOB_NOMAGIC) or GLOB_TILDE) or GLOB_ONLYDIR) or GLOB_TILDE_CHECK;
  GLOB_NOSPACE = 1;
  GLOB_ABORTED = 2;
  GLOB_NOMATCH = 3;
  GLOB_NOSYS = 4;

  GLOB_ABEND = GLOB_ABORTED;

Type
  TGlobCloseDirProc = procedure(Param: Pointer); cdecl;
  TGlobReadDirFunc = function(Param: Pointer): PDirEnt; cdecl;
  TGlobOpenDirFunc = function(Param: PChar): __ptr_t; cdecl;
  TGlobStatFunc = function(Param1: PChar; Param2: PStatBuf): longint; cdecl;

   Pglob_t = ^glob_t;
   glob_t = record
     gl_pathc : cardinal;
     gl_pathv : ^Pchar;
     gl_offs : cardinal;
     gl_flags : longint;
     gl_closedir : TGlobClosedirProc;
     gl_readdir : TGlobReaddirFunc;
     gl_opendir : TGlobOpendirFunc;
     gl_lstat : TGlobStatFunc;
     gl_stat : TGlobStatFunc;
   end;

 TGlobErrFunc = function(PathName: PChar; ErrNo: longint): longint; cdecl;

 function glob(__pattern:Pchar; __flags:longint; __errfunc:TGlobErrFunc; __pglob:Pglob_t):longint;cdecl;external clib name 'glob';
 procedure globfree(__pglob:Pglob_t);cdecl;external clib name 'globfree';

implementation

function lstat(__file:Pchar; var __buf:_stat):longint;
begin
  __lxstat(_STAT_VER,__file,__buf);
end;

function lstat64(__file:Pchar; var __buf:_stat64):longint;
begin
  __lxstat64(_STAT_VER,__file,__buf);
end;

function stat(__file:Pchar; var __buf:_stat):longint;
begin
  __xstat(_STAT_VER,__file,__buf);
end;

function stat64(__file:Pchar; var __buf: _stat64):longint;
begin
  __xstat64(_STAT_VER,__file,__buf);
end;


end.

