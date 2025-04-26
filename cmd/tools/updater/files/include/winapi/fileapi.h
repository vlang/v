/**
 * This file is part of the mingw-w64 runtime package.
 * No warranty is given; refer to the file DISCLAIMER within this package.
 */
#ifndef _APISETFILE_
#define _APISETFILE_

//#include <apiset.h>
//#include <apisetcconv.h>
//#include <minwindef.h>
//#include <minwinbase.h>
//
//#ifdef __cplusplus
//extern "C" {
//#endif
//
//#if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_APP)
//#define CREATE_NEW 1
//#define CREATE_ALWAYS 2
//#define OPEN_EXISTING 3
//#define OPEN_ALWAYS 4
//#define TRUNCATE_EXISTING 5
//
//#define INVALID_FILE_SIZE ((DWORD)0xffffffff)
//#define INVALID_SET_FILE_POINTER ((DWORD)-1)
//#define INVALID_FILE_ATTRIBUTES ((DWORD)-1)
//#endif
//
//#if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP) || _WIN32_WINNT >= _WIN32_WINNT_WIN10
//WINBASEAPI HANDLE WINAPI CreateFileW (LPCWSTR lpFileName, DWORD dwDesiredAccess, DWORD dwShareMode, LPSECURITY_ATTRIBUTES lpSecurityAttributes, DWORD dwCreationDisposition, DWORD dwFlagsAndAttributes, HANDLE hTemplateFile);
//#define CreateFile __MINGW_NAME_AW(CreateFile)
//WINBASEAPI DWORD WINAPI GetFileAttributesW (LPCWSTR lpFileName);
//#define GetFileAttributes __MINGW_NAME_AW(GetFileAttributes)
//WINBASEAPI DWORD WINAPI GetFileSize (HANDLE hFile, LPDWORD lpFileSizeHigh);
//WINBASEAPI DWORD WINAPI SetFilePointer (HANDLE hFile, LONG lDistanceToMove, PLONG lpDistanceToMoveHigh, DWORD dwMoveMethod);
//#endif
//#if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)
//  typedef struct _BY_HANDLE_FILE_INFORMATION {
//    DWORD dwFileAttributes;
//    FILETIME ftCreationTime;
//    FILETIME ftLastAccessTime;
//    FILETIME ftLastWriteTime;
//    DWORD dwVolumeSerialNumber;
//    DWORD nFileSizeHigh;
//    DWORD nFileSizeLow;
//    DWORD nNumberOfLinks;
//    DWORD nFileIndexHigh;
//    DWORD nFileIndexLow;
//  } BY_HANDLE_FILE_INFORMATION, *PBY_HANDLE_FILE_INFORMATION,
//    *LPBY_HANDLE_FILE_INFORMATION;
//
//  WINBASEAPI HANDLE WINAPI CreateFileA (LPCSTR lpFileName, DWORD dwDesiredAccess, DWORD dwShareMode, LPSECURITY_ATTRIBUTES lpSecurityAttributes, DWORD dwCreationDisposition, DWORD dwFlagsAndAttributes, HANDLE hTemplateFile);
//  WINBASEAPI WINBOOL WINAPI DefineDosDeviceW (DWORD dwFlags, LPCWSTR lpDeviceName, LPCWSTR lpTargetPath);
//  WINBASEAPI WINBOOL WINAPI FindCloseChangeNotification (HANDLE hChangeHandle);
//  WINBASEAPI HANDLE WINAPI FindFirstChangeNotificationA (LPCSTR lpPathName, WINBOOL bWatchSubtree, DWORD dwNotifyFilter);
//  WINBASEAPI HANDLE WINAPI FindFirstChangeNotificationW (LPCWSTR lpPathName, WINBOOL bWatchSubtree, DWORD dwNotifyFilter);
//  WINBASEAPI HANDLE WINAPI FindFirstVolumeW (LPWSTR lpszVolumeName, DWORD cchBufferLength);
//  WINBASEAPI WINBOOL WINAPI FindNextChangeNotification (HANDLE hChangeHandle);
//  WINBASEAPI WINBOOL WINAPI FindNextVolumeW (HANDLE hFindVolume, LPWSTR lpszVolumeName, DWORD cchBufferLength);
//  WINBASEAPI WINBOOL WINAPI FindVolumeClose (HANDLE hFindVolume);
//  WINBASEAPI WINBOOL WINAPI GetFileInformationByHandle (HANDLE hFile, LPBY_HANDLE_FILE_INFORMATION lpFileInformation);
//#endif
//#if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP) || _WIN32_WINNT >= _WIN32_WINNT_WIN10
//  WINBASEAPI LONG WINAPI CompareFileTime (CONST FILETIME *lpFileTime1, CONST FILETIME *lpFileTime2);
//  WINBASEAPI WINBOOL WINAPI DeleteVolumeMountPointW (LPCWSTR lpszVolumeMountPoint);
//  WINBASEAPI WINBOOL WINAPI FileTimeToLocalFileTime (CONST FILETIME *lpFileTime, LPFILETIME lpLocalFileTime);
//  WINBASEAPI HANDLE WINAPI FindFirstFileA (LPCSTR lpFileName, LPWIN32_FIND_DATAA lpFindFileData);
//  WINBASEAPI HANDLE WINAPI FindFirstFileW (LPCWSTR lpFileName, LPWIN32_FIND_DATAW lpFindFileData);
//  WINBASEAPI WINBOOL WINAPI GetDiskFreeSpaceA (LPCSTR lpRootPathName, LPDWORD lpSectorsPerCluster, LPDWORD lpBytesPerSector, LPDWORD lpNumberOfFreeClusters, LPDWORD lpTotalNumberOfClusters);
//  WINBASEAPI WINBOOL WINAPI GetDiskFreeSpaceW (LPCWSTR lpRootPathName, LPDWORD lpSectorsPerCluster, LPDWORD lpBytesPerSector, LPDWORD lpNumberOfFreeClusters, LPDWORD lpTotalNumberOfClusters);
//  WINBASEAPI UINT WINAPI GetDriveTypeA (LPCSTR lpRootPathName);
//  WINBASEAPI UINT WINAPI GetDriveTypeW (LPCWSTR lpRootPathName);
//  WINBASEAPI DWORD WINAPI GetFileAttributesA (LPCSTR lpFileName);
//  WINBASEAPI WINBOOL WINAPI GetFileSizeEx (HANDLE hFile, PLARGE_INTEGER lpFileSize);
//  WINBASEAPI WINBOOL WINAPI GetFileTime (HANDLE hFile, LPFILETIME lpCreationTime, LPFILETIME lpLastAccessTime, LPFILETIME lpLastWriteTime);
//  WINBASEAPI DWORD WINAPI GetFileType (HANDLE hFile);
//  WINBASEAPI DWORD WINAPI GetFullPathNameA (LPCSTR lpFileName, DWORD nBufferLength, LPSTR lpBuffer, LPSTR *lpFilePart);
//  WINBASEAPI DWORD WINAPI GetFullPathNameW (LPCWSTR lpFileName, DWORD nBufferLength, LPWSTR lpBuffer, LPWSTR *lpFilePart);
//  WINBASEAPI DWORD WINAPI GetLogicalDrives (VOID);
//#endif
//#if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)
//  WINBASEAPI DWORD WINAPI GetLogicalDriveStringsW (DWORD nBufferLength, LPWSTR lpBuffer);
//  WINBASEAPI DWORD WINAPI GetLongPathNameA (LPCSTR lpszShortPath, LPSTR lpszLongPath, DWORD cchBuffer);
//  WINBASEAPI DWORD WINAPI GetLongPathNameW (LPCWSTR lpszShortPath, LPWSTR lpszLongPath, DWORD cchBuffer);
//  WINBASEAPI DWORD WINAPI GetShortPathNameW (LPCWSTR lpszLongPath, LPWSTR lpszShortPath, DWORD cchBuffer);
//  WINBASEAPI UINT WINAPI GetTempFileNameA (LPCSTR lpPathName, LPCSTR lpPrefixString, UINT uUnique, LPSTR lpTempFileName);
//  WINBASEAPI UINT WINAPI GetTempFileNameW (LPCWSTR lpPathName, LPCWSTR lpPrefixString, UINT uUnique, LPWSTR lpTempFileName);
//  WINBASEAPI WINBOOL WINAPI GetVolumeInformationW (LPCWSTR lpRootPathName, LPWSTR lpVolumeNameBuffer, DWORD nVolumeNameSize, LPDWORD lpVolumeSerialNumber, LPDWORD lpMaximumComponentLength, LPDWORD lpFileSystemFlags, LPWSTR lpFileSystemNameBuffer, DWORD nFileSystemNameSize);
//  WINBASEAPI WINBOOL WINAPI GetVolumePathNameW (LPCWSTR lpszFileName, LPWSTR lpszVolumePathName, DWORD cchBufferLength);
//  WINBASEAPI WINBOOL WINAPI LocalFileTimeToFileTime (CONST FILETIME *lpLocalFileTime, LPFILETIME lpFileTime);
//  WINBASEAPI WINBOOL WINAPI LockFile (HANDLE hFile, DWORD dwFileOffsetLow, DWORD dwFileOffsetHigh, DWORD nNumberOfBytesToLockLow, DWORD nNumberOfBytesToLockHigh);
//  WINBASEAPI DWORD WINAPI QueryDosDeviceW (LPCWSTR lpDeviceName, LPWSTR lpTargetPath, DWORD ucchMax);
//  WINBASEAPI WINBOOL WINAPI ReadFileEx (HANDLE hFile, LPVOID lpBuffer, DWORD nNumberOfBytesToRead, LPOVERLAPPED lpOverlapped, LPOVERLAPPED_COMPLETION_ROUTINE lpCompletionRoutine);
//  WINBASEAPI WINBOOL WINAPI ReadFileScatter (HANDLE hFile, FILE_SEGMENT_ELEMENT aSegmentArray[], DWORD nNumberOfBytesToRead, LPDWORD lpReserved, LPOVERLAPPED lpOverlapped);
//  WINBASEAPI WINBOOL WINAPI SetFileTime (HANDLE hFile, CONST FILETIME *lpCreationTime, CONST FILETIME *lpLastAccessTime, CONST FILETIME *lpLastWriteTime);
//  WINBASEAPI WINBOOL WINAPI SetFileValidData (HANDLE hFile, LONGLONG ValidDataLength);
//  WINBASEAPI WINBOOL WINAPI UnlockFile (HANDLE hFile, DWORD dwFileOffsetLow, DWORD dwFileOffsetHigh, DWORD nNumberOfBytesToUnlockLow, DWORD nNumberOfBytesToUnlockHigh);
//  WINBASEAPI WINBOOL WINAPI WriteFileEx (HANDLE hFile, LPCVOID lpBuffer, DWORD nNumberOfBytesToWrite, LPOVERLAPPED lpOverlapped, LPOVERLAPPED_COMPLETION_ROUTINE lpCompletionRoutine);
//  WINBASEAPI WINBOOL WINAPI WriteFileGather (HANDLE hFile, FILE_SEGMENT_ELEMENT aSegmentArray[], DWORD nNumberOfBytesToWrite, LPDWORD lpReserved, LPOVERLAPPED lpOverlapped);
//  WINBASEAPI WINBOOL WINAPI GetVolumeNameForVolumeMountPointW (LPCWSTR lpszVolumeMountPoint, LPWSTR lpszVolumeName, DWORD cchBufferLength);
//  WINBASEAPI WINBOOL WINAPI GetVolumePathNamesForVolumeNameW (LPCWSTR lpszVolumeName, LPWCH lpszVolumePathNames, DWORD cchBufferLength, PDWORD lpcchReturnLength);
//  WINBASEAPI DWORD WINAPI GetTempPathA (DWORD nBufferLength, LPSTR lpBuffer);
//  WINBASEAPI DWORD WINAPI GetTempPathW (DWORD nBufferLength, LPWSTR lpBuffer);
//
//#ifdef UNICODE
//#define DefineDosDevice DefineDosDeviceW
//#define DeleteVolumeMountPoint DeleteVolumeMountPointW
//#define FindFirstVolume FindFirstVolumeW
//#define FindNextVolume FindNextVolumeW
//#define GetLogicalDriveStrings GetLogicalDriveStringsW
//#define GetShortPathName GetShortPathNameW
//#define GetVolumeInformation GetVolumeInformationW
//#define GetVolumePathName GetVolumePathNameW
//#define QueryDosDevice QueryDosDeviceW
//#define GetVolumeNameForVolumeMountPoint GetVolumeNameForVolumeMountPointW
//#define GetVolumePathNamesForVolumeName GetVolumePathNamesForVolumeNameW
//#endif
//#define FindFirstChangeNotification __MINGW_NAME_AW(FindFirstChangeNotification)
//#define FindFirstFile __MINGW_NAME_AW(FindFirstFile)
//#define GetDiskFreeSpace __MINGW_NAME_AW(GetDiskFreeSpace)
//#define GetDriveType __MINGW_NAME_AW(GetDriveType)
//#define GetFullPathName __MINGW_NAME_AW(GetFullPathName)
//#define GetLongPathName __MINGW_NAME_AW(GetLongPathName)
//#define GetTempPath __MINGW_NAME_AW(GetTempPath)
//#define GetTempFileName __MINGW_NAME_AW(GetTempFileName)
//
//
#if _WIN32_WINNT >= 0x0600
  WINBASEAPI DWORD WINAPI GetFinalPathNameByHandleA (HANDLE hFile, LPSTR lpszFilePath, DWORD cchFilePath, DWORD dwFlags);
  WINBASEAPI DWORD WINAPI GetFinalPathNameByHandleW (HANDLE hFile, LPWSTR lpszFilePath, DWORD cchFilePath, DWORD dwFlags);
  WINBASEAPI WINBOOL WINAPI GetVolumeInformationByHandleW (HANDLE hFile, LPWSTR lpVolumeNameBuffer, DWORD nVolumeNameSize, LPDWORD lpVolumeSerialNumber, LPDWORD lpMaximumComponentLength, LPDWORD lpFileSystemFlags, LPWSTR lpFileSystemNameBuffer, DWORD nFileSystemNameSize);

#define GetFinalPathNameByHandle __MINGW_NAME_AW(GetFinalPathNameByHandle)
#endif
//#endif
//
//#if WINAPI_FAMILY_PARTITION (WINAPI_FAMILY_DESKTOP_APP)
//  typedef struct _WIN32_FILE_ATTRIBUTE_DATA {
//    DWORD dwFileAttributes;
//    FILETIME ftCreationTime;
//    FILETIME ftLastAccessTime;
//    FILETIME ftLastWriteTime;
//    DWORD nFileSizeHigh;
//    DWORD nFileSizeLow;
//  } WIN32_FILE_ATTRIBUTE_DATA, *LPWIN32_FILE_ATTRIBUTE_DATA;
//
//#if _WIN32_WINNT >= 0x0602
//  typedef struct _CREATEFILE2_EXTENDED_PARAMETERS {
//    DWORD dwSize;
//    DWORD dwFileAttributes;
//    DWORD dwFileFlags;
//    DWORD dwSecurityQosFlags;
//    LPSECURITY_ATTRIBUTES lpSecurityAttributes;
//    HANDLE hTemplateFile;
//  } CREATEFILE2_EXTENDED_PARAMETERS, *PCREATEFILE2_EXTENDED_PARAMETERS,
//    *LPCREATEFILE2_EXTENDED_PARAMETERS;
//#endif
//
//  WINBASEAPI WINBOOL WINAPI CreateDirectoryA (LPCSTR lpPathName, LPSECURITY_ATTRIBUTES lpSecurityAttributes);
//  WINBASEAPI WINBOOL WINAPI CreateDirectoryW (LPCWSTR lpPathName, LPSECURITY_ATTRIBUTES lpSecurityAttributes);
//  WINBASEAPI WINBOOL WINAPI DeleteFileA (LPCSTR lpFileName);
//  WINBASEAPI WINBOOL WINAPI DeleteFileW (LPCWSTR lpFileName);
//  WINBASEAPI WINBOOL WINAPI FindClose (HANDLE hFindFile);
//  WINBASEAPI HANDLE WINAPI FindFirstFileExA (LPCSTR lpFileName, FINDEX_INFO_LEVELS fInfoLevelId, LPVOID lpFindFileData, FINDEX_SEARCH_OPS fSearchOp, LPVOID lpSearchFilter, DWORD dwAdditionalFlags);
//  WINBASEAPI HANDLE WINAPI FindFirstFileExW (LPCWSTR lpFileName, FINDEX_INFO_LEVELS fInfoLevelId, LPVOID lpFindFileData, FINDEX_SEARCH_OPS fSearchOp, LPVOID lpSearchFilter, DWORD dwAdditionalFlags);
//  WINBASEAPI WINBOOL WINAPI FindNextFileA (HANDLE hFindFile, LPWIN32_FIND_DATAA lpFindFileData);
//  WINBASEAPI WINBOOL WINAPI FindNextFileW (HANDLE hFindFile, LPWIN32_FIND_DATAW lpFindFileData);
//  WINBASEAPI WINBOOL WINAPI FlushFileBuffers (HANDLE hFile);
//  WINBASEAPI WINBOOL WINAPI GetDiskFreeSpaceExA (LPCSTR lpDirectoryName, PULARGE_INTEGER lpFreeBytesAvailableToCaller, PULARGE_INTEGER lpTotalNumberOfBytes, PULARGE_INTEGER lpTotalNumberOfFreeBytes);
//  WINBASEAPI WINBOOL WINAPI GetDiskFreeSpaceExW (LPCWSTR lpDirectoryName, PULARGE_INTEGER lpFreeBytesAvailableToCaller, PULARGE_INTEGER lpTotalNumberOfBytes, PULARGE_INTEGER lpTotalNumberOfFreeBytes);
//  WINBASEAPI DWORD WINAPI GetFileType (HANDLE hFile);
//  WINBASEAPI WINBOOL WINAPI GetFileAttributesExA (LPCSTR lpFileName, GET_FILEEX_INFO_LEVELS fInfoLevelId, LPVOID lpFileInformation);
//  WINBASEAPI WINBOOL WINAPI GetFileAttributesExW (LPCWSTR lpFileName, GET_FILEEX_INFO_LEVELS fInfoLevelId, LPVOID lpFileInformation);
//  WINBASEAPI WINBOOL WINAPI LockFileEx (HANDLE hFile, DWORD dwFlags, DWORD dwReserved, DWORD nNumberOfBytesToLockLow, DWORD nNumberOfBytesToLockHigh, LPOVERLAPPED lpOverlapped);
//  WINBASEAPI WINBOOL WINAPI ReadFile (HANDLE hFile, LPVOID lpBuffer, DWORD nNumberOfBytesToRead, LPDWORD lpNumberOfBytesRead, LPOVERLAPPED lpOverlapped);
//  WINBASEAPI WINBOOL WINAPI RemoveDirectoryA (LPCSTR lpPathName);
//  WINBASEAPI WINBOOL WINAPI RemoveDirectoryW (LPCWSTR lpPathName);
//  WINBASEAPI WINBOOL WINAPI SetEndOfFile (HANDLE hFile);
//  WINBASEAPI WINBOOL WINAPI SetFileAttributesA (LPCSTR lpFileName, DWORD dwFileAttributes);
//  WINBASEAPI WINBOOL WINAPI SetFileAttributesW (LPCWSTR lpFileName, DWORD dwFileAttributes);
//  WINBASEAPI WINBOOL WINAPI SetFilePointerEx (HANDLE hFile, LARGE_INTEGER liDistanceToMove, PLARGE_INTEGER lpNewFilePointer, DWORD dwMoveMethod);
//  WINBASEAPI WINBOOL WINAPI UnlockFileEx (HANDLE hFile, DWORD dwReserved, DWORD nNumberOfBytesToUnlockLow, DWORD nNumberOfBytesToUnlockHigh, LPOVERLAPPED lpOverlapped);
//  WINBASEAPI WINBOOL WINAPI WriteFile (HANDLE hFile, LPCVOID lpBuffer, DWORD nNumberOfBytesToWrite, LPDWORD lpNumberOfBytesWritten, LPOVERLAPPED lpOverlapped);
//
//#define CreateDirectory __MINGW_NAME_AW(CreateDirectory)
//#define DeleteFile __MINGW_NAME_AW(DeleteFile)
//#define FindFirstFileEx __MINGW_NAME_AW(FindFirstFileEx)
//#define FindNextFile __MINGW_NAME_AW(FindNextFile)
//#define GetDiskFreeSpaceEx __MINGW_NAME_AW(GetDiskFreeSpaceEx)
//#define GetFileAttributesEx __MINGW_NAME_AW(GetFileAttributesEx)
//#define RemoveDirectory __MINGW_NAME_AW(RemoveDirectory)
//#define SetFileAttributes __MINGW_NAME_AW(SetFileAttributes)
//
//#if _WIN32_WINNT >= 0x0600
//  WINBASEAPI WINBOOL WINAPI SetFileInformationByHandle (HANDLE hFile, FILE_INFO_BY_HANDLE_CLASS FileInformationClass, LPVOID lpFileInformation, DWORD dwBufferSize);
//#endif
//#if _WIN32_WINNT >= 0x0602
//  WINBASEAPI HANDLE WINAPI CreateFile2 (LPCWSTR lpFileName, DWORD dwDesiredAccess, DWORD dwShareMode, DWORD dwCreationDisposition, LPCREATEFILE2_EXTENDED_PARAMETERS pCreateExParams);
//#endif
//#endif
//
//#ifdef __cplusplus
//}
//#endif
#endif
