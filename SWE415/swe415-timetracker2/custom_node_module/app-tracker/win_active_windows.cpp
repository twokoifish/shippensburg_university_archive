#include <windows.h>
#include <stringapiset.h>
#include <napi.h>
#include <codecvt>
#include <string>
#include <locale>
#include <tchar.h>
#include <psapi.h>
#include <iostream>
#include <string>
#include <vector>
#include <windows.h>
#include <stringapiset.h>
#include <napi.h>
#include <codecvt>
#include <string>
#include <locale>
#include <tchar.h>
#include <psapi.h>
#include <iostream>

using namespace std;
using namespace Napi;

typedef struct
{
  vector<HWND> window_handles;
  DWORD window_count;
} window_manager_t;

BOOL CALLBACK WindowChecker(HWND hwnd, LPARAM lParam)
{
  window_manager_t *manager = (window_manager_t *)lParam;
  wchar_t window_text[1024];
  DWORD size = sizeof(window_text) / sizeof(wchar_t);
  GetWindowTextW(hwnd, window_text, size);
  wstring wstr(window_text);
  string str(wstr.begin(), wstr.end());
  if (IsWindowVisible(hwnd) && str.length() != 0 && str.compare("Chrome Legacy Window") != 0)
  {
    manager->window_handles.push_back(hwnd);
    manager->window_count++;
  }
  return TRUE;
}

window_manager_t GetAllParentWindows()
{
  window_manager_t manager;
  manager.window_count = 0;
  EnumWindows(WindowChecker, (LPARAM)&manager);
  return manager;
}

window_manager_t GetAllChildWindows(window_manager_t parent_set)
{
  for (DWORD i = 0; i < parent_set.window_count; i++)
  {
    EnumChildWindows(parent_set.window_handles[i], WindowChecker, (LPARAM)&parent_set);
  }
  return parent_set;
}

string GetProcessName(HWND window_handle)
{
  DWORD pid = 0;
  GetWindowThreadProcessId(window_handle, &pid);
  wchar_t process_name[1024];
  HANDLE process_handle = OpenProcess(PROCESS_QUERY_INFORMATION | PROCESS_VM_READ, FALSE, pid);
  if (process_handle != NULL)
  {
    HMODULE module_handle;
    DWORD module_size_bytes;
    if (K32EnumProcessModules(process_handle, &module_handle, sizeof(module_handle), &module_size_bytes))
    {
      DWORD word_size = sizeof(process_name) / sizeof(wchar_t);
      K32GetModuleBaseNameW(process_handle, module_handle, process_name, word_size);
    }
  }
  wstring wstr(process_name);
  string str(wstr.begin(), wstr.end());
  return str;
}

window_manager_t RemoveSystemWindows(window_manager_t set)
{
  window_manager_t manager;
  manager.window_count = 0;
  for (DWORD i = 0; i < set.window_count; i++)
  {
    string process_name = GetProcessName(set.window_handles[i]);

    const string internal_user_processes[] = {
        "RuntimeBroker.exe",
        "FileCoAuth.exe",
        "identity_helper.exe",
        "ApplicationFrameHost.exe",
        "WinStore.App.exe",
        "YourPhone.exe",
        "winlogo.exe",
        "svchost.exe",
        "UserOOBEBroker.exe",
        "SystemSettingsBroker.exe",
        "taskhostw.exe",
        "StartMenuExperienceHost.exe",
        "dllhost.exe",
        "DllHost.exe",
        "ctfmon.exe",
        "csrss.exe",
        "DAX3API.exe",
        "Microsoft.Photos.exe",
        "LockApp.exe",
        "fontdrvhost.exe",
        "dwm.exe",
        "HxTsr.exe",
        "HxOutlook.exe",
        "HxCalendarAppImm.exe",
        "conhost.exe",
        "SettingSyncHost.exe",
        "SecurityHealthSystray.exe",
        "SearchApp.exe",
        "smartscreen.exe",
        "sihost.exe",
        "ShellExperienceHost.exe",
        "schtasks.exe",
        "YourPhoneServer.exe",
        "Explorer.EXE",
        "TextInputHost.exe",
        "SystemSettings.exe",
        "OpenConsole.exe",
        "VCTIP.EXE",
        ""};

    DWORD internal_size = sizeof(internal_user_processes) / sizeof(string);

    int isInternal = 0;
    for (DWORD j = 0; j < internal_size; j++)
    {
      if (process_name.compare(internal_user_processes[j]) == 0)
      {
        isInternal = 1;
        break;
      }
    }

    if (isInternal == 0)
    {
      manager.window_handles.push_back(set.window_handles[i]);
      manager.window_count++;
    }
  }
  return manager;
}

string GetWindowTitle(HWND window_handle)
{
  wchar_t window_text[1024];
  DWORD size = sizeof(window_text) / sizeof(wchar_t);
  GetWindowTextW(window_handle, window_text, size);
  wstring wstr(window_text);
  string str(wstr.begin(), wstr.end());
  return str;
}

DWORD GetProcessID(HWND window_handle) {
  DWORD pid = 0;
  GetWindowThreadProcessId(window_handle, &pid);
  return pid;
}

Object CreateDesktopWindow(Napi::Env env, HWND window_handle, DWORD foreground_pid)
{
  Object desktop_window = Object::New(env);
  desktop_window.Set("name", GetProcessName(window_handle));                              //windowClass
  desktop_window.Set("titleBar", GetWindowTitle(window_handle)); //windowName
  DWORD pid = GetProcessID(window_handle);
  desktop_window.Set("pid", pid);  
  desktop_window.Set("id", (int) window_handle);                                // id
  desktop_window.Set("isFocus", (pid == foreground_pid));


  desktop_window.Set("preventsScreenSleep", true); //?
  desktop_window.Set("preventsSystemSleep", true); //?
  return desktop_window;
}

Promise FetchDesktopWindows(const Napi::CallbackInfo &info)
{
  Env env = info.Env();
  Promise::Deferred deferred = Promise::Deferred::New(env);
  window_manager_t manager = GetAllParentWindows();
  manager = GetAllChildWindows(manager);
  manager = RemoveSystemWindows(manager);
  HWND foreground_window_handle = GetForegroundWindow();
  DWORD foreground_pid;
  GetWindowThreadProcessId(foreground_window_handle, &foreground_pid);
  Array desktop_windows = Array::New(env, manager.window_count);
  for (DWORD i = 0; i < manager.window_count; i++)
  {
    desktop_windows[i] = CreateDesktopWindow(env, manager.window_handles[i], foreground_pid);
  }
  deferred.Resolve(desktop_windows);
  return deferred.Promise();
}

Object Init(Napi::Env env, Napi::Object exports)
{
  exports.Set(String::New(env, "fetchDesktopWindows"), Function::New(env, FetchDesktopWindows));
  return exports;
}

NODE_API_MODULE(app_tracker, Init)